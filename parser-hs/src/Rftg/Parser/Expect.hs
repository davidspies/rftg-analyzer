{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Expect
  ( parseExpectations
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Rftg.Bga.Json
  ( Object
  , arrayField
  , field
  , intValue
  , keyText
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  )
import Rftg.Bga.State
  ( BgaPhase (..)
  , BgaState
  , bgaStateHasPhase
  , bgaStateIsExploreStart
  , bgaStateIsFinalGameOver
  , bgaStateIsNewActionRound
  , bgaStateIsSearch
  , bgaStateIsSearchStart
  , optionalBgaStateField
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  , Seat (..)
  )
import Rftg.Keldon.Script
  ( ChoiceOrder (..)
  , ExpectLine (..)
  , KeldonChoice (..)
  , KeldonScript (..)
  , OrderedScriptLine (..)
  , ScriptLine (..)
  , appendScript
  , choiceScriptAt
  , emptyScript
  )
import Rftg.Parser.Action (parseActions)
import Rftg.Parser.CardIndex
  ( CardIndex (..)
  , discardCardIds
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( cardId
  , cardName
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  , ReviewSelection
  , selectReviewPlayer
  )
import Rftg.Parser.Setup (gamedataFor)

data CurrentPhase
  = PhaseAction
  | PhaseReveal
  | PhaseExplore
  | PhaseDevelop
  | PhaseSettle
  | PhaseConsume
  | PhaseProduce
  | PhaseDiscard
  | PhaseGameOver
  | PhaseOther
  deriving stock (Eq, Show)

data PendingDraw = PendingDraw
  { pendingDrawPlayer :: PlayerId
  , pendingDrawCards :: [Value]
  }
  deriving stock (Eq, Show)

data PlayerExpectState = PlayerExpectState
  { exactHand :: Bool
  , handCards :: Map Int Text
  , handCount :: Maybe Int
  , handFresh :: Bool
  , handWaitTableau :: Maybe Int
  , tableauCount :: Maybe Int
  , goodsCount :: Int
  , goodsWorlds :: [Text]
  , prestigeCount :: Maybe Int
  , prestigeSnapshot :: Maybe Int
  , vpCount :: Int
  }
  deriving stock (Eq, Show)

data ExpectState = ExpectState
  { cardIndex :: CardIndex
  , playersById :: Map PlayerId Player
  , playerStates :: Map PlayerId PlayerExpectState
  , tableauCards :: Map PlayerId [Text]
  , goodWorlds :: Map Int Int
  , activeGoods :: Set Int
  , currentPhase :: CurrentPhase
  , actionsCommitted :: Bool
  , nextRoundIndex :: Int
  , actionOrders :: Map Int [(PlayerId, ChoiceOrder)]
  , pendingDraws :: [PendingDraw]
  , pendingExplored :: [[Value]]
  , exploredIds :: Map PlayerId [Int]
  , activeSearch :: Maybe PlayerId
  , pendingSearchKept :: Map PlayerId Value
  , expectScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseExpectations :: ReviewSelection -> Value -> Either Text KeldonScript
parseExpectations reviewSelection rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  actions <- parseActions rootValue
  actionOrderMap <- actionOrdersFromScript players actions
  reviewPid <- playerId <$> selectReviewPlayer reviewSelection players
  initialStates <- traverse (initialPlayerState reviewPid gamedatas cardTypes) players
  notifications <- notificationObjects root
  seededGoods <- initialGoodWorlds notifications
  walked <-
    foldM
      (expectStep cardTypes)
      (ExpectState
        { cardIndex = startingCardIndex
        , playersById = Map.fromList [(playerId player, player) | player <- players]
        , playerStates = Map.fromList [(playerId player, state) | (player, state) <- zip players initialStates]
        , tableauCards = Map.fromList [(playerId player, []) | player <- players]
        , goodWorlds = seededGoods
        , activeGoods = Set.empty
        , currentPhase = PhaseOther
        , actionsCommitted = True
        , nextRoundIndex = 0
        , actionOrders = actionOrderMap
        , pendingDraws = []
        , pendingExplored = []
        , exploredIds = Map.empty
        , activeSearch = Nothing
        , pendingSearchKept = Map.empty
        , expectScript = emptyScript
        })
      notifications
  finished <- commitActions walked
  finalState <- emitFinalExpectations finished
  pure (expectScript finalState)
initialPlayerState :: PlayerId -> Object -> Map Int Text -> Player -> Either Text PlayerExpectState
initialPlayerState reviewPid gamedatas cardTypes player = do
  gamedata <- gamedataFor gamedatas (playerId player)
  handValues <- objectValues <$> objectField "hand" gamedata
  handEntries <- traverse handEntry handValues
  pure PlayerExpectState
    { exactHand = playerId player == reviewPid
    , handCards = Map.fromList handEntries
    , handCount = Nothing
    , handFresh = False
    , handWaitTableau = Nothing
    , tableauCount = Nothing
    , goodsCount = 0
    , goodsWorlds = []
    , prestigeCount = Nothing
    , prestigeSnapshot = Nothing
    , vpCount = 0
    }
  where
    handEntry cardValue = do
      cid <- cardId cardValue
      name <- cardName cardTypes cardValue
      pure (cid, name)

initialGoodWorlds :: [Object] -> Either Text (Map Int Int)
initialGoodWorlds notifications =
  foldM addGood Map.empty notifications
  where
    addGood goods notification
      | notificationType notification /= "goodproduction" = pure goods
      | otherwise = do
          args <- objectField "args" notification
          goodId <- intValue "goodproduction good_id" =<< field "good_id" args
          worldId <- intValue "goodproduction world_id" =<< field "world_id" args
          pure (Map.insertWith (\_ old -> old) goodId worldId goods)

actionOrdersFromScript :: [Player] -> KeldonScript -> Either Text (Map Int [(PlayerId, ChoiceOrder)])
actionOrdersFromScript players script =
  foldM addSeat Map.empty (Map.toList (scriptChoices script))
  where
    pidBySeat =
      Map.fromList [(playerSeat player, playerId player) | player <- players]

    addSeat byRound (seat, lines_) = do
      pid <-
        case Map.lookup seat pidBySeat of
          Just pid' -> pure pid'
          Nothing -> Left ("ACTION script has unknown seat " <> showText (unSeat seat))
      foldM (addLine pid) byRound lines_

    addLine pid byRound ordered =
      case orderedLine ordered of
        Choice _ _ (ChooseAction _) ->
          case unChoiceOrder (orderedLineOrder ordered) of
            roundIndex : _ ->
              pure (Map.alter (Just . maybe [(pid, orderedLineOrder ordered)] (<> [(pid, orderedLineOrder ordered)])) roundIndex byRound)
            [] -> Left "ACTION choice has empty order"
        _ -> pure byRound

expectStep :: Map Int Text -> ExpectState -> Object -> Either Text ExpectState
expectStep cardTypes state notification = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "showTableau" -> handleShowTableau cardTypes stateWithCards notification
    "drawCards" -> handleDrawCards stateWithCards notification
    "explored_choice" -> handleExploredChoice cardTypes stateWithCards notification
    "keepcards" -> handleKeepCards stateWithCards notification
    "discard" -> handleDiscard stateWithCards notification
    "playcard" -> handlePlayCard cardTypes stateWithCards notification
    "discardfromtableau" -> handleDiscardFromTableau stateWithCards notification
    "goodproduction" -> handleGoodProduction stateWithCards notification
    "consume" -> handleConsume stateWithCards notification
    "updatePrestige" -> handleUpdatePrestige stateWithCards notification
    "updateCardCount" -> handleUpdateCardCount stateWithCards notification
    "updateScore" -> handleUpdateScore stateWithCards notification
    "drawCards_def" -> handleDrawCardsDef stateWithCards notification
    "explored_choice_log" -> handleExploredChoiceLog stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: ExpectState -> Object -> Either Text ExpectState
handleGameState state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      stateBeforeEnter <-
        if bgaStateIsSearch bgaState
          then pure state
          else finishPendingSearch state
      if bgaStateIsSearchStart bgaState
        then do
          pid <- PlayerId <$> (intValue "Search active_player" =<< field "active_player" args)
          enterState bgaState stateBeforeEnter { activeSearch = Just pid }
        else enterState bgaState stateBeforeEnter

enterState :: BgaState -> ExpectState -> Either Text ExpectState
enterState bgaState state
  | bgaStateIsNewActionRound bgaState =
      pure (snapshotPrestige state)
        { currentPhase = PhaseAction
        , actionsCommitted = False
        }
  | bgaStateIsExploreStart bgaState =
      flushPendingExplores =<< setPhase PhaseExplore =<< commitActions state
  | bgaStateIsFinalGameOver bgaState =
      pure state { currentPhase = PhaseGameOver }
  | bgaStateHasPhase BgaAction bgaState =
      setPhase PhaseReveal =<< commitActions state
  | bgaStateHasPhase BgaExplore bgaState =
      setPhase PhaseExplore =<< commitActions state
  | bgaStateHasPhase BgaDevelop bgaState =
      setPhase PhaseDevelop =<< commitActions state
  | bgaStateHasPhase BgaSettle bgaState =
      setPhase PhaseSettle =<< commitActions state
  | bgaStateHasPhase BgaConsume bgaState =
      setPhase PhaseConsume =<< commitActions state
  | bgaStateHasPhase BgaProduce bgaState =
      setPhase PhaseProduce =<< commitActions state
  | bgaStateHasPhase BgaDiscard bgaState =
      setPhase PhaseDiscard =<< commitActions state
  | bgaStateHasPhase BgaGameOver bgaState =
      setPhase PhaseGameOver =<< commitActions state
  | otherwise =
      pure state

setPhase :: CurrentPhase -> ExpectState -> Either Text ExpectState
setPhase phase state = pure state { currentPhase = phase }

snapshotPrestige :: ExpectState -> ExpectState
snapshotPrestige state =
  state { playerStates = fmap snapshotPlayer (playerStates state) }
  where
    snapshotPlayer playerState =
      playerState { prestigeSnapshot = prestigeCount playerState }

commitActions :: ExpectState -> Either Text ExpectState
commitActions state
  | actionsCommitted state = pure state
  | otherwise = do
      let roundIndex = nextRoundIndex state
      case Map.lookup roundIndex (actionOrders state) of
        Nothing ->
          pure state
            { actionsCommitted = True
            , nextRoundIndex = roundIndex + 1
            }
        Just actions -> do
          script <- foldM addActionExpectation emptyScript actions
          pure state
            { actionsCommitted = True
            , nextRoundIndex = roundIndex + 1
            , expectScript = expectScript state `appendScript` script
            }
  where
    addActionExpectation script (pid, order) = do
      player <- lookupPlayer (playersById state) pid
      playerState <- lookupPlayerState pid state
      pure (script `appendScript` expectationsAt order player playerState)

emitFinalExpectations :: ExpectState -> Either Text ExpectState
emitFinalExpectations state = do
  let finalOrder = ChoiceOrder [nextRoundIndex state, 9, 0]
      withLivePrestige = state { playerStates = fmap finalPrestige (playerStates state) }
  script <- foldM (addFinal finalOrder withLivePrestige) emptyScript (Map.elems (playersById state))
  pure withLivePrestige { expectScript = expectScript withLivePrestige `appendScript` script }
  where
    finalPrestige playerState =
      playerState { prestigeSnapshot = prestigeCount playerState }

    addFinal order state' script player = do
      playerState <- lookupPlayerState (playerId player) state'
      pure (script `appendScript` expectationsAt order player playerState)

expectationsAt :: ChoiceOrder -> Player -> PlayerExpectState -> KeldonScript
expectationsAt order player playerState =
  choiceScriptAt order (playerSeat player) (fmap (Expect (playerSeat player)) (expectationLines playerState))

expectationLines :: PlayerExpectState -> [ExpectLine]
expectationLines playerState =
  [ExpectGoods (goodsCount playerState)]
    <> maybe [] (pure . ExpectPrestige) (prestigeSnapshot playerState)
    <> [ExpectGoodsDist (Text.intercalate "|" (List.sort (goodsWorlds playerState)))]
    <> handExpectation
    <> maybe [] (pure . ExpectTableau) (tableauCount playerState)
    <> [ExpectVp (vpCount playerState)]
  where
    handExpectation
      | exactHand playerState = [ExpectHand (Map.size (handCards playerState))]
      | handFresh playerState = maybe [] (pure . ExpectHand) (handCount playerState)
      | otherwise = []

handleShowTableau :: Map Int Text -> ExpectState -> Object -> Either Text ExpectState
handleShowTableau cardTypes state notification = do
  args <- objectField "args" notification
  cardsObject <- expectObject "showTableau cards" =<< field "cards" args
  entries <- traverse tableauEntry (objectValues cardsObject)
  pure state { tableauCards = foldl addEntry (tableauCards state) entries }
  where
    tableauEntry cardValue = do
      pid <- cardPlayerId cardValue
      name <- cardName cardTypes cardValue
      pure (pid, name)
    addEntry table (pid, name) =
      Map.alter (Just . (<> [name]) . maybe [] id) pid table

handleDrawCards :: ExpectState -> Object -> Either Text ExpectState
handleDrawCards state notification = do
  cards <- notificationCardValues notification
  case cards of
    [] -> Left "drawCards notification has no cards"
    firstCard : _ -> do
      cardObject <- expectObject "drawCards card" firstCard
      case optionalField "location" cardObject of
        Just (String "hand") -> do
          pid <- cardPlayerId firstCard
          pure state
            { pendingDraws = pendingDraws state <> [PendingDraw pid cards]
            , playerStates = Map.adjust (\playerState -> playerState { handFresh = False }) pid (playerStates state)
            }
        Just (String "explored") -> pure state
        Just (String "aside") ->
          case activeSearch state of
            Nothing -> pure state
            Just pid ->
              case cards of
                [kept] -> pure state { pendingSearchKept = Map.insert pid kept (pendingSearchKept state) }
                _ -> Left "Search kept multiple aside cards"
        Just (String "retrofit") -> pure state
        Just (String other) -> Left ("unexpected drawCards location " <> other)
        Nothing -> pure state
        Just _ -> Left "drawCards location is not text"

handleDrawCardsDef :: ExpectState -> Object -> Either Text ExpectState
handleDrawCardsDef state notification = do
  args <- objectField "args" notification
  case (optionalField "player_name" args, optionalField "card_nbr" args) of
    (Just playerNameValue, Just cardCountValue) -> do
      playerName <- textValue "drawCards_def player_name" playerNameValue
      pid <- playerIdByName playerName state
      cardCount <- intValue "drawCards_def card_nbr" cardCountValue
      flushLoggedDraw pid cardCount state
    _ -> pure state

handleExploredChoiceLog :: ExpectState -> Object -> Either Text ExpectState
handleExploredChoiceLog state notification = do
  args <- objectField "args" notification
  case (optionalField "player_id" args, optionalField "nbr" args) of
    (Just pidValue, Just cardCountValue) -> do
      pid <- PlayerId <$> intValue "explored_choice_log player_id" pidValue
      cardCount <- intValue "explored_choice_log nbr" cardCountValue
      flushLoggedDraw pid cardCount state
    _ -> pure state

handleExploredChoice :: Map Int Text -> ExpectState -> Object -> Either Text ExpectState
handleExploredChoice _cardTypes state notification = do
  cards <- arrayField "args" notification
  if currentPhase state /= PhaseExplore
    then pure state { pendingExplored = pendingExplored state <> [cards] }
    else applyExploredChoice cards state

flushPendingExplores :: ExpectState -> Either Text ExpectState
flushPendingExplores state =
  foldM (flip applyExploredChoice) state (pendingExplored state)
    >>= \flushed -> pure flushed { pendingExplored = [] }

finishPendingSearch :: ExpectState -> Either Text ExpectState
finishPendingSearch state =
  case activeSearch state of
    Nothing -> pure state
    Just pid ->
      case Map.lookup pid (pendingSearchKept state) of
        Nothing -> pure state { activeSearch = Nothing }
        Just kept -> do
          withHand <- addCardsToHand (actionsCommitted state) pid [kept] state
          pure withHand
            { activeSearch = Nothing
            , pendingSearchKept = Map.delete pid (pendingSearchKept withHand)
            }

applyExploredChoice :: [Value] -> ExpectState -> Either Text ExpectState
applyExploredChoice cards state =
  case cards of
    [] -> pure state
    firstCard : _ -> do
      pid <- cardPlayerId firstCard
      cardIds <- traverse cardId cards
      withHand <- addCardsToHand (actionsCommitted state) pid cards state
      pure withHand { exploredIds = Map.insert pid cardIds (exploredIds withHand) }

handleKeepCards :: ExpectState -> Object -> Either Text ExpectState
handleKeepCards state notification = do
  keptCards <- notificationCardValues notification
  case keptCards of
    [] -> pure state
    firstCard : _ -> do
      pid <- cardPlayerId firstCard
      keptIds <- Set.fromList <$> traverse cardId keptCards
      let currentExplored = Set.fromList (Map.findWithDefault [] pid (exploredIds state))
          discards = Set.toList (currentExplored `Set.difference` keptIds)
      pure state
        { playerStates = Map.adjust (removeCards discards) pid (playerStates state)
        , exploredIds = Map.delete pid (exploredIds state)
        }

handleDiscard :: ExpectState -> Object -> Either Text ExpectState
handleDiscard state notification = do
  args <- objectField "args" notification
  cardIds <- discardCardIds args
  case takeQueuedDrawCards cardIds state of
    Just stateWithoutQueuedDraw -> pure stateWithoutQueuedDraw
    Nothing ->
      case discardOwnerMaybe (cardIndex state) cardIds of
        Nothing -> pure state
        Just ownerResult -> do
          owner <- ownerResult
          pure state { playerStates = Map.adjust (removeCards cardIds) owner (playerStates state) }

handlePlayCard :: Map Int Text -> ExpectState -> Object -> Either Text ExpectState
handlePlayCard _cardTypes state notification = do
  args <- objectField "args" notification
  case (optionalField "money" args, optionalField "card" args) of
    (Nothing, _) -> pure state
    (_, Nothing) -> pure state
    (Just _, Just cardValue) -> do
      pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
      _ <- lookupPlayer (playersById state) pid
      playedId <- cardId cardValue
      card <- lookupKnownCardName (cardIndex state) playedId
      moneyIds <- traverse (intValue "playcard money") =<< arrayField "money" args
      let base = maybe 0 id (tableauCount =<< Map.lookup pid (playerStates state))
          wait = handWaitTableau =<< Map.lookup pid (playerStates state)
          nextWait =
            case wait of
              Just existing | existing > base -> existing + 1
              _ -> base + 1
          updatePlayer =
            removeCards (playedId : moneyIds)
              . (\playerState -> playerState { handFresh = False, handWaitTableau = Just nextWait })
      pure state
        { playerStates = Map.adjust updatePlayer pid (playerStates state)
        , tableauCards = Map.alter (Just . (<> [card]) . maybe [] id) pid (tableauCards state)
        }
handleDiscardFromTableau :: ExpectState -> Object -> Either Text ExpectState
handleDiscardFromTableau state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardInstanceId <- intValue "discardfromtableau card" cardValue
      case Map.lookup cardInstanceId (knownCardOwners (cardIndex state)) of
        Nothing -> pure state
        Just owner -> do
          name <- lookupKnownCardName (cardIndex state) cardInstanceId
          pure state
            { tableauCards = Map.adjust (removeOne name) owner (tableauCards state)
            , playerStates = Map.adjust (removeGoodWorld name) owner (playerStates state)
            }

handleGoodProduction :: ExpectState -> Object -> Either Text ExpectState
handleGoodProduction state notification = do
  args <- objectField "args" notification
  goodId <- intValue "goodproduction good_id" =<< field "good_id" args
  worldId <- intValue "goodproduction world_id" =<< field "world_id" args
  if goodId `Set.member` activeGoods state
    then pure state
    else
      case Map.lookup worldId (knownCardOwners (cardIndex state)) of
        Nothing -> pure state { goodWorlds = Map.insert goodId worldId (goodWorlds state), activeGoods = Set.insert goodId (activeGoods state) }
        Just owner -> do
          world <- lookupKnownCardName (cardIndex state) worldId
          pure state
            { goodWorlds = Map.insert goodId worldId (goodWorlds state)
            , activeGoods = Set.insert goodId (activeGoods state)
            , playerStates = Map.adjust (addGoodWorld world) owner (playerStates state)
            }

handleConsume :: ExpectState -> Object -> Either Text ExpectState
handleConsume state notification = do
  args <- objectField "args" notification
  goodId <- intValue "consume good_id" =<< field "good_id" args
  case Map.lookup goodId (goodWorlds state) of
    Nothing -> pure state
    Just worldId -> do
      world <- lookupKnownCardName (cardIndex state) worldId
      let owner = Map.lookup worldId (knownCardOwners (cardIndex state))
      pure state
        { activeGoods = Set.delete goodId (activeGoods state)
        , playerStates = maybe (playerStates state) (\pid -> Map.adjust (removeGoodWorld world) pid (playerStates state)) owner
        }

handleUpdatePrestige :: ExpectState -> Object -> Either Text ExpectState
handleUpdatePrestige state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "updatePrestige player_id" =<< field "player_id" args)
  prestige <- intValue "updatePrestige prestige" =<< field "prestige" args
  pure state { playerStates = Map.adjust (\playerState -> playerState { prestigeCount = Just prestige }) pid (playerStates state) }

handleUpdateCardCount :: ExpectState -> Object -> Either Text ExpectState
handleUpdateCardCount state notification = do
  args <- objectField "args" notification
  withTableau <- updateCountObject "tableau" updateTableau args state
  updateCountObject "hand" updateHand args withTableau
  where
    updateTableau count playerState =
      playerState { tableauCount = Just count }

    updateHand count playerState =
      case handWaitTableau playerState of
        Just wait | maybe 0 id (tableauCount playerState) < wait -> playerState
        _ ->
          playerState
            { handWaitTableau = Nothing
            , handCount = Just count
            , handFresh = True
            }

handleUpdateScore :: ExpectState -> Object -> Either Text ExpectState
handleUpdateScore state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "updateScore player_id" =<< field "player_id" args)
  vp <- intValue "updateScore vp" =<< field "vp" args
  pure state { playerStates = Map.adjust (\playerState -> playerState { vpCount = vp }) pid (playerStates state) }

updateCountObject ::
  Text ->
  (Int -> PlayerExpectState -> PlayerExpectState) ->
  Object ->
  ExpectState ->
  Either Text ExpectState
updateCountObject name update args state =
  case optionalField name args of
    Nothing -> pure state
    Just (Object counts) ->
      objectKeyValues counts >>= foldM updateCount state
    Just (Array counts)
      | Vector.null counts -> pure state
      | otherwise -> Left ("updateCardCount " <> name <> " is a non-empty array")
    Just _ -> Left ("updateCardCount " <> name <> " is not an object")
  where
    updateCount state' (pid, value) = do
      count <- intValue ("updateCardCount " <> name) value
      pure state' { playerStates = Map.adjust (update count) pid (playerStates state') }

addCardsToHand :: Bool -> PlayerId -> [Value] -> ExpectState -> Either Text ExpectState
addCardsToHand trackCount pid cards state = do
  entries <- traverse cardEntry cards
  let updatePlayer playerState =
        foldl
          (\current (cid, name) -> addHandCard trackCount cid name current)
          playerState
          entries
  pure state { playerStates = Map.adjust updatePlayer pid (playerStates state) }
  where
    cardEntry cardValue = do
      cid <- cardId cardValue
      name <- lookupKnownCardName (cardIndex state) cid
      pure (cid, name)

flushLoggedDraw :: PlayerId -> Int -> ExpectState -> Either Text ExpectState
flushLoggedDraw pid cardCount state =
  case takeFirstMatching matches (pendingDraws state) of
    Nothing -> pure state
    Just (before, draw, after) -> do
      withCards <- addCardsToHand True pid (pendingDrawCards draw) state
      pure withCards { pendingDraws = before <> after }
  where
    matches draw =
      pendingDrawPlayer draw == pid && length (pendingDrawCards draw) == cardCount

takeFirstMatching :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
takeFirstMatching matches =
  go []
  where
    go _ [] = Nothing
    go seen (value : rest) =
      if matches value
        then Just (seen, value, rest)
        else go (seen <> [value]) rest

takeQueuedDrawCards :: [Int] -> ExpectState -> Maybe ExpectState
takeQueuedDrawCards cardIds state =
  case go [] (pendingDraws state) of
    Nothing -> Nothing
    Just updated -> Just state { pendingDraws = updated }
  where
    wanted = Set.fromList cardIds

    go _ [] = Nothing
    go seen (draw : rest) =
      let (found, kept) = List.partition (cardInWanted wanted) (pendingDrawCards draw)
       in if length found == Set.size wanted
            then
              let updated =
                    if null kept
                      then seen <> rest
                      else seen <> [draw { pendingDrawCards = kept }] <> rest
               in Just updated
            else go (seen <> [draw]) rest

    cardInWanted wantedIds cardValue =
      case cardId cardValue of
        Right cid -> cid `Set.member` wantedIds
        Left _ -> False

playerIdByName :: Text -> ExpectState -> Either Text PlayerId
playerIdByName name state =
  case [playerId player | player <- Map.elems (playersById state), playerName player == name] of
    [pid] -> pure pid
    [] -> Left ("unknown player name " <> name)
    _ -> Left ("duplicate player name " <> name)

addHandCard :: Bool -> Int -> Text -> PlayerExpectState -> PlayerExpectState
addHandCard trackCount cid name playerState =
  playerState
    { handCards = Map.insert cid name (handCards playerState)
    , handCount =
        if trackCount && handFresh playerState
          then (+ 1) <$> handCount playerState
          else handCount playerState
    }

removeCards :: [Int] -> PlayerExpectState -> PlayerExpectState
removeCards cardIds playerState =
  foldl (flip removeCard) playerState cardIds

removeCard :: Int -> PlayerExpectState -> PlayerExpectState
removeCard cid playerState =
  let hadCard = Map.member cid (handCards playerState)
   in playerState
        { handCards = Map.delete cid (handCards playerState)
        , handCount =
            if hadCard && handFresh playerState
              then subtract 1 <$> handCount playerState
              else handCount playerState
        }

addGoodWorld :: Text -> PlayerExpectState -> PlayerExpectState
addGoodWorld world playerState =
  playerState
    { goodsCount = goodsCount playerState + 1
    , goodsWorlds = goodsWorlds playerState <> [canonicalCardName world]
    }

removeGoodWorld :: Text -> PlayerExpectState -> PlayerExpectState
removeGoodWorld world playerState =
  if canonicalCardName world `elem` goodsWorlds playerState
    then
      playerState
        { goodsCount = goodsCount playerState - 1
        , goodsWorlds = removeOne (canonicalCardName world) (goodsWorlds playerState)
        }
    else playerState

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne wanted (value : rest)
  | wanted == value = rest
  | otherwise = value : removeOne wanted rest

notificationCardValues :: Object -> Either Text [Value]
notificationCardValues notification =
  case optionalField "args" notification of
    Just (Array values) -> pure (Vector.toList values)
    Just (Object obj) -> pure (objectValues obj)
    _ -> Left "notification args is not a card array or object"

cardPlayerId :: Value -> Either Text PlayerId
cardPlayerId value = do
  cardObject <- expectObject "card" value
  PlayerId <$> (intValue "card location_arg" =<< field "location_arg" cardObject)

discardOwnerMaybe :: CardIndex -> [Int] -> Maybe (Either Text PlayerId)
discardOwnerMaybe index cardIds =
  case fmap (`Map.lookup` knownCardOwners index) cardIds of
    [] -> Just (Left "discard has no cards")
    owners
      | all (== Nothing) owners -> Nothing
      | any (== Nothing) owners ->
          Just (Left ("discard has partly unknown owners: " <> Text.intercalate ", " (fmap renderOwner owners)))
      | otherwise ->
          case sequence owners of
            Just (owner : rest)
              | all (== owner) rest -> Just (Right owner)
              | otherwise ->
                  Just
                    ( Left
                        ( "discard has mixed owners: "
                            <> Text.intercalate ", " (fmap (showText . unPlayerId) (owner : rest))
                        )
                    )
            _ -> Just (Left "discard owner lookup unexpectedly failed")
  where
    renderOwner Nothing = "<unknown>"
    renderOwner (Just owner) = showText (unPlayerId owner)

lookupPlayerState :: PlayerId -> ExpectState -> Either Text PlayerExpectState
lookupPlayerState pid state =
  case Map.lookup pid (playerStates state) of
    Just playerState -> pure playerState
    Nothing -> Left ("missing expectation state for player " <> showText (unPlayerId pid))

lookupPlayer :: Map PlayerId Player -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case Map.lookup pid players of
    Just player -> pure player
    Nothing -> Left ("unknown player " <> showText (unPlayerId pid))

objectKeyValues :: Object -> Either Text [(PlayerId, Value)]
objectKeyValues obj =
  traverse entry (KeyMap.toList obj)
  where
    entry (key, value) = do
      pid <- PlayerId <$> intValue "object player id" (String (keyText key))
      pure (pid, value)

showText :: Show a => a -> Text
showText = Text.pack . show
