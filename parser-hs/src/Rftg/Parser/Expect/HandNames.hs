{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Expect.HandNames
  ( applyHandNameExpectations
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
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
  , field
  , intValue
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  , valueText
  )
import Rftg.Bga.State
  ( BgaSettleState (..)
  , BgaState (..)
  , bgaStateIsExploreStart
  , bgaStateIsNewActionRound
  , bgaStateIsSearch
  , bgaStateIsSearchDone
  , bgaStateIsSearchStart
  , bgaStateIsTerraformingEngineers
  , optionalBgaStateField
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  , Seat
  )
import Rftg.Keldon.Script
  ( ChoiceOrder (..)
  , ExpectLine (..)
  , KeldonChoice (..)
  , KeldonScript (..)
  , OrderedScriptLine (..)
  , ScriptLine (..)
  )
import Rftg.Parser.CardIndex
  ( CardIndex (..)
  , applyTakeoverCardMove
  , cardsFromNotification
  , cardsPlayerId
  , discardCardIds
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  , takeoverCardMove
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , ReviewSelection
  , cardId
  , cardName
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parsePlayers
  , selectReviewPlayer
  )
import Rftg.Parser.Phase.Cursor
  ( Phase (..)
  , PhaseCursor (..)
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )
import Rftg.Parser.Setup
  ( gamedataFor
  , parseStartOptions
  )

data PendingDiscard = PendingDiscard
  { pendingDiscardIds :: [Int]
  , pendingDiscardCards :: [Text]
  }
  deriving stock (Eq, Show)

data LoggedDraw = LoggedDraw
  { loggedDrawPlayer :: PlayerId
  , loggedDrawCards :: [Value]
  }
  deriving stock (Eq, Show)

data HandNameTarget
  = TargetDiscard [Text]
  | TargetDiscardPrestige Text
  | TargetConsumeHand [Text]
  | TargetPlace Text
  | TargetPayment [Text]
  | TargetDefense [Text]
  | TargetSettle Text
  deriving stock (Eq, Show)

data HandNameInsertion = HandNameInsertion
  { insertionSeat :: Seat
  , insertionOrder :: ChoiceOrder
  , insertionNames :: [Text]
  , insertionTarget :: HandNameTarget
  }
  deriving stock (Eq, Show)

data HandNamesState = HandNamesState
  { phaseCursor :: PhaseCursor
  , currentBgaState :: Maybe BgaState
  , cardIndex :: CardIndex
  , reviewPlayer :: Player
  , reviewHand :: Map Int Text
  , reviewChoiceMade :: Bool
  , startSeen :: Bool
  , startOptionPlayers :: Set PlayerId
  , roundZeroDiscardIds :: Map PlayerId [Int]
  , roundZeroSavePlayers :: Set PlayerId
  , tableauCards :: Map PlayerId [Text]
  , pendingDiscards :: Map PlayerId PendingDiscard
  , pendingMercenaries :: Map PlayerId [Text]
  , pendingUpgrades :: Map PlayerId Text
  , pendingLoggedDraws :: [LoggedDraw]
  , pendingConsumeHandDraws :: Map PlayerId [Value]
  , pendingExploredChoices :: [[Value]]
  , exploredNames :: Map PlayerId [Text]
  , exploredIds :: Map PlayerId [Int]
  , activeSearch :: Maybe PlayerId
  , searchOffer :: Map PlayerId (Int, Text)
  , scavengerPile :: Set Int
  , discardedTableauCards :: Set Int
  , playedCards :: Set Int
  , insertions :: [HandNameInsertion]
  }
  deriving stock (Eq, Show)

applyHandNameExpectations :: ReviewSelection -> Value -> KeldonScript -> Either Text KeldonScript
applyHandNameExpectations reviewSelection rootValue script = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypeInfos <- parseCardTypeInfos gamedatas
  let cardTypes = fmap cardTypeName cardTypeInfos
      cardInfosByName = Map.fromList [(cardTypeName info, info) | info <- Map.elems cardTypeInfos]
  review <- selectReviewPlayer reviewSelection players
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  reviewHandEntries <- initialReviewHand gamedatas cardTypes review
  startOptions <- parseStartOptions players gamedatas cardTypes
  notifications <- notificationObjects root
  walked <-
    foldM
      (handNamesStep players cardInfosByName cardTypes)
      (emptyHandNamesState players review startingCardIndex reviewHandEntries (Map.keysSet startOptions))
      (zip [0 :: Int ..] notifications)
  searched <- finishActiveSearch walked
  checked <-
    if null (pendingExploredChoices searched)
      then pure searched
      else Left ("unresolved explore choices for handnames: " <> showText (pendingExploredChoices searched))
  flushed <- flushAllPending players (length notifications) checked
  insertHandNames (insertions flushed) script

initialReviewHand :: Object -> Map Int Text -> Player -> Either Text (Map Int Text)
initialReviewHand gamedatas cardTypes player = do
  gamedata <- gamedataFor gamedatas (playerId player)
  handValues <- objectValues <$> objectField "hand" gamedata
  Map.fromList <$> traverse handEntry handValues
  where
    handEntry cardValue = do
      cid <- cardId cardValue
      name <- cardName cardTypes cardValue
      pure (cid, name)

emptyHandNamesState :: [Player] -> Player -> CardIndex -> Map Int Text -> Set PlayerId -> HandNamesState
emptyHandNamesState players review startingCardIndex hand startPlayers = HandNamesState
  { phaseCursor = initialPhaseCursor
  , currentBgaState = Nothing
  , cardIndex = startingCardIndex
  , reviewPlayer = review
  , reviewHand = hand
  , reviewChoiceMade = False
  , startSeen = False
  , startOptionPlayers = startPlayers
  , roundZeroDiscardIds = Map.empty
  , roundZeroSavePlayers = Set.empty
  , tableauCards = Map.fromList [(playerId player, []) | player <- players]
  , pendingDiscards = Map.empty
  , pendingMercenaries = Map.empty
  , pendingUpgrades = Map.empty
  , pendingLoggedDraws = []
  , pendingConsumeHandDraws = Map.empty
  , pendingExploredChoices = []
  , exploredNames = Map.empty
  , exploredIds = Map.empty
  , activeSearch = Nothing
  , searchOffer = Map.empty
  , scavengerPile = Set.empty
  , discardedTableauCards = Set.empty
  , playedCards = Set.empty
  , insertions = []
  }

handNamesStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  HandNamesState ->
  (Int, Object) ->
  Either Text HandNamesState
handNamesStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState players cardTypes eventIx stateWithCards notification
    "showTableau" -> handleShowTableau cardTypes stateWithCards notification
    "discardfromtableau" -> handleDiscardFromTableau cardInfosByName stateWithCards notification
    "discard" -> handleDiscard players eventIx stateWithCards notification
    "mercenary_used" -> handleMercenaryUsed stateWithCards notification
    "playcard" -> handlePlayCard players cardInfosByName cardTypes eventIx stateWithCards notification
    "takeover" -> handleTakeover cardTypes stateWithCards notification
    "explored_choice" -> handleExploredChoice cardTypes stateWithCards notification
    "keepcards" -> handleKeepCards players cardTypes eventIx stateWithCards notification
    "drawCards" -> handleDrawCards stateWithCards notification
    "drawCards_def" -> handleDrawCardsDef players cardInfosByName eventIx stateWithCards notification
    "explored_choice_log" -> handleExploredChoiceLog stateWithCards notification
    "consumecard" -> handleConsumeCard cardInfosByName eventIx stateWithCards notification
    "consume" -> handleConsume players cardInfosByName eventIx stateWithCards notification
    "goodproduction" -> handleGoodProduction cardInfosByName stateWithCards notification
    "gambling" -> handleGambling stateWithCards notification
    "scavengerUpdate" -> handleScavengerUpdate players stateWithCards notification
    "scavengeFromExplore" -> handleScavengerUpdate players stateWithCards notification
    _ -> pure stateWithCards

handleTakeover :: Map Int Text -> HandNamesState -> Object -> Either Text HandNamesState
handleTakeover cardTypes state notification = do
  move <- takeoverCardMove cardTypes notification
  movedTableau <- applyTakeoverCardMove move (tableauCards state)
  pure state { tableauCards = movedTableau }

handleGameState :: [Player] -> Map Int Text -> Int -> HandNamesState -> Object -> Either Text HandNamesState
handleGameState players cardTypes eventIx state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      stateBeforeEnter <-
        if bgaStateIsSearch bgaState
          then pure state
          else finishActiveSearch state
      stateBeforeTakeover <-
        case bgaState of
          BgaSettleState BgaSettleTakeoverPrevent ->
            flushTakeoverPayment eventIx args stateBeforeEnter
          BgaSettleState BgaSettleTakeoverResolution ->
            flushTakeoverDefenses eventIx stateBeforeEnter
          _ -> pure stateBeforeEnter
      stateBeforeAdvance <-
        if bgaStateIsNewActionRound bgaState
          then flushAllPending players eventIx stateBeforeTakeover
          else pure stateBeforeTakeover
      advanced <- enterState players bgaState args stateBeforeAdvance
      if bgaStateIsExploreStart bgaState
        then flushPendingExplores cardTypes advanced
        else pure advanced

flushTakeoverPayment :: Int -> Object -> HandNamesState -> Either Text HandNamesState
flushTakeoverPayment eventIx outerArgs state = do
  args <- objectField "args" outerArgs
  pid <- PlayerId <$> (intValue "takeover player_id" =<< field "player_id" args)
  let mercenaries = Map.findWithDefault [] pid (pendingMercenaries state)
  case (mercenaries, Map.lookup pid (pendingDiscards state)) of
    ([], _) -> pure state
    (_, Nothing) ->
      Left ("takeover payment for player " <> showText (unPlayerId pid) <> " has mercenary powers but no discards")
    (_, Just pending) -> do
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      withExpectation <- emitHandNames order pid (TargetPayment (pendingDiscardCards pending)) state
      pure (removeReviewCards pid (pendingDiscardIds pending) withExpectation)
        { pendingMercenaries = Map.delete pid (pendingMercenaries withExpectation)
        , pendingDiscards = Map.delete pid (pendingDiscards withExpectation)
        }

flushTakeoverDefenses :: Int -> HandNamesState -> Either Text HandNamesState
flushTakeoverDefenses eventIx state =
  foldM flushOne state (Map.keys (pendingMercenaries state))
  where
    flushOne current pid =
      case Map.lookup pid (pendingDiscards current) of
        Nothing -> pure current
        Just pending -> do
          order <- cursorChoiceOrder (phaseCursor current) eventIx
          withExpectation <- emitHandNames order pid (TargetDefense (pendingDiscardCards pending)) current
          pure (removeReviewCards pid (pendingDiscardIds pending) withExpectation)
            { pendingMercenaries = Map.delete pid (pendingMercenaries withExpectation)
            , pendingDiscards = Map.delete pid (pendingDiscards withExpectation)
            }

enterState :: [Player] -> BgaState -> Object -> HandNamesState -> Either Text HandNamesState
enterState players bgaState args state
  | bgaStateIsSearchStart bgaState = do
      pid <- activePidFromState players args
      case activeSearch state of
        Nothing ->
          pure state
            { activeSearch = Just pid
            , phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
            , currentBgaState = Just bgaState
            }
        Just current ->
          Left
            ( "nested Search state for player "
                <> showText (unPlayerId current)
                <> " while entering Search for "
                <> showText (unPlayerId pid)
            )
  | bgaStateIsSearchDone bgaState = do
      pid <- activePidFromState players args
      case activeSearch state of
        Nothing -> Left ("Search done for player " <> showText (unPlayerId pid) <> " without active Search")
        Just current
          | current == pid ->
              pure state
                { phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
                , currentBgaState = Just bgaState
                }
          | otherwise ->
              Left
                ( "Search done for player "
                    <> showText (unPlayerId pid)
                    <> " while "
                    <> showText (unPlayerId current)
                    <> " is active"
                )
  | otherwise =
      pure state
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
        , currentBgaState = Just bgaState
        }

activePidFromState :: [Player] -> Object -> Either Text PlayerId
activePidFromState players args = do
  pid <- PlayerId <$> (intValue "gameStateChange active_player" =<< field "active_player" args)
  if pid `Set.member` Set.fromList (fmap playerId players)
    then pure pid
    else Left ("state has unknown active player " <> showText (unPlayerId pid))

finishActiveSearch :: HandNamesState -> Either Text HandNamesState
finishActiveSearch state =
  case activeSearch state of
    Nothing -> pure state
    Just pid ->
      case Map.lookup pid (searchOffer state) of
        Nothing -> Left ("Search ended for player " <> showText (unPlayerId pid) <> " without kept card")
        Just (cid, name) ->
          pure (addCardsToReviewHand pid [(cid, name)] state)
            { activeSearch = Nothing
            , searchOffer = Map.delete pid (searchOffer state)
            }

handleShowTableau :: Map Int Text -> HandNamesState -> Object -> Either Text HandNamesState
handleShowTableau cardTypes state notification = do
  args <- objectField "args" notification
  cardsObject <- expectObject "showTableau cards" =<< field "cards" args
  entries <- traverse tableauEntry (objectValues cardsObject)
  let reviewPid = playerId (reviewPlayer state)
      setupDiscardIds = Map.findWithDefault [] reviewPid (roundZeroDiscardIds state)
      setupChoiceMade =
        reviewPid `Set.member` startOptionPlayers state
          || not (null setupDiscardIds)
          || reviewPid `Set.member` roundZeroSavePlayers state
  pure state
    { startSeen = True
    , tableauCards = foldl addEntry (tableauCards state) entries
    , reviewHand = removeCardsFromHand setupDiscardIds (reviewHand state)
    , reviewChoiceMade = reviewChoiceMade state || setupChoiceMade
    }
  where
    tableauEntry cardValue = do
      pid <- cardPlayerId cardValue
      name <- cardName cardTypes cardValue
      pure (pid, name)

    addEntry table (pid, name) =
      Map.alter (Just . appendUnique name . maybe [] id) pid table

handleDiscardFromTableau :: Map Text CardTypeInfo -> HandNamesState -> Object -> Either Text HandNamesState
handleDiscardFromTableau cardInfosByName state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardInstanceId <- intValue "discardfromtableau card" cardValue
      if cardInstanceId `Set.member` discardedTableauCards state
        then pure state
        else do
          owner <- tableauDiscardOwner cardInstanceId args state
          case owner of
            Nothing -> pure state
            Just pid -> do
              name <- lookupKnownCardName (cardIndex state) cardInstanceId
              info <- lookupCardInfo cardInfosByName name
              let stateWithoutCard = state
                    { discardedTableauCards = Set.insert cardInstanceId (discardedTableauCards state)
                    , tableauCards = Map.adjust (removeOne name) pid (tableauCards state)
                    }
              if not (startSeen stateWithoutCard)
                then pure stateWithoutCard
                else if cardTypeType info == "world"
                  && (maybe False bgaStateIsTerraformingEngineers (currentBgaState state) || not (cardTypeIsSettlePaymentDiscardSource info))
                  then
                    pure stateWithoutCard
                      { pendingUpgrades = Map.insert pid name (pendingUpgrades stateWithoutCard)
                      }
                  else pure stateWithoutCard

tableauDiscardOwner :: Int -> Object -> HandNamesState -> Either Text (Maybe PlayerId)
tableauDiscardOwner cardInstanceId args state =
  case Map.lookup cardInstanceId (knownCardOwners (cardIndex state)) of
    Just owner -> pure (Just owner)
    Nothing ->
      case optionalField "player_id" args of
        Nothing -> pure Nothing
        Just playerValue -> do
          pid <- PlayerId <$> intValue "discardfromtableau player_id" playerValue
          pure (Just pid)

handleDiscard :: [Player] -> Int -> HandNamesState -> Object -> Either Text HandNamesState
handleDiscard players eventIx state notification = do
  args <- objectField "args" notification
  cardIds <- discardCardIds args
  cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
  case activeSearch state of
    Just pid -> handleSearchDiscard pid cardIds state
    Nothing ->
      case takeQueuedDrawCards cardIds (pendingLoggedDraws state) of
        Just (_owner, remainingDraws) ->
          pure state { pendingLoggedDraws = remainingDraws }
        Nothing ->
          if not (startSeen state)
            then do
              owner <- discardOwner (cardIndex state) cardIds
              pure state
                { roundZeroDiscardIds =
                    Map.alter (Just . (<> cardIds) . maybe [] id) owner (roundZeroDiscardIds state)
                }
            else do
              owner <- discardOwner (cardIndex state) cardIds
              handleOwnedDiscard players eventIx owner cardIds cards state

handleSearchDiscard :: PlayerId -> [Int] -> HandNamesState -> Either Text HandNamesState
handleSearchDiscard pid cardIds state =
  case (cardIds, Map.lookup pid (searchOffer state)) of
    ([discardedId], Just (offerId, _))
      | discardedId == offerId ->
          pure state { searchOffer = Map.delete pid (searchOffer state) }
    _ -> Left ("unexpected discard during Search: " <> showText cardIds)

handleOwnedDiscard :: [Player] -> Int -> PlayerId -> [Int] -> [Text] -> HandNamesState -> Either Text HandNamesState
handleOwnedDiscard players eventIx pid cardIds cards state
  | cursorRound (phaseCursor state) == Nothing =
      pure (storePendingDiscard pid (PendingDiscard cardIds cards) state)
  | cursorPhase (phaseCursor state) `elem` [Explore, Discard] = do
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      emitDiscardExpectation order pid cardIds cards state
  | otherwise = do
      let keepPending =
            if Map.member pid (pendingMercenaries state)
              then Map.lookup pid (pendingDiscards state)
              else Nothing
      flushed <-
        if keepPending == Nothing
          then flushPendingDiscard players eventIx pid state
          else pure state
      let pending =
            case keepPending of
              Nothing -> PendingDiscard cardIds cards
              Just old -> PendingDiscard (pendingDiscardIds old <> cardIds) (pendingDiscardCards old <> cards)
      pure (storePendingDiscard pid pending flushed)

storePendingDiscard :: PlayerId -> PendingDiscard -> HandNamesState -> HandNamesState
storePendingDiscard pid pending state =
  state { pendingDiscards = Map.insert pid pending (pendingDiscards state) }

handleMercenaryUsed :: HandNamesState -> Object -> Either Text HandNamesState
handleMercenaryUsed state notification = do
  args <- objectField "args" notification
  sourceId <- intValue "mercenary_used card" =<< field "card" args
  sourceName <- lookupKnownCardName (cardIndex state) sourceId
  case Map.lookup sourceId (knownCardOwners (cardIndex state)) of
    Nothing -> pure state
    Just owner ->
      pure state
        { pendingMercenaries =
            Map.alter (Just . appendUnique sourceName . maybe [] id) owner (pendingMercenaries state)
        }

handlePlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  Int ->
  HandNamesState ->
  Object ->
  Either Text HandNamesState
handlePlayCard players cardInfosByName cardTypes eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "money" args of
    Nothing -> pure state
    Just _ -> do
      cardValue <- field "card" args
      cardInstanceId <- cardId cardValue
      if cardInstanceId `Set.member` playedCards state
        then pure state
        else do
          pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
          _ <- lookupPlayer players pid
          card <- cardName cardTypes cardValue
          cardInfo <- lookupCardInfo cardInfosByName card
          moneyIds0 <- traverse (intValue "playcard money") =<< arrayFieldFromObject "money" args
          money0 <- traverse (lookupKnownCardName (cardIndex state)) moneyIds0
          let merc =
                if cardTypeIsMilitary cardInfo
                  then Map.findWithDefault [] pid (pendingMercenaries state)
                  else []
              pendingMercMoney = if null merc then Nothing else Map.lookup pid (pendingDiscards state)
              moneyIds = maybe [] pendingDiscardIds pendingMercMoney <> moneyIds0
              money = maybe [] pendingDiscardCards pendingMercMoney <> money0
              stateWithoutMerc =
                state
                  { pendingMercenaries = Map.delete pid (pendingMercenaries state)
                  , pendingDiscards =
                      if null merc
                        then pendingDiscards state
                        else Map.delete pid (pendingDiscards state)
                  }
          flushed <- flushPendingDiscard players eventIx pid stateWithoutMerc
          let withTableau = addTableauCard pid card flushed
              withPlayed = withTableau { playedCards = Set.insert cardInstanceId (playedCards withTableau) }
          case (cardTypeType cardInfo, Map.lookup pid (pendingUpgrades withPlayed)) of
            ("world", Just _oldWorld) -> do
              source <- settleReplaceSource cardInfosByName pid withPlayed
              order <- cursorChoiceOrder (phaseCursor withPlayed) eventIx
              withSettle <- emitHandNames order pid (TargetSettle source) withPlayed
              pure (removeReviewCards pid [cardInstanceId] withSettle)
                { pendingUpgrades = Map.delete pid (pendingUpgrades withSettle)
                }
            _ -> do
              order <- cursorChoiceOrder (phaseCursor withPlayed) eventIx
              withPlace <- emitHandNames order pid (TargetPlace card) withPlayed
              let withoutPlayed = removeReviewCards pid [cardInstanceId] withPlace
              if null money
                then pure withoutPlayed
                else do
                  withPayment <- emitHandNames order pid (TargetPayment money) withoutPlayed
                  pure (removeReviewCards pid moneyIds withPayment)

handleExploredChoice :: Map Int Text -> HandNamesState -> Object -> Either Text HandNamesState
handleExploredChoice cardTypes state notification = do
  cards <- cardsFromNotification "explored_choice" notification
  if cursorPhase (phaseCursor state) == Explore
    then applyExploredChoice cardTypes cards state
    else pure state { pendingExploredChoices = pendingExploredChoices state <> [cards] }

flushPendingExplores :: Map Int Text -> HandNamesState -> Either Text HandNamesState
flushPendingExplores cardTypes state =
  foldM (flip (applyExploredChoice cardTypes)) state (pendingExploredChoices state)
    >>= \flushed -> pure flushed { pendingExploredChoices = [] }

applyExploredChoice :: Map Int Text -> [Value] -> HandNamesState -> Either Text HandNamesState
applyExploredChoice cardTypes cards state = do
  pid <- cardsPlayerId "explored_choice" cards
  ids <- traverse cardId cards
  names <- traverse (cardName cardTypes) cards
  let entries = zip ids names
  pure (addCardsToReviewHand pid entries state)
    { exploredNames = Map.insert pid names (exploredNames state)
    , exploredIds = Map.insert pid ids (exploredIds state)
    }

handleKeepCards ::
  [Player] ->
  Map Int Text ->
  Int ->
  HandNamesState ->
  Object ->
  Either Text HandNamesState
handleKeepCards players cardTypes eventIx state notification = do
  cards <- cardsFromNotification "keepcards" notification
  pid <- cardsPlayerId "keepcards" cards
  flushed <- flushPendingDiscard players eventIx pid state
  keptNames <- traverse (cardName cardTypes) cards
  keptIds <- Set.fromList <$> traverse cardId cards
  case Map.lookup pid (exploredNames flushed) of
    Nothing -> Left ("keepcards for player " <> showText (unPlayerId pid) <> " without explored_choice")
    Just names -> do
      discards <- removeKeptCards names keptNames
      order <- cursorChoiceOrder (phaseCursor flushed) eventIx
      withExpectation <- emitHandNames order pid (TargetDiscard discards) flushed
      let drawnIds = Map.findWithDefault [] pid (exploredIds withExpectation)
          discardedIds = filter (`Set.notMember` keptIds) drawnIds
      pure (removeReviewCards pid discardedIds withExpectation)
        { exploredNames = Map.delete pid (exploredNames withExpectation)
        , exploredIds = Map.delete pid (exploredIds withExpectation)
        }

handleDrawCards :: HandNamesState -> Object -> Either Text HandNamesState
handleDrawCards state notification = do
  cards <- cardsFromNotification "drawCards" notification
  case cards of
    [] -> Left "drawCards notification has no cards"
    firstCard : _ -> do
      location <- cardLocation firstCard
      case location of
        Just "hand" -> handleHandDraw cards state
        Just "aside" -> handleSearchOffer cards state
        Just "explored" -> pure state
        Just "retrofit" -> pure state
        Just other -> Left ("unexpected drawCards location " <> other)
        Nothing -> handleHandDraw cards state

handleHandDraw :: [Value] -> HandNamesState -> Either Text HandNamesState
handleHandDraw cards state = do
  pid <- cardsPlayerId "drawCards" cards
  ids <- traverse cardId cards
  if not (null ids) && all (`Set.member` scavengerPile state) ids
    then do
      entries <- traverse (cardEntry (cardIndex state)) cards
      pure (addCardsToReviewHand pid entries state)
        { scavengerPile = foldr Set.delete (scavengerPile state) ids
        }
    else
      pure state { pendingLoggedDraws = pendingLoggedDraws state <> [LoggedDraw pid cards] }

handleSearchOffer :: [Value] -> HandNamesState -> Either Text HandNamesState
handleSearchOffer cards state =
  case activeSearch state of
    Nothing -> pure state
    Just pid ->
      case cards of
        [cardValue] -> do
          cid <- cardId cardValue
          name <- lookupKnownCardName (cardIndex state) cid
          pure state { searchOffer = Map.insert pid (cid, name) (searchOffer state) }
        _ -> Left "Search kept multiple aside cards"

handleDrawCardsDef :: [Player] -> Map Text CardTypeInfo -> Int -> HandNamesState -> Object -> Either Text HandNamesState
handleDrawCardsDef players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player_name" args of
    Nothing -> pure state
    Just playerNameValue -> do
      playerName <- textValue "drawCards_def player_name" playerNameValue
      player <- lookupPlayerByName players playerName
      let pid = playerId player
      stateAfterSource <-
        case optionalField "card_name" args of
          Nothing -> pure state
          Just sourceValue -> do
            source <- canonicalCardName <$> textValue "drawCards_def card_name" sourceValue
            sourceInfo <- lookupCardInfo cardInfosByName source
            if cardTypeHasPhase4Draw sourceInfo
              then flushPendingDiscard players eventIx pid state
              else pure state
      case optionalField "card_nbr" args of
        Nothing -> pure stateAfterSource
        Just countValue -> do
          count <- intValue "drawCards_def card_nbr" countValue
          flushLoggedDraw pid count stateAfterSource

handleExploredChoiceLog :: HandNamesState -> Object -> Either Text HandNamesState
handleExploredChoiceLog state notification = do
  args <- objectField "args" notification
  case (optionalField "player_id" args, optionalField "nbr" args) of
    (Just pidValue, Just countValue) -> do
      pid <- PlayerId <$> intValue "explored_choice_log player_id" pidValue
      count <- intValue "explored_choice_log nbr" countValue
      flushLoggedDraw pid count state
    _ -> pure state

handleConsumeCard :: Map Text CardTypeInfo -> Int -> HandNamesState -> Object -> Either Text HandNamesState
handleConsumeCard cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "consumecard player_id" =<< field "player_id" args)
  power <- canonicalCardName <$> (textValue "consumecard world_name" =<< field "world_name" args)
  powerInfo <- lookupCardInfo cardInfosByName power
  let pending = Map.lookup pid (pendingDiscards state)
      withoutPending = state { pendingDiscards = Map.delete pid (pendingDiscards state) }
  if cardTypeHasDiscardPrestige powerInfo
    then
      case pending of
        Nothing -> Left ("DISCARD_PRESTIGE has no pending discard for " <> showText (unPlayerId pid))
        Just discard ->
          case pendingDiscardCards discard of
            [card] -> do
              order <- cursorChoiceOrder (phaseCursor state) eventIx
              withExpectation <- emitHandNames order pid (TargetDiscardPrestige card) withoutPending
              pure (removeReviewCards pid (pendingDiscardIds discard) withExpectation)
            cards ->
              Left
                ( "DISCARD_PRESTIGE expected one pending card for "
                    <> showText (unPlayerId pid)
                    <> ", got "
                    <> showText (length cards)
                )
    else do
      withConsumeHand <-
        case pending of
          Nothing -> pure withoutPending
          Just discard -> do
            order <- afterWindfallFlushOrder <$> cursorChoiceOrder (phaseCursor state) eventIx
            withExpectation <- emitHandNames order pid (TargetConsumeHand (pendingDiscardCards discard)) withoutPending
            pure (removeReviewCards pid (pendingDiscardIds discard) withExpectation)
      applyPendingConsumeHandDraw pid withConsumeHand

handleConsume :: [Player] -> Map Text CardTypeInfo -> Int -> HandNamesState -> Object -> Either Text HandNamesState
handleConsume players cardInfosByName eventIx state notification =
  if currentBgaState state == Just (BgaSettleState BgaSettleTakeoverDefenderBoost)
    then pure state
    else do
      args <- objectField "args" notification
      case optionalField "player_id" args of
        Nothing -> pure state
        Just pidValue -> do
          pid <- PlayerId <$> intValue "consume player_id" pidValue
          _ <- lookupPlayer players pid
          case optionalField "world_id" args of
            Just worldIdValue -> do
              powerId <- intValue "consume world_id" worldIdValue
              power <- lookupKnownCardName (cardIndex state) powerId
              powerInfo <- lookupCardInfo cardInfosByName power
              if cardTypeHasGoodForSettleCost powerInfo && cursorPhase (phaseCursor state) == Settle
                then pure state
                else flushPendingDiscard players eventIx pid state
            Nothing -> flushPendingDiscard players eventIx pid state

handleGoodProduction :: Map Text CardTypeInfo -> HandNamesState -> Object -> Either Text HandNamesState
handleGoodProduction cardInfosByName state notification = do
  args <- objectField "args" notification
  case optionalField "windfallreason" args of
    Nothing -> pure state
    Just reasonValue -> do
      let reason = valueText reasonValue
      if reason == "phase"
        then pure state
        else do
          sourceId <- intValue "goodproduction windfallreason source" reasonValue
          source <- lookupKnownCardName (cardIndex state) sourceId
          sourceInfo <- lookupCardInfo cardInfosByName source
          if not (cardTypeHasWindfallProduceIfDiscard sourceInfo)
            then pure state
            else
              case Map.lookup sourceId (knownCardOwners (cardIndex state)) of
                Nothing -> pure state
                Just owner ->
                  case Map.lookup owner (pendingDiscards state) of
                    Just pending
                      | length (pendingDiscardCards pending) == 1 ->
                          pure (removeReviewCards owner (pendingDiscardIds pending) state)
                            { pendingDiscards = Map.delete owner (pendingDiscards state)
                            }
                    _ -> pure state

handleGambling :: HandNamesState -> Object -> Either Text HandNamesState
handleGambling state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "gambling player_id" =<< field "player_id" args)
  flipped <- canonicalCardName <$> (textValue "gambling card_name" =<< field "card_name" args)
  case Map.lookup pid (pendingDiscards state) of
    Just pending
      | pendingDiscardCards pending == [flipped] ->
          pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }
    _ -> pure state

handleScavengerUpdate :: [Player] -> HandNamesState -> Object -> Either Text HandNamesState
handleScavengerUpdate players state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cid <- cardId cardValue
      let stateWithPile = state { scavengerPile = Set.insert cid (scavengerPile state) }
      tableauOwner <- scavengerOwner players stateWithPile
      namedOwner <- scavengerNamedOwner players args
      let owner = firstJust tableauOwner namedOwner
          stateWithRoundZeroSave =
            case namedOwner of
              Just pid | not (startSeen stateWithPile) ->
                stateWithPile { roundZeroSavePlayers = Set.insert pid (roundZeroSavePlayers stateWithPile) }
              _ -> stateWithPile
      case owner of
        Nothing -> pure stateWithRoundZeroSave
        Just pid -> clearScavengedPending cid pid stateWithRoundZeroSave

scavengerOwner :: [Player] -> HandNamesState -> Either Text (Maybe PlayerId)
scavengerOwner _players state =
  case [pid | (pid, cards) <- Map.toList (tableauCards state), "Galactic Scavengers" `elem` cards] of
    [] -> pure Nothing
    [pid] -> pure (Just pid)
    owners -> Left ("multiple Galactic Scavengers owners: " <> showText (fmap unPlayerId owners))

scavengerNamedOwner :: [Player] -> Object -> Either Text (Maybe PlayerId)
scavengerNamedOwner players args =
  case optionalField "player_name" args of
    Nothing -> pure Nothing
    Just playerNameValue -> do
      playerName <- textValue "scavengerUpdate player_name" playerNameValue
      Just . playerId <$> lookupPlayerByName players playerName

clearScavengedPending :: Int -> PlayerId -> HandNamesState -> Either Text HandNamesState
clearScavengedPending cid pid state =
  case Map.lookup pid (pendingDiscards state) of
    Nothing -> pure (markReviewChoice pid state)
    Just pending ->
      if cid `elem` pendingDiscardIds pending
        then do
          let pending' = removePendingCard cid pending
              pendingMap =
                if null (pendingDiscardIds pending')
                  then Map.delete pid (pendingDiscards state)
                  else Map.insert pid pending' (pendingDiscards state)
          pure (removeReviewCards pid [cid] (markReviewChoice pid state))
            { pendingDiscards = pendingMap
            }
        else pure (markReviewChoice pid state)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just value) _ = Just value
firstJust Nothing fallback = fallback

flushLoggedDraw :: PlayerId -> Int -> HandNamesState -> Either Text HandNamesState
flushLoggedDraw pid count state =
  case takeFirstMatching matches (pendingLoggedDraws state) of
    Nothing -> pure state
    Just (before, draw, after) ->
      applyLoggedDraw pid (loggedDrawCards draw) state { pendingLoggedDraws = before <> after }
  where
    matches draw =
      loggedDrawPlayer draw == pid && length (loggedDrawCards draw) == count

applyLoggedDraw :: PlayerId -> [Value] -> HandNamesState -> Either Text HandNamesState
applyLoggedDraw pid cards state =
  if cursorPhase (phaseCursor state) == Consume && Map.member pid (pendingDiscards state)
    then
      pure state
        { pendingConsumeHandDraws =
            Map.alter (Just . (<> cards) . maybe [] id) pid (pendingConsumeHandDraws state)
        }
    else do
      entries <- traverse (cardEntry (cardIndex state)) cards
      pure (addCardsToReviewHand pid entries state)

applyPendingConsumeHandDraw :: PlayerId -> HandNamesState -> Either Text HandNamesState
applyPendingConsumeHandDraw pid state =
  case Map.lookup pid (pendingConsumeHandDraws state) of
    Nothing -> pure state
    Just cards -> do
      entries <- traverse (cardEntry (cardIndex state)) cards
      pure (addCardsToReviewHand pid entries state)
        { pendingConsumeHandDraws = Map.delete pid (pendingConsumeHandDraws state)
        }

flushAllPending :: [Player] -> Int -> HandNamesState -> Either Text HandNamesState
flushAllPending players eventIx state =
  foldM
    (\current pid -> flushPendingDiscard players eventIx pid current)
    state
    (Map.keys (pendingDiscards state))

flushPendingDiscard :: [Player] -> Int -> PlayerId -> HandNamesState -> Either Text HandNamesState
flushPendingDiscard _players eventIx pid state =
  case Map.lookup pid (pendingDiscards state) of
    Nothing -> pure state
    Just pending -> do
      order <-
        case cursorRound (phaseCursor state) of
          Nothing -> pure (ChoiceOrder [])
          Just _ -> cursorChoiceOrder (phaseCursor state) eventIx
      withExpectation <- emitHandNames order pid (TargetDiscard (pendingDiscardCards pending)) state
      pure (removeReviewCards pid (pendingDiscardIds pending) withExpectation)
        { pendingDiscards = Map.delete pid (pendingDiscards withExpectation)
        }

emitDiscardExpectation :: ChoiceOrder -> PlayerId -> [Int] -> [Text] -> HandNamesState -> Either Text HandNamesState
emitDiscardExpectation order pid ids cards state = do
  withExpectation <- emitHandNames order pid (TargetDiscard cards) state
  pure (removeReviewCards pid ids withExpectation)

emitHandNames :: ChoiceOrder -> PlayerId -> HandNameTarget -> HandNamesState -> Either Text HandNamesState
emitHandNames order pid target state
  | pid /= playerId (reviewPlayer state) = pure state
  | otherwise =
      let insertion = HandNameInsertion
            { insertionSeat = playerSeat (reviewPlayer state)
            , insertionOrder = order
            , insertionNames = List.sort (Map.elems (reviewHand state))
            , insertionTarget = target
            }
       in pure state
            { insertions = insertions state <> [insertion]
            , reviewChoiceMade = True
            }

markReviewChoice :: PlayerId -> HandNamesState -> HandNamesState
markReviewChoice pid state =
  if pid == playerId (reviewPlayer state)
    then state { reviewChoiceMade = True }
    else state

addCardsToReviewHand :: PlayerId -> [(Int, Text)] -> HandNamesState -> HandNamesState
addCardsToReviewHand pid entries state =
  if pid == playerId (reviewPlayer state)
    then state { reviewHand = foldl addEntry (reviewHand state) entries }
    else state
  where
    addEntry hand (cid, name) = Map.insert cid name hand

removeReviewCards :: PlayerId -> [Int] -> HandNamesState -> HandNamesState
removeReviewCards pid ids state =
  if pid == playerId (reviewPlayer state)
    then state { reviewHand = removeCardsFromHand ids (reviewHand state) }
    else state

removeCardsFromHand :: [Int] -> Map Int Text -> Map Int Text
removeCardsFromHand ids hand =
  foldr Map.delete hand ids

addTableauCard :: PlayerId -> Text -> HandNamesState -> HandNamesState
addTableauCard pid card state =
  state { tableauCards = Map.alter (Just . appendUnique card . maybe [] id) pid (tableauCards state) }

settleReplaceSource :: Map Text CardTypeInfo -> PlayerId -> HandNamesState -> Either Text Text
settleReplaceSource cardInfosByName pid state =
  case filter hasSettleReplace (Map.findWithDefault [] pid (tableauCards state)) of
    source : _ -> pure source
    [] -> Left ("upgrade has no settle-replace source for player " <> showText (unPlayerId pid))
  where
    hasSettleReplace name =
      maybe False cardTypeHasSettleReplace (Map.lookup name cardInfosByName)

removeKeptCards :: [Text] -> [Text] -> Either Text [Text]
removeKeptCards =
  foldM removeKept
  where
    removeKept remaining kept =
      case break (== kept) remaining of
        (_, []) -> Left ("kept card was not explored: " <> kept)
        (before, _ : after) -> pure (before <> after)

insertHandNames :: [HandNameInsertion] -> KeldonScript -> Either Text KeldonScript
insertHandNames handNames script =
  foldM insertOne script handNames
  where
    insertOne current insertion = do
      linesForSeat <- case Map.lookup (insertionSeat insertion) (scriptChoices current) of
        Nothing -> Left ("handnames target seat has no choices: " <> showText insertion)
        Just lines_ -> pure lines_
      updatedLines <- insertIntoLines insertion linesForSeat
      pure current
        { scriptChoices = Map.insert (insertionSeat insertion) updatedLines (scriptChoices current)
        }

insertIntoLines :: HandNameInsertion -> [OrderedScriptLine] -> Either Text [OrderedScriptLine]
insertIntoLines insertion =
  go []
  where
    go _ [] =
      Left
        ( "handnames target choice not found at "
            <> showText (insertionOrder insertion)
            <> ": "
            <> showText (insertionTarget insertion)
        )
    go seen (line : rest)
      | orderedLineOrder line == insertionOrder insertion
          && targetMatches (insertionTarget insertion) (orderedLine line) =
          pure (seen <> [expectLine, line] <> rest)
      | otherwise = go (seen <> [line]) rest

    expectLine = OrderedScriptLine
      { orderedLineOrder = insertionOrder insertion
      , orderedLine = Expect (insertionSeat insertion) (ExpectHandNames (insertionNames insertion))
      }

targetMatches :: HandNameTarget -> ScriptLine -> Bool
targetMatches target line =
  case (target, line) of
    (TargetDiscard cards, Choice _ _ (ChooseDiscard actual)) -> cards == actual
    (TargetDiscardPrestige card, Choice _ _ (ChooseDiscardPrestige actual)) -> card == actual
    (TargetConsumeHand cards, Choice _ _ (ChooseConsumeHand actual)) -> cards == actual
    (TargetPlace card, Choice _ _ (ChoosePlace (Just actual))) -> card == actual
    (TargetPayment cards, Choice _ _ (ChoosePayment actual _specials)) -> cards == actual
    (TargetDefense cards, Choice _ _ (ChooseDefend actual _specials)) -> cards == actual
    (TargetSettle card, Choice _ _ (ChooseSettle (Just actual))) -> card == actual
    _ -> False

takeQueuedDrawCards :: [Int] -> [LoggedDraw] -> Maybe (PlayerId, [LoggedDraw])
takeQueuedDrawCards cardIds = go []
  where
    wanted = Set.fromList cardIds

    go _ [] = Nothing
    go seen (draw : rest) =
      let (found, kept) = List.partition (cardInWanted wanted) (loggedDrawCards draw)
       in if length found == Set.size wanted
            then
              let updated =
                    if null kept
                      then seen <> rest
                      else seen <> [draw { loggedDrawCards = kept }] <> rest
               in Just (loggedDrawPlayer draw, updated)
            else go (seen <> [draw]) rest

    cardInWanted wantedIds cardValue =
      case cardId cardValue of
        Right cid -> cid `Set.member` wantedIds
        Left _ -> False

takeFirstMatching :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
takeFirstMatching matches = go []
  where
    go _ [] = Nothing
    go seen (value : rest)
      | matches value = Just (seen, value, rest)
      | otherwise = go (seen <> [value]) rest

cardEntry :: CardIndex -> Value -> Either Text (Int, Text)
cardEntry index value = do
  cid <- cardId value
  name <- lookupKnownCardName index cid
  pure (cid, name)

cardLocation :: Value -> Either Text (Maybe Text)
cardLocation value = do
  cardObject <- expectObject "drawCards card" value
  case optionalField "location" cardObject of
    Nothing -> pure Nothing
    Just (String location) -> pure (Just location)
    Just _ -> Left "card location is not text"

cardPlayerId :: Value -> Either Text PlayerId
cardPlayerId value = do
  cardObject <- expectObject "card" value
  PlayerId <$> (intValue "card location_arg" =<< field "location_arg" cardObject)

discardOwner :: CardIndex -> [Int] -> Either Text PlayerId
discardOwner index cardIds = do
  owners <- traverse lookupOwner cardIds
  case owners of
    [] -> Left "discard has no cards"
    owner : rest
      | all (== owner) rest -> pure owner
      | otherwise ->
          Left
            ( "discard has mixed owners: "
                <> Text.intercalate ", " (fmap (showText . unPlayerId) owners)
            )
  where
    lookupOwner cid =
      case Map.lookup cid (knownCardOwners index) of
        Just owner -> pure owner
        Nothing -> Left ("discard references card with unknown owner " <> showText cid)

arrayFieldFromObject :: Text -> Object -> Either Text [Value]
arrayFieldFromObject name obj =
  case optionalField name obj of
    Just (Array values) -> pure (Vector.toList values)
    Just _ -> Left (name <> " is not an array")
    Nothing -> Left ("missing " <> name)

removePendingCard :: Int -> PendingDiscard -> PendingDiscard
removePendingCard cid pending =
  let pairs = filter ((/= cid) . fst) (zip (pendingDiscardIds pending) (pendingDiscardCards pending))
   in PendingDiscard (fmap fst pairs) (fmap snd pairs)

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

lookupPlayerByName :: [Player] -> Text -> Either Text Player
lookupPlayerByName players name =
  case filter ((== name) . playerName) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> name)
    _ -> Left ("duplicate player " <> name)

afterWindfallFlushOrder :: ChoiceOrder -> ChoiceOrder
afterWindfallFlushOrder (ChoiceOrder parts) =
  ChoiceOrder (parts <> [2])

appendUnique :: Eq a => a -> [a] -> [a]
appendUnique value values =
  if value `elem` values
    then values
    else values <> [value]

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne wanted (value : rest)
  | wanted == value = rest
  | otherwise = value : removeOne wanted rest

showText :: Show a => a -> Text
showText = Text.pack . show
