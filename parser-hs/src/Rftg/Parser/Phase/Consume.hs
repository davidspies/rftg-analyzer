{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Consume
  ( parseConsumeChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , field
  , intValue
  , objectField
  , optionalField
  , expectObject
  , textValue
  )
import Rftg.Bga.State (optionalBgaStateField)
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  , Seat
  )
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , ChoiceOrder (..)
  , KeldonChoice (..)
  , KeldonScript
  , ScriptLine (..)
  , appendScript
  , choiceScriptAt
  , emptyScript
  )
import Rftg.Parser.CardIndex
  ( CardIndex (..)
  , cardsFromNotification
  , cardsPlayerId
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , cardId
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parsePlayers
  )
import Rftg.Parser.Phase.Cursor
  ( Phase (..)
  , PhaseCursor (..)
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data PendingDiscard = PendingDiscard
  { pendingDiscardIds :: [Int]
  , pendingDiscardCards :: [Text]
  }
  deriving stock (Eq, Show)

data QueuedDraw = QueuedDraw
  { queuedDrawPlayer :: PlayerId
  , queuedDrawIds :: [Int]
  }
  deriving stock (Eq, Show)

data ConsumeAnswer
  = ConsumeNoAnswer
  | ConsumeGood [Text]
  | ConsumeTrade Text
  | ConsumeHand [Text]
  deriving stock (Eq, Show)

data ConsumeBlock = ConsumeBlock
  { consumeBlockPlayer :: PlayerId
  , consumeBlockPower :: Text
  , consumeBlockOrder :: ChoiceOrder
  , consumeBlockAnswer :: ConsumeAnswer
  }
  deriving stock (Eq, Show)

data ConsumeState = ConsumeState
  { phaseCursor :: PhaseCursor
  , cardIndex :: CardIndex
  , activeGoods :: Map Int Int
  , queuedDraws :: [QueuedDraw]
  , pendingGamblingDiscards :: Map PlayerId PendingDiscard
  , pendingDiscards :: Map PlayerId PendingDiscard
  , consumeBlocks :: Map (Int, PlayerId, Text) ConsumeBlock
  , consumeScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseConsumeChoices :: Value -> Either Text KeldonScript
parseConsumeChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypeInfos <- parseCardTypeInfos gamedatas
  let cardTypes = fmap cardTypeName cardTypeInfos
      cardInfosByName = Map.fromList [(cardTypeName info, info) | info <- Map.elems cardTypeInfos]
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  walked <-
    foldM
      (consumeStep players cardInfosByName cardTypes)
      (emptyConsumeState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  ensureNoPendingGambling walked
  ensureNoPendingDiscards walked
  blockScript <- consumeBlocksScript players walked
  pure (consumeScript walked `appendScript` blockScript)

emptyConsumeState :: CardIndex -> ConsumeState
emptyConsumeState startingCardIndex = ConsumeState
  { phaseCursor = initialPhaseCursor
  , cardIndex = startingCardIndex
  , activeGoods = Map.empty
  , queuedDraws = []
  , pendingGamblingDiscards = Map.empty
  , pendingDiscards = Map.empty
  , consumeBlocks = Map.empty
  , consumeScript = emptyScript
  }

consumeStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  ConsumeState ->
  (Int, Object) ->
  Either Text ConsumeState
consumeStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "goodproduction" -> handleGoodProduction stateWithCards notification
    "drawCards" -> queueLoggedDraw stateWithCards notification
    "drawCards_def" -> handleDrawCardsDef players cardInfosByName eventIx stateWithCards notification
    "discard" -> handleDiscard stateWithCards notification
    "gambling" -> handleGambling stateWithCards notification
    "consumeprestige" -> handleConsumePrestige eventIx stateWithCards notification
    "consumecard" -> handleConsumeCard cardInfosByName eventIx stateWithCards notification
    "consume" -> handleConsume players cardInfosByName eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: ConsumeState -> Object -> Either Text ConsumeState
handleGameState state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState ->
      pure state
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
        }

handleGoodProduction :: ConsumeState -> Object -> Either Text ConsumeState
handleGoodProduction state notification = do
  args <- objectField "args" notification
  goodId <- intValue "goodproduction good_id" =<< field "good_id" args
  worldId <- intValue "goodproduction world_id" =<< field "world_id" args
  case Map.lookup goodId (activeGoods state) of
    Just existingWorldId
      | existingWorldId == worldId -> pure state
      | otherwise ->
          Left
            ( "active good "
                <> showText goodId
                <> " moved from "
                <> showText existingWorldId
                <> " to "
                <> showText worldId
            )
    Nothing -> pure state { activeGoods = Map.insert goodId worldId (activeGoods state) }

handleConsume ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  ConsumeState ->
  Object ->
  Either Text ConsumeState
handleConsume players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  goodId <- intValue "consume good_id" =<< field "good_id" args
  (worldName, stateWithoutGood) <- consumeGood goodId state
  case optionalField "world_id" args of
    Just worldIdValue -> do
      powerId <- intValue "consume world_id" worldIdValue
      powerCard <- lookupKnownCardName (cardIndex stateWithoutGood) powerId
      powerInfo <- lookupCardInfo cardInfosByName powerCard
      pid <- consumePlayer players powerId args stateWithoutGood
      if cardTypeHasGoodForSettleCost powerInfo && cursorPhase (phaseCursor stateWithoutGood) == Settle
        then emitImmediate players eventIx pid (ChooseGood [worldName]) stateWithoutGood
        else
          updateConsumeBlock
            eventIx
            pid
            powerCard
            ( if cardTypeHasConsumeForSell powerInfo
                then ConsumeTrade worldName
                else ConsumeGood [worldName]
            )
            stateWithoutGood
    Nothing -> do
      pid <- PlayerId <$> (intValue "consume player_id" =<< field "player_id" args)
      _ <- lookupPlayer players pid
      emitImmediate players eventIx pid (ChooseTrade worldName) stateWithoutGood

consumePlayer :: [Player] -> Int -> Object -> ConsumeState -> Either Text PlayerId
consumePlayer players powerId args state =
  case optionalField "player_id" args of
    Just pidValue -> do
      pid <- PlayerId <$> intValue "consume player_id" pidValue
      _ <- lookupPlayer players pid
      pure pid
    Nothing ->
      case Map.lookup powerId (knownCardOwners (cardIndex state)) of
        Just owner -> pure owner
        Nothing -> Left ("consume power owner unknown for " <> showText powerId)

consumeGood :: Int -> ConsumeState -> Either Text (Text, ConsumeState)
consumeGood goodId state =
  case Map.lookup goodId (activeGoods state) of
    Nothing -> Left ("consume of inactive good " <> showText goodId)
    Just worldId -> do
      worldName <- lookupKnownCardName (cardIndex state) worldId
      pure (worldName, state { activeGoods = Map.delete goodId (activeGoods state) })

handleConsumePrestige :: Int -> ConsumeState -> Object -> Either Text ConsumeState
handleConsumePrestige eventIx state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "consumeprestige player_id" =<< field "player_id" args)
  power <- canonicalCardName <$> (textValue "consumeprestige world_name" =<< field "world_name" args)
  updateConsumeBlock eventIx pid power ConsumeNoAnswer state

handleConsumeCard :: Map Text CardTypeInfo -> Int -> ConsumeState -> Object -> Either Text ConsumeState
handleConsumeCard cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "consumecard player_id" =<< field "player_id" args)
  power <- canonicalCardName <$> (textValue "consumecard world_name" =<< field "world_name" args)
  powerInfo <- lookupCardInfo cardInfosByName power
  let pending = Map.lookup pid (pendingDiscards state)
      stateWithoutPending = state { pendingDiscards = Map.delete pid (pendingDiscards state) }
  if cardTypeHasDiscardPrestige powerInfo
    then pure stateWithoutPending
    else
      updateConsumeBlock
        eventIx
        pid
        power
        (maybe ConsumeNoAnswer (ConsumeHand . pendingDiscardCards) pending)
        stateWithoutPending

handleDrawCardsDef :: [Player] -> Map Text CardTypeInfo -> Int -> ConsumeState -> Object -> Either Text ConsumeState
handleDrawCardsDef players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player_name" args of
    Nothing -> pure state
    Just playerNameValue -> do
      playerName <- textValue "drawCards_def player_name" playerNameValue
      player <- lookupPlayerByName players playerName
      let pid = playerId player
      stateWithoutLoggedDraw <-
        case optionalField "card_nbr" args of
          Nothing -> pure state
          Just countValue -> do
            count <- intValue "drawCards_def card_nbr" countValue
            pure (flushLoggedDraw pid count state)
      case optionalField "card_name" args of
        Nothing -> pure stateWithoutLoggedDraw
        Just sourceValue -> do
          sourceName <- canonicalCardName <$> textValue "drawCards_def card_name" sourceValue
          sourceInfo <- lookupCardInfo cardInfosByName sourceName
          if cardTypeHasPhase4Draw sourceInfo
            then updateConsumeBlock eventIx pid sourceName ConsumeNoAnswer stateWithoutLoggedDraw
            else pure stateWithoutLoggedDraw

handleDiscard :: ConsumeState -> Object -> Either Text ConsumeState
handleDiscard state notification =
  if cursorPhase (phaseCursor state) /= Consume
    then pure state
    else do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      case takeQueuedDrawCards cardIds (queuedDraws state) of
        Just (owner, remainingQueuedDraws) ->
          pure state
            { queuedDraws = remainingQueuedDraws
            , pendingGamblingDiscards =
                Map.insert owner (PendingDiscard cardIds cards) (pendingGamblingDiscards state)
            }
        Nothing -> do
          owner <- discardOwner (cardIndex state) cardIds
          case Map.lookup owner (pendingDiscards state) of
            Nothing ->
              pure state
                { pendingDiscards = Map.insert owner (PendingDiscard cardIds cards) (pendingDiscards state)
                }
            Just pending ->
              Left
                ( "new consume discard "
                    <> Text.intercalate ", " cards
                    <> " before pending discard resolved for player "
                    <> showText (unPlayerId owner)
                    <> ": "
                    <> Text.intercalate ", " (pendingDiscardCards pending)
                )

handleGambling :: ConsumeState -> Object -> Either Text ConsumeState
handleGambling state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "gambling player_id" =<< field "player_id" args)
  flipped <- canonicalCardName <$> (textValue "gambling card_name" =<< field "card_name" args)
  case Map.lookup pid (pendingGamblingDiscards state) of
    Just transient -> do
      unlessSingleMatching "Gambling discard" flipped transient
      pure state { pendingGamblingDiscards = Map.delete pid (pendingGamblingDiscards state) }
    Nothing ->
      case Map.lookup pid (pendingDiscards state) of
        Just pending
          | singlePendingCardMatches flipped pending ->
              pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }
        _ -> pure state

queueLoggedDraw :: ConsumeState -> Object -> Either Text ConsumeState
queueLoggedDraw state notification = do
  cards <- cardsFromNotification "drawCards" notification
  pid <- cardsPlayerId "drawCards" cards
  cardIds <- traverse cardId cards
  pure state { queuedDraws = queuedDraws state <> [QueuedDraw pid cardIds] }

flushLoggedDraw :: PlayerId -> Int -> ConsumeState -> ConsumeState
flushLoggedDraw pid count state =
  state { queuedDraws = dropFirstMatchingDraw [] (queuedDraws state) }
  where
    dropFirstMatchingDraw seen [] = seen
    dropFirstMatchingDraw seen (draw : rest)
      | queuedDrawPlayer draw == pid && length (queuedDrawIds draw) == count = seen <> rest
      | otherwise = dropFirstMatchingDraw (seen <> [draw]) rest

takeQueuedDrawCards :: [Int] -> [QueuedDraw] -> Maybe (PlayerId, [QueuedDraw])
takeQueuedDrawCards cardIds = go []
  where
    go _ [] = Nothing
    go seen (draw : rest)
      | all (`elem` queuedDrawIds draw) cardIds =
          let remainingIds = filter (`notElem` cardIds) (queuedDrawIds draw)
              remainingDraws =
                seen
                  <> [draw { queuedDrawIds = remainingIds } | not (null remainingIds)]
                  <> rest
           in Just (queuedDrawPlayer draw, remainingDraws)
      | otherwise = go (seen <> [draw]) rest

updateConsumeBlock :: Int -> PlayerId -> Text -> ConsumeAnswer -> ConsumeState -> Either Text ConsumeState
updateConsumeBlock eventIx pid power answer state = do
  order <- afterWindfallFlushOrder <$> cursorChoiceOrder (phaseCursor state) eventIx
  roundIndex <- currentRoundValue state
  let key = (roundIndex, pid, power)
      newBlock = ConsumeBlock pid power order answer
  case Map.lookup key (consumeBlocks state) of
    Nothing -> pure state { consumeBlocks = Map.insert key newBlock (consumeBlocks state) }
    Just oldBlock -> do
      mergedAnswer <- mergeConsumeAnswer power (consumeBlockAnswer oldBlock) answer
      pure state
        { consumeBlocks =
            Map.insert key oldBlock { consumeBlockOrder = order, consumeBlockAnswer = mergedAnswer } (consumeBlocks state)
        }

mergeConsumeAnswer :: Text -> ConsumeAnswer -> ConsumeAnswer -> Either Text ConsumeAnswer
mergeConsumeAnswer _ old ConsumeNoAnswer = pure old
mergeConsumeAnswer _ ConsumeNoAnswer new = pure new
mergeConsumeAnswer _ (ConsumeGood oldGoods) (ConsumeGood newGoods) = pure (ConsumeGood (oldGoods <> newGoods))
mergeConsumeAnswer power old new =
  Left
    ( "mixed consume answers for "
        <> power
        <> ": "
        <> showText old
        <> " then "
        <> showText new
    )

currentRoundValue :: ConsumeState -> Either Text Int
currentRoundValue state =
  case cursorRound (phaseCursor state) of
    Just roundIndex -> pure roundIndex
    Nothing -> Left "consume choice before first round"

emitImmediate :: [Player] -> Int -> PlayerId -> KeldonChoice -> ConsumeState -> Either Text ConsumeState
emitImmediate players eventIx pid choice state = do
  player <- lookupPlayer players pid
  order <- afterWindfallFlushOrder <$> cursorChoiceOrder (phaseCursor state) eventIx
  let seat = playerSeat player
      script = choiceScriptAt order seat [Choice Optional seat choice]
  pure state { consumeScript = consumeScript state `appendScript` script }

afterWindfallFlushOrder :: ChoiceOrder -> ChoiceOrder
afterWindfallFlushOrder (ChoiceOrder parts) =
  ChoiceOrder (parts <> [2])

consumeBlocksScript :: [Player] -> ConsumeState -> Either Text KeldonScript
consumeBlocksScript players state =
  foldl appendScript emptyScript <$> traverse blockScript (Map.elems (consumeBlocks state))
  where
    blockScript block = do
      player <- lookupPlayer players (consumeBlockPlayer block)
      let seat = playerSeat player
          lines_ =
            Choice Optional seat (ChooseConsume (consumeBlockPower block) (-1))
              : answerLines seat (consumeBlockAnswer block)
      pure (choiceScriptAt (consumeBlockOrder block) seat lines_)

answerLines :: Seat -> ConsumeAnswer -> [ScriptLine]
answerLines seat answer =
  case answer of
    ConsumeNoAnswer -> []
    ConsumeGood worlds -> [Choice Optional seat (ChooseGood worlds)]
    ConsumeTrade world -> [Choice Optional seat (ChooseTrade world)]
    ConsumeHand cards -> [Choice Optional seat (ChooseConsumeHand cards)]

ensureNoPendingGambling :: ConsumeState -> Either Text ()
ensureNoPendingGambling state =
  case Map.toList (pendingGamblingDiscards state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved Gambling discards: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid) <> "=" <> Text.intercalate "/" (pendingDiscardCards discard)
              | (pid, discard) <- pending
              ]
        )

ensureNoPendingDiscards :: ConsumeState -> Either Text ()
ensureNoPendingDiscards state =
  case Map.toList (pendingDiscards state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved consume discards: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid) <> "=" <> Text.intercalate "/" (pendingDiscardCards discard)
              | (pid, discard) <- pending
              ]
        )

unlessSingleMatching :: Text -> Text -> PendingDiscard -> Either Text ()
unlessSingleMatching label flipped pending =
  case pendingDiscardCards pending of
    [card]
      | canonicalCardName card == flipped -> pure ()
    cards ->
      Left
        ( label
            <> " "
            <> Text.intercalate ", " cards
            <> " does not match flipped card "
            <> flipped
        )

singlePendingCardMatches :: Text -> PendingDiscard -> Bool
singlePendingCardMatches flipped pending =
  case pendingDiscardCards pending of
    [card] -> canonicalCardName card == flipped
    _ -> False

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

lookupPlayerByName :: [Player] -> Text -> Either Text Player
lookupPlayerByName players name =
  case filter ((== name) . playerName) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> name)
    _ -> Left ("duplicate player " <> name)

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

showText :: Show a => a -> Text
showText = Text.pack . show
