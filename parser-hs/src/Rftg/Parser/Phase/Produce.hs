{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Produce
  ( parseProduceChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , field
  , intValue
  , objectField
  , optionalField
  , expectObject
  , valueText
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
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
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parsePlayers
  )

data WindfallItem = WindfallItem
  { windfallRound :: Int
  , windfallWorld :: Text
  , windfallReason :: Maybe Text
  , windfallSeq :: Int
  }
  deriving stock (Eq, Show)

data PendingDiscard = PendingDiscard
  { pendingDiscardIds :: [Int]
  , pendingDiscardCards :: [Text]
  }
  deriving stock (Eq, Show)

data ProduceState = ProduceState
  { currentRound :: Maybe Int
  , phaseOrder :: Int
  , cardIndex :: CardIndex
  , activeGoods :: Map Int Int
  , windfallBuffers :: Map PlayerId [WindfallItem]
  , pendingDiscards :: Map PlayerId PendingDiscard
  , produceScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseProduceChoices :: Value -> Either Text KeldonScript
parseProduceChoices rootValue = do
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
      (produceStep players cardInfosByName cardTypes)
      (emptyProduceState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  finalState <- flushAllWindfalls players (length notifications) walked
  pure (produceScript finalState)

emptyProduceState :: CardIndex -> ProduceState
emptyProduceState startingCardIndex = ProduceState
  { currentRound = Nothing
  , phaseOrder = 0
  , cardIndex = startingCardIndex
  , activeGoods = Map.empty
  , windfallBuffers = Map.empty
  , pendingDiscards = Map.empty
  , produceScript = emptyScript
  }

produceStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  ProduceState ->
  (Int, Object) ->
  Either Text ProduceState
produceStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" ->
      handleGameState players eventIx stateWithCards notification
    "goodproduction" ->
      handleGoodProduction players cardInfosByName eventIx stateWithCards notification
    "consume" ->
      handleConsume players eventIx stateWithCards notification
    "discard" ->
      handleDiscard players eventIx stateWithCards notification
    "gambling" ->
      flushNotificationPlayer players eventIx stateWithCards notification
    _ -> pure stateWithCards

handleConsume :: [Player] -> Int -> ProduceState -> Object -> Either Text ProduceState
handleConsume players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "good_id" args of
    Nothing -> flushNotificationPlayer players eventIx state notification
    Just goodIdValue -> do
      goodId <- intValue "consume good_id" goodIdValue
      case Map.lookup goodId (activeGoods state) of
        Nothing -> Left ("consume of inactive good " <> showText goodId)
        Just _ -> flushNotificationPlayer players eventIx state { activeGoods = Map.delete goodId (activeGoods state) } notification

handleGameState :: [Player] -> Int -> ProduceState -> Object -> Either Text ProduceState
handleGameState players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure state
    Just idValue -> do
      stateId <- intValue "gameStateChange id" idValue
      stateForTransition <-
        if shouldFlushBeforeState state stateId
          then do
            checked <- ensureNoPendingDiscards state
            flushAllWindfalls players eventIx checked
          else pure state
      pure (applyGameState stateId stateForTransition)

applyGameState :: Int -> ProduceState -> ProduceState
applyGameState stateId state =
  case stateId of
    10 -> state { currentRound = Just (nextRound state), phaseOrder = 0 }
    _ -> state { phaseOrder = phaseOrderFor stateId (phaseOrder state) }

shouldFlushBeforeState :: ProduceState -> Int -> Bool
shouldFlushBeforeState state stateId =
  stateId == 10 || (phaseOrder state == 5 && phaseOrderFor stateId (phaseOrder state) /= 5)

nextRound :: ProduceState -> Int
nextRound state =
  case currentRound state of
    Nothing -> 0
    Just n -> n + 1

phaseOrderFor :: Int -> Int -> Int
phaseOrderFor stateId current =
  case stateId of
    20 -> 1
    21 -> 1
    30 -> 2
    31 -> 2
    230 -> 2
    231 -> 2
    311 -> 2
    40 -> 3
    41 -> 3
    42 -> 3
    43 -> 3
    241 -> 3
    242 -> 3
    341 -> 3
    342 -> 3
    442 -> 3
    542 -> 3
    50 -> 4
    51 -> 4
    52 -> 4
    60 -> 5
    61 -> 5
    62 -> 5
    69 -> 5
    70 -> 6
    71 -> 6
    98 -> 9
    99 -> 9
    100 -> 9
    _ -> current

handleGoodProduction ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  ProduceState ->
  Object ->
  Either Text ProduceState
handleGoodProduction players cardInfosByName eventIx state notification = do
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
    Nothing -> do
      let stateWithGood = state { activeGoods = Map.insert goodId worldId (activeGoods state) }
      worldName <- lookupKnownCardName (cardIndex stateWithGood) worldId
      worldInfo <- lookupCardInfo cardInfosByName worldName
      if not (cardIsWindfall worldInfo)
        then pure stateWithGood
        else case currentRound stateWithGood of
          Nothing -> pure stateWithGood
          Just roundIndex -> do
            discardProduce <- discardProduceSource cardInfosByName stateWithGood args
            case discardProduce of
              Just source -> emitDiscardProduce players eventIx stateWithGood source worldName
              Nothing -> do
                owner <- productionOwner players stateWithGood worldId args
                let reason = valueText <$> optionalField "windfallreason" args
                    item =
                      WindfallItem
                        { windfallRound = roundIndex
                        , windfallWorld = worldName
                        , windfallReason = reason
                        , windfallSeq = bufferLength owner stateWithGood
                        }
                pure stateWithGood
                  { windfallBuffers =
                      Map.alter (appendBuffer item) owner (windfallBuffers stateWithGood)
                  }

cardIsWindfall :: CardTypeInfo -> Bool
cardIsWindfall info =
  "windfall" `Set.member` cardTypeCategories info

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

data DiscardProduceSource = DiscardProduceSource
  { discardProduceOwner :: PlayerId
  , discardProduceName :: Text
  }
  deriving stock (Eq, Show)

discardProduceSource :: Map Text CardTypeInfo -> ProduceState -> Object -> Either Text (Maybe DiscardProduceSource)
discardProduceSource cardInfosByName state args =
  case optionalField "windfallreason" args of
    Nothing -> pure Nothing
    Just reasonValue
      | valueText reasonValue == "phase" -> pure Nothing
      | otherwise -> do
          sourceId <- intValue "goodproduction windfallreason" reasonValue
          sourceName <- lookupKnownCardName (cardIndex state) sourceId
          sourceInfo <- lookupCardInfo cardInfosByName sourceName
          if not (cardTypeHasWindfallProduceIfDiscard sourceInfo)
            then pure Nothing
            else do
              owner <- lookupCardOwner sourceId state
              pure (Just (DiscardProduceSource owner sourceName))

lookupCardOwner :: Int -> ProduceState -> Either Text PlayerId
lookupCardOwner cardId_ state =
  case Map.lookup cardId_ (knownCardOwners (cardIndex state)) of
    Just owner -> pure owner
    Nothing -> Left ("card owner unknown for " <> showText cardId_)

emitDiscardProduce ::
  [Player] ->
  Int ->
  ProduceState ->
  DiscardProduceSource ->
  Text ->
  Either Text ProduceState
emitDiscardProduce players eventIx state source worldName = do
  player <- lookupPlayer players (discardProduceOwner source)
  pending <- pendingDiscardFor source state
  discard <- singlePendingDiscard source pending
  roundIndex <- currentRoundValue state
  let order = ChoiceOrder [roundIndex, phaseOrder state, eventIx]
      line seat choice = Choice Optional seat choice
      script =
        choiceScriptAt
          order
          (playerSeat player)
          [ line (playerSeat player) (ChooseProduce (discardProduceName source) (-1))
          , line (playerSeat player) (ChooseDiscardProduce discard worldName)
          ]
  pure state
    { pendingDiscards = Map.delete (discardProduceOwner source) (pendingDiscards state)
    , produceScript = produceScript state `appendScript` script
    }

pendingDiscardFor :: DiscardProduceSource -> ProduceState -> Either Text PendingDiscard
pendingDiscardFor source state =
  case Map.lookup (discardProduceOwner source) (pendingDiscards state) of
    Just pending -> pure pending
    Nothing ->
      Left
        ( "discard-produce source "
            <> discardProduceName source
            <> " has no pending discard"
        )

singlePendingDiscard :: DiscardProduceSource -> PendingDiscard -> Either Text Text
singlePendingDiscard source pending =
  case pendingDiscardCards pending of
    [card] -> pure card
    cards ->
      Left
        ( "discard-produce source "
            <> discardProduceName source
            <> " has "
            <> showText (length cards)
            <> " pending discards: "
            <> Text.intercalate ", " cards
        )

currentRoundValue :: ProduceState -> Either Text Int
currentRoundValue state =
  case currentRound state of
    Just roundIndex -> pure roundIndex
    Nothing -> Left "discard-produce before first round"

handleDiscard :: [Player] -> Int -> ProduceState -> Object -> Either Text ProduceState
handleDiscard players eventIx state notification =
  if phaseOrder state /= 5
    then flushAllWindfalls players eventIx state
    else do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      owner <- discardOwner (cardIndex state) cardIds
      flushed <- flushPlayerWindfalls players eventIx owner state
      case Map.lookup owner (pendingDiscards flushed) of
        Nothing ->
          pure flushed
            { pendingDiscards =
                Map.insert owner (PendingDiscard cardIds cards) (pendingDiscards flushed)
            }
        Just pending ->
          Left
            ( "new Produce discard "
                <> Text.intercalate ", " cards
                <> " before pending discard resolved for player "
                <> showText (unPlayerId owner)
                <> ": "
                <> Text.intercalate ", " (pendingDiscardCards pending)
            )

ensureNoPendingDiscards :: ProduceState -> Either Text ProduceState
ensureNoPendingDiscards state =
  case Map.toList (pendingDiscards state) of
    [] -> pure state
    pending ->
      Left
        ( "unresolved Produce discards: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid) <> "=" <> Text.intercalate "/" (pendingDiscardCards discard)
              | (pid, discard) <- pending
              ]
        )

productionOwner :: [Player] -> ProduceState -> Int -> Object -> Either Text PlayerId
productionOwner players state worldId args = do
  producedBy <- producedByPlayer args
  case (knownOwner, producedBy) of
    (Just owner, Just pid)
      | owner == pid -> pure owner
      | otherwise ->
          Left
            ( "goodproduction owner mismatch for world "
                <> showText worldId
                <> ": index has "
                <> showText (unPlayerId owner)
                <> ", produced_by has "
                <> showText (unPlayerId pid)
            )
    (Just owner, Nothing) -> pure owner
    (Nothing, Just pid) -> do
      _ <- lookupPlayer players pid
      pure pid
    (Nothing, Nothing) ->
      Left ("goodproduction cannot attribute world " <> showText worldId)
  where
    knownOwner = Map.lookup worldId (knownCardOwners (cardIndex state))

producedByPlayer :: Object -> Either Text (Maybe PlayerId)
producedByPlayer args =
  case optionalField "produced_by" args of
    Nothing -> pure Nothing
    Just value -> Just . PlayerId <$> intValue "goodproduction produced_by" value

appendBuffer :: WindfallItem -> Maybe [WindfallItem] -> Maybe [WindfallItem]
appendBuffer item Nothing = Just [item]
appendBuffer item (Just items) = Just (items <> [item])

bufferLength :: PlayerId -> ProduceState -> Int
bufferLength pid state =
  length (Map.findWithDefault [] pid (windfallBuffers state))

flushNotificationPlayer :: [Player] -> Int -> ProduceState -> Object -> Either Text ProduceState
flushNotificationPlayer players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player_id" args of
    Nothing -> pure state
    Just pidValue -> do
      pid <- PlayerId <$> intValue "notification player_id" pidValue
      flushPlayerWindfalls players eventIx pid state

flushAllWindfalls :: [Player] -> Int -> ProduceState -> Either Text ProduceState
flushAllWindfalls players eventIx state =
  foldM
    (\current pid -> flushPlayerWindfalls players eventIx pid current)
    state
    (Map.keys (windfallBuffers state))

flushPlayerWindfalls :: [Player] -> Int -> PlayerId -> ProduceState -> Either Text ProduceState
flushPlayerWindfalls players eventIx pid state =
  case Map.lookup pid (windfallBuffers state) of
    Nothing -> pure state
    Just [] -> pure state { windfallBuffers = Map.delete pid (windfallBuffers state) }
    Just items -> do
      player <- lookupPlayer players pid
      let script =
            foldl
              appendScript
              emptyScript
              (fmap (windfallScript player (phaseOrder state) eventIx) (List.sortOn windfallSortKey items))
      pure state
        { windfallBuffers = Map.delete pid (windfallBuffers state)
        , produceScript = produceScript state `appendScript` script
        }

windfallSortKey :: WindfallItem -> (Int, Int)
windfallSortKey item =
  (reasonRank (windfallReason item), windfallSeq item)

windfallScript :: Player -> Int -> Int -> WindfallItem -> KeldonScript
windfallScript player flushPhaseOrder eventIx item =
  choiceScriptAt
    (ChoiceOrder [windfallRound item, flushPhaseOrder, eventIx, reasonRank (windfallReason item), windfallSeq item])
    (playerSeat player)
    [Choice Optional (playerSeat player) (ChooseWindfall (windfallWorld item))]

reasonRank :: Maybe Text -> Int
reasonRank (Just "phase") = 0
reasonRank _ = 1

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

showText :: Show a => a -> Text
showText = Text.pack . show
