{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Power.DiscardPrestige
  ( parseDiscardPrestigeChoices
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
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  )
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , KeldonChoice (..)
  , KeldonScript
  , ScriptLine (..)
  , appendScript
  , choiceScriptAt
  , emptyScript
  )
import Rftg.Parser.CardIndex
  ( CardIndex
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
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

data DiscardPrestigeState = DiscardPrestigeState
  { phaseCursor :: PhaseCursor
  , activeSearch :: Bool
  , cardIndex :: CardIndex
  , pendingDiscards :: Map PlayerId PendingDiscard
  , discardPrestigeScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseDiscardPrestigeChoices :: Value -> Either Text KeldonScript
parseDiscardPrestigeChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypeInfos <- parseCardTypeInfos gamedatas
  let cardTypes = fmap cardTypeName cardTypeInfos
      cardInfosByName = Map.fromList [(cardTypeName info, info) | info <- Map.elems cardTypeInfos]
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  finalState <-
    foldM
      (discardPrestigeStep players cardInfosByName cardTypes)
      (emptyDiscardPrestigeState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  pure (discardPrestigeScript finalState)

emptyDiscardPrestigeState :: CardIndex -> DiscardPrestigeState
emptyDiscardPrestigeState startingCardIndex = DiscardPrestigeState
  { phaseCursor = initialPhaseCursor
  , activeSearch = False
  , cardIndex = startingCardIndex
  , pendingDiscards = Map.empty
  , discardPrestigeScript = emptyScript
  }

discardPrestigeStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  DiscardPrestigeState ->
  (Int, Object) ->
  Either Text DiscardPrestigeState
discardPrestigeStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "discard" -> handleDiscard stateWithCards notification
    "consumecard" -> handleConsumeCard players cardInfosByName eventIx stateWithCards notification
    "playcard" -> clearPlayCardPending stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: DiscardPrestigeState -> Object -> Either Text DiscardPrestigeState
handleGameState state notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure state
    Just idValue -> do
      stateId <- intValue "gameStateChange id" idValue
      let cursor = advancePhaseCursor stateId (phaseCursor state)
          searchActive = stateId == 201 || stateId == 202
          pending =
            if stateId == 10
              then Map.empty
              else pendingDiscards state
      pure state
        { phaseCursor = cursor
        , activeSearch = searchActive
        , pendingDiscards = pending
        }

handleDiscard :: DiscardPrestigeState -> Object -> Either Text DiscardPrestigeState
handleDiscard state notification =
  if not (capturesPendingDiscard state)
    then pure state
    else do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      names <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      owner <- discardOwner (cardIndex state) cardIds
      pure state
        { pendingDiscards =
            Map.insert owner (PendingDiscard cardIds names) (pendingDiscards state)
        }

capturesPendingDiscard :: DiscardPrestigeState -> Bool
capturesPendingDiscard state =
  case (cursorRound (phaseCursor state), cursorPhase (phaseCursor state)) of
    (Nothing, _) -> False
    (_, Explore) -> False
    (_, Discard) -> False
    _ -> not (activeSearch state)

handleConsumeCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  DiscardPrestigeState ->
  Object ->
  Either Text DiscardPrestigeState
handleConsumeCard players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "consumecard player_id" =<< field "player_id" args)
  powerCard <- canonicalCardName <$> (textValue "consumecard world_name" =<< field "world_name" args)
  powerInfo <- lookupCardInfo cardInfosByName powerCard
  if cardTypeHasDiscardPrestige powerInfo
    then emitDiscardPrestige players eventIx pid state
    else pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }

emitDiscardPrestige :: [Player] -> Int -> PlayerId -> DiscardPrestigeState -> Either Text DiscardPrestigeState
emitDiscardPrestige players eventIx pid state = do
  player <- lookupPlayer players pid
  pending <- pendingDiscardFor pid state
  card <-
    case pendingDiscardCards pending of
      [singleCard] -> pure singleCard
      cards ->
        Left
          ( "DISCARD_PRESTIGE expected one pending card for "
              <> showText (unPlayerId pid)
              <> ", got "
              <> showText (length cards)
          )
  order <- cursorChoiceOrder (phaseCursor state) eventIx
  let line = Choice Optional (playerSeat player) (ChooseDiscardPrestige card)
      script = choiceScriptAt order (playerSeat player) [line]
  pure state
    { pendingDiscards = Map.delete pid (pendingDiscards state)
    , discardPrestigeScript = discardPrestigeScript state `appendScript` script
    }

pendingDiscardFor :: PlayerId -> DiscardPrestigeState -> Either Text PendingDiscard
pendingDiscardFor pid state =
  case Map.lookup pid (pendingDiscards state) of
    Just pending -> pure pending
    Nothing -> Left ("DISCARD_PRESTIGE has no pending discard for " <> showText (unPlayerId pid))

clearPlayCardPending :: DiscardPrestigeState -> Object -> Either Text DiscardPrestigeState
clearPlayCardPending state notification = do
  args <- objectField "args" notification
  case optionalField "player" args of
    Nothing -> pure state
    Just pidValue -> do
      pid <- PlayerId <$> intValue "playcard player" pidValue
      pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }

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

showText :: Show a => a -> Text
showText = Text.pack . show
