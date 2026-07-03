{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Place
  ( parsePlaceChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
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
  )
import Rftg.Bga.State
  ( BgaState
  , bgaStateIsDevelopMain
  , bgaStateIsNewActionRound
  , bgaStateIsSettleMain
  , bgaStateIsTerraformingEngineers
  , optionalBgaStateField
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
  ( CardIndex (..)
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , cardId
  , cardName
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parsePlayers
  )
import Rftg.Parser.Phase.Cursor
  ( PhaseCursor
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data BuildPhase = DevelopBuild | SettleBuild
  deriving stock (Eq, Ord, Show)

data PlaceState = PlaceState
  { phaseCursor :: PhaseCursor
  , currentBgaState :: Maybe BgaState
  , cardIndex :: CardIndex
  , developStep :: Int
  , settleStep :: Int
  , developPlays :: Map PlayerId Int
  , settlePlays :: Map PlayerId Int
  , pendingUpgrades :: Map PlayerId Text
  , discardedTableauCards :: Set Int
  , playedCards :: Set Int
  , placeScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parsePlaceChoices :: Value -> Either Text KeldonScript
parsePlaceChoices rootValue = do
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
      (placeStep players cardInfosByName cardTypes)
      (emptyPlaceState players startingCardIndex)
      (zip [0 :: Int ..] notifications)
  ensureNoPendingUpgrades walked
  pure (placeScript walked)

emptyPlaceState :: [Player] -> CardIndex -> PlaceState
emptyPlaceState players startingCardIndex = PlaceState
  { phaseCursor = initialPhaseCursor
  , currentBgaState = Nothing
  , cardIndex = startingCardIndex
  , developStep = 0
  , settleStep = 0
  , developPlays = zeroPlayerMap
  , settlePlays = zeroPlayerMap
  , pendingUpgrades = Map.empty
  , discardedTableauCards = Set.empty
  , playedCards = Set.empty
  , placeScript = emptyScript
  }
  where
    zeroPlayerMap = Map.fromList [(playerId player, 0) | player <- players]

placeStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  PlaceState ->
  (Int, Object) ->
  Either Text PlaceState
placeStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState players stateWithCards notification
    "discardfromtableau" -> handleDiscardFromTableau cardInfosByName stateWithCards notification
    "playcard" -> handlePlayCard players cardInfosByName cardTypes eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: [Player] -> PlaceState -> Object -> Either Text PlaceState
handleGameState players state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      let advanced = advanceForState players bgaState state
      pure advanced
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor advanced)
        , currentBgaState = Just bgaState
        }

advanceForState :: [Player] -> BgaState -> PlaceState -> PlaceState
advanceForState players bgaState state
  | bgaStateIsNewActionRound bgaState = resetRound players state
  | bgaStateIsDevelopMain bgaState = state { developStep = developStep state + 1 }
  | bgaStateIsSettleMain bgaState = state { settleStep = settleStep state + 1 }
  | otherwise = state

resetRound :: [Player] -> PlaceState -> PlaceState
resetRound players state =
  state
    { developStep = 0
    , settleStep = 0
    , developPlays = zeroPlayerMap
    , settlePlays = zeroPlayerMap
    }
  where
    zeroPlayerMap = Map.fromList [(playerId player, 0) | player <- players]

handleDiscardFromTableau :: Map Text CardTypeInfo -> PlaceState -> Object -> Either Text PlaceState
handleDiscardFromTableau cardInfosByName state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardInstanceId <- intValue "discardfromtableau card" cardValue
      if cardInstanceId `Set.member` discardedTableauCards state
        then pure state
        else do
          name <- lookupKnownCardName (cardIndex state) cardInstanceId
          owner <- lookupCardOwner cardInstanceId state
          info <- lookupCardInfo cardInfosByName name
          let stateWithDiscard =
                state { discardedTableauCards = Set.insert cardInstanceId (discardedTableauCards state) }
          if maybe False bgaStateIsTerraformingEngineers (currentBgaState state) && cardTypeType info == "world"
            then
              case Map.lookup owner (pendingUpgrades stateWithDiscard) of
                Nothing ->
                  pure stateWithDiscard
                    { pendingUpgrades = Map.insert owner name (pendingUpgrades stateWithDiscard)
                    }
                Just pending ->
                  Left
                    ( "new upgrade discard "
                        <> name
                        <> " before pending upgrade resolved for player "
                        <> showText (unPlayerId owner)
                        <> ": "
                        <> pending
                    )
            else pure stateWithDiscard

handlePlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  Int ->
  PlaceState ->
  Object ->
  Either Text PlaceState
handlePlayCard players cardInfosByName cardTypes eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "money" args of
    Nothing -> markPlayedIfNeeded cardTypes state args
    Just _ -> do
      cardValue <- field "card" args
      cardInstanceId <- cardId cardValue
      if cardInstanceId `Set.member` playedCards state
        then pure state
        else do
          pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
          player <- lookupPlayer players pid
          name <- cardName cardTypes cardValue
          info <- lookupCardInfo cardInfosByName name
          let stateWithPlayed = state { playedCards = Set.insert cardInstanceId (playedCards state) }
          case buildPhaseFor info of
            Nothing -> pure stateWithPlayed
            Just SettleBuild
              | Map.member pid (pendingUpgrades stateWithPlayed) ->
                  pure stateWithPlayed
                    { pendingUpgrades = Map.delete pid (pendingUpgrades stateWithPlayed)
                    }
            Just buildPhase -> emitPlace player eventIx buildPhase name stateWithPlayed

markPlayedIfNeeded :: Map Int Text -> PlaceState -> Object -> Either Text PlaceState
markPlayedIfNeeded cardTypes state args =
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardInstanceId <- cardId cardValue
      if cardInstanceId `Set.member` playedCards state
        then pure state
        else do
          _ <- cardName cardTypes cardValue
          pure state { playedCards = Set.insert cardInstanceId (playedCards state) }

buildPhaseFor :: CardTypeInfo -> Maybe BuildPhase
buildPhaseFor info =
  case cardTypeType info of
    "development" -> Just DevelopBuild
    "world" -> Just SettleBuild
    _ -> Nothing

emitPlace :: Player -> Int -> BuildPhase -> Text -> PlaceState -> Either Text PlaceState
emitPlace player eventIx buildPhase card state = do
  order <- cursorChoiceOrder (phaseCursor state) eventIx
  let pid = playerId player
      seat = playerSeat player
      skipped = skippedPlaces pid buildPhase state
      lines_ =
        replicate skipped (Choice Required seat (ChoosePlace Nothing))
          <> [Choice Required seat (ChoosePlace (Just card))]
      script = choiceScriptAt order seat lines_
  pure (recordPlaces pid buildPhase (skipped + 1) state)
    { placeScript = placeScript state `appendScript` script
    }

skippedPlaces :: PlayerId -> BuildPhase -> PlaceState -> Int
skippedPlaces pid buildPhase state =
  max 0 (stepFor buildPhase state - 1 - playsFor pid buildPhase state)

recordPlaces :: PlayerId -> BuildPhase -> Int -> PlaceState -> PlaceState
recordPlaces pid buildPhase count state =
  case buildPhase of
    DevelopBuild ->
      state { developPlays = Map.insert pid (playsFor pid buildPhase state + count) (developPlays state) }
    SettleBuild ->
      state { settlePlays = Map.insert pid (playsFor pid buildPhase state + count) (settlePlays state) }

stepFor :: BuildPhase -> PlaceState -> Int
stepFor DevelopBuild = developStep
stepFor SettleBuild = settleStep

playsFor :: PlayerId -> BuildPhase -> PlaceState -> Int
playsFor pid DevelopBuild state = Map.findWithDefault 0 pid (developPlays state)
playsFor pid SettleBuild state = Map.findWithDefault 0 pid (settlePlays state)

lookupCardOwner :: Int -> PlaceState -> Either Text PlayerId
lookupCardOwner cardInstanceId state =
  case Map.lookup cardInstanceId (knownCardOwners (cardIndex state)) of
    Just owner -> pure owner
    Nothing -> Left ("card owner unknown for " <> showText cardInstanceId)

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

ensureNoPendingUpgrades :: PlaceState -> Either Text ()
ensureNoPendingUpgrades state =
  case Map.toList (pendingUpgrades state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved place upgrades: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid) <> "=" <> oldWorld
              | (pid, oldWorld) <- pending
              ]
        )

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

showText :: Show a => a -> Text
showText = Text.pack . show
