{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Discard
  ( parseDiscardChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , objectField
  , expectObject
  )
import Rftg.Bga.State
  ( BgaPhase (..)
  , bgaStateHasPhase
  , bgaStateIsNewActionRound
  , bgaStateLeavesPhase
  , optionalBgaStateField
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
  ( CardIndex
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )

data PhaseCursor
  = InDiscard
  | OutsideDiscard
  deriving stock (Eq, Show)

data DiscardState = DiscardState
  { currentRound :: Maybe Int
  , phaseCursor :: PhaseCursor
  , cardIndex :: CardIndex
  , discardScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseDiscardChoices :: Value -> Either Text KeldonScript
parseDiscardChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  finalState <-
    foldM
      (discardStep players cardTypes)
      (emptyDiscardState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  pure (discardScript finalState)

emptyDiscardState :: CardIndex -> DiscardState
emptyDiscardState startingCardIndex = DiscardState
  { currentRound = Nothing
  , phaseCursor = OutsideDiscard
  , cardIndex = startingCardIndex
  , discardScript = emptyScript
  }

discardStep ::
  [Player] ->
  Map Int Text ->
  DiscardState ->
  (Int, Object) ->
  Either Text DiscardState
discardStep players cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "discard" -> handleDiscard players eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: DiscardState -> Object -> Either Text DiscardState
handleGameState state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState
      | bgaStateIsNewActionRound bgaState ->
          pure state { currentRound = Just (nextRound state), phaseCursor = OutsideDiscard }
      | bgaStateHasPhase BgaDiscard bgaState ->
          pure state { phaseCursor = InDiscard }
      | bgaStateLeavesPhase BgaDiscard bgaState ->
          pure state { phaseCursor = OutsideDiscard }
      | otherwise ->
          pure state

nextRound :: DiscardState -> Int
nextRound state =
  case currentRound state of
    Nothing -> 0
    Just n -> n + 1

handleDiscard :: [Player] -> Int -> DiscardState -> Object -> Either Text DiscardState
handleDiscard players eventIx state notification =
  case phaseCursor state of
    OutsideDiscard -> pure state
    InDiscard -> do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      names <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      owner <- discardOwner (cardIndex state) cardIds
      emitDiscardChoice players eventIx owner names state

emitDiscardChoice :: [Player] -> Int -> PlayerId -> [Text] -> DiscardState -> Either Text DiscardState
emitDiscardChoice players eventIx pid cards state = do
  player <- lookupPlayer players pid
  roundIndex <- currentRoundOrError state
  let line = Choice Required (playerSeat player) (ChooseDiscard cards)
      script = choiceScriptAt (ChoiceOrder [roundIndex, 6, eventIx]) (playerSeat player) [line]
  pure state { discardScript = discardScript state `appendScript` script }

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

currentRoundOrError :: DiscardState -> Either Text Int
currentRoundOrError state =
  case currentRound state of
    Just roundIndex -> pure roundIndex
    Nothing -> Left "Discard choice before first action round"

showText :: Show a => a -> Text
showText = Text.pack . show
