{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Explore
  ( parseExploreChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , intValue
  , objectField
  , optionalField
  , expectObject
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
  , cardsFromNotification
  , cardsPlayerId
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( cardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )

data PhaseCursor
  = InExplore
  | OutsideExplore
  deriving stock (Eq, Show)

data ExploreState = ExploreState
  { currentRound :: Maybe Int
  , phaseCursor :: PhaseCursor
  , exploredByPlayer :: Map PlayerId [Text]
  , cardIndex :: CardIndex
  , exploreScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseExploreChoices :: Value -> Either Text KeldonScript
parseExploreChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  let conceded = any ((== "playerConcedeGame") . notificationType) notifications
  finalState <-
    foldM
      (exploreStep players cardTypes)
      (emptyExploreState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  assertNoPendingExplores conceded finalState
  pure (exploreScript finalState)

emptyExploreState :: CardIndex -> ExploreState
emptyExploreState startingCardIndex = ExploreState
  { currentRound = Nothing
  , phaseCursor = OutsideExplore
  , exploredByPlayer = Map.empty
  , cardIndex = startingCardIndex
  , exploreScript = emptyScript
  }

exploreStep ::
  [Player] ->
  Map Int Text ->
  ExploreState ->
  (Int, Object) ->
  Either Text ExploreState
exploreStep players cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "explored_choice" -> handleExploredChoice cardTypes stateWithCards notification
    "keepcards" -> handleKeepCards players cardTypes eventIx stateWithCards notification
    "discard" -> handleDiscard players eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: ExploreState -> Object -> Either Text ExploreState
handleGameState state notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure state
    Just idValue -> do
      stateId <- intValue "gameStateChange id" idValue
      case stateId of
        10 -> pure state { currentRound = Just (nextRound state), phaseCursor = OutsideExplore }
        20 -> pure state { phaseCursor = InExplore }
        21 -> pure state { phaseCursor = InExplore }
        _
          | isKnownNonExploreState stateId ->
              pure state { phaseCursor = OutsideExplore }
          | otherwise ->
              pure state

nextRound :: ExploreState -> Int
nextRound state =
  case currentRound state of
    Nothing -> 0
    Just n -> n + 1

isKnownNonExploreState :: Int -> Bool
isKnownNonExploreState stateId =
  stateId `elem`
    [ 3
    , 4
    , 5
    , 10
    , 11
    , 12
    , 19
    , 30
    , 31
    , 40
    , 41
    , 42
    , 43
    , 50
    , 51
    , 52
    , 60
    , 61
    , 62
    , 69
    , 70
    , 71
    , 98
    , 99
    , 100
    , 230
    , 231
    , 241
    , 242
    , 311
    , 341
    , 342
    , 442
    , 542
    ]

handleExploredChoice :: Map Int Text -> ExploreState -> Object -> Either Text ExploreState
handleExploredChoice cardTypes state notification = do
  cards <- cardsFromNotification "explored_choice" notification
  pid <- cardsPlayerId "explored_choice" cards
  names <- traverse (cardName cardTypes) cards
  pure state { exploredByPlayer = Map.insert pid names (exploredByPlayer state) }

handleKeepCards ::
  [Player] ->
  Map Int Text ->
  Int ->
  ExploreState ->
  Object ->
  Either Text ExploreState
handleKeepCards players cardTypes eventIx state notification = do
  cards <- cardsFromNotification "keepcards" notification
  pid <- cardsPlayerId "keepcards" cards
  kept <- traverse (cardName cardTypes) cards
  explored <- case Map.lookup pid (exploredByPlayer state) of
    Just names -> pure names
    Nothing -> Left ("keepcards for player " <> showText (unPlayerId pid) <> " without explored_choice")
  discards <- removeKeptCards explored kept
  emitDiscardChoice players eventIx pid discards
    state { exploredByPlayer = Map.delete pid (exploredByPlayer state) }

removeKeptCards :: [Text] -> [Text] -> Either Text [Text]
removeKeptCards =
  foldM removeOne
  where
    removeOne remaining kept =
      case break (== kept) remaining of
        (_, []) -> Left ("kept card was not explored: " <> kept)
        (before, _ : after) -> pure (before <> after)

handleDiscard :: [Player] -> Int -> ExploreState -> Object -> Either Text ExploreState
handleDiscard players eventIx state notification =
  case phaseCursor state of
    OutsideExplore -> pure state
    InExplore -> do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      names <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      owner <- discardOwner (cardIndex state) cardIds
      emitDiscardChoice players eventIx owner names state

emitDiscardChoice :: [Player] -> Int -> PlayerId -> [Text] -> ExploreState -> Either Text ExploreState
emitDiscardChoice players eventIx pid cards state = do
  player <- lookupPlayer players pid
  roundIndex <- currentRoundOrError state
  let line = Choice Required (playerSeat player) (ChooseDiscard cards)
      script = choiceScriptAt (ChoiceOrder [roundIndex, 1, eventIx]) (playerSeat player) [line]
  pure state { exploreScript = exploreScript state `appendScript` script }

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

currentRoundOrError :: ExploreState -> Either Text Int
currentRoundOrError state =
  case currentRound state of
    Just roundIndex -> pure roundIndex
    Nothing -> Left "Explore choice before first action round"

assertNoPendingExplores :: Bool -> ExploreState -> Either Text ()
assertNoPendingExplores allowIncomplete state =
  case Map.toList (exploredByPlayer state) of
    [] -> pure ()
    _ | allowIncomplete -> pure ()
    pending ->
      Left ("unresolved explored choices: " <> showText pending)

showText :: Show a => a -> Text
showText = Text.pack . show
