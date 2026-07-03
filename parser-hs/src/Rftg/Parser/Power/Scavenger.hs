{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Power.Scavenger
  ( parseScavengerChoices
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
  , objectValues
  , optionalField
  , expectObject
  , textValue
  )
import Rftg.Bga.State
  ( BgaPhase (..)
  , bgaPhaseOrder
  , bgaStateIsNewActionRound
  , bgaStatePhaseOrder
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
import Rftg.Parser.Common
  ( cardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )

data ScavengerState = ScavengerState
  { currentRound :: Maybe Int
  , phaseOrder :: Int
  , tableauByPlayer :: Map PlayerId [Text]
  , scavengerScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseScavengerChoices :: Value -> Either Text KeldonScript
parseScavengerChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  notifications <- notificationObjects root
  finalState <-
    foldM
      (scavengerStep players cardTypes)
      emptyScavengerState
      (zip [0 :: Int ..] notifications)
  pure (scavengerScript finalState)

emptyScavengerState :: ScavengerState
emptyScavengerState = ScavengerState
  { currentRound = Nothing
  , phaseOrder = 0
  , tableauByPlayer = Map.empty
  , scavengerScript = emptyScript
  }

scavengerStep ::
  [Player] ->
  Map Int Text ->
  ScavengerState ->
  (Int, Object) ->
  Either Text ScavengerState
scavengerStep players cardTypes state (eventIx, notification) =
  case notificationType notification of
    "gameStateChange" -> handleGameState state notification
    "showTableau" -> handleShowTableau cardTypes state notification
    "playcard" -> handlePlayCard cardTypes state notification
    "scavengerUpdate" -> handleScavengerSave players cardTypes eventIx state notification
    "scavengeFromExplore" -> handleScavengerSave players cardTypes eventIx state notification
    _ -> pure state

handleGameState :: ScavengerState -> Object -> Either Text ScavengerState
handleGameState state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState
      | bgaStateIsNewActionRound bgaState ->
          pure state { currentRound = Just (nextRound state), phaseOrder = bgaPhaseOrder BgaAction }
      | otherwise ->
          case bgaStatePhaseOrder bgaState of
            Nothing -> pure state
            Just order -> pure state { phaseOrder = order }

nextRound :: ScavengerState -> Int
nextRound state =
  case currentRound state of
    Nothing -> 0
    Just n -> n + 1

handleShowTableau :: Map Int Text -> ScavengerState -> Object -> Either Text ScavengerState
handleShowTableau cardTypes state notification = do
  args <- objectField "args" notification
  cardsObject <- objectField "cards" args
  entries <- traverse startWorldEntry (objectValues cardsObject)
  pure state { tableauByPlayer = appendTableauEntries entries (tableauByPlayer state) }
  where
    startWorldEntry value = do
      obj <- expectObject "showTableau card" value
      pid <- PlayerId <$> (intValue "showTableau location_arg" =<< field "location_arg" obj)
      name <- cardName cardTypes value
      pure (pid, name)

handlePlayCard :: Map Int Text -> ScavengerState -> Object -> Either Text ScavengerState
handlePlayCard cardTypes state notification = do
  args <- objectField "args" notification
  case (optionalField "player" args, optionalField "card" args) of
    (Just playerValue, Just cardValue) -> do
      pid <- PlayerId <$> intValue "playcard player" playerValue
      name <- cardName cardTypes cardValue
      pure state { tableauByPlayer = appendTableauEntry pid name (tableauByPlayer state) }
    _ -> pure state

handleScavengerSave ::
  [Player] ->
  Map Int Text ->
  Int ->
  ScavengerState ->
  Object ->
  Either Text ScavengerState
handleScavengerSave players cardTypes eventIx state notification =
  case currentRound state of
    Nothing -> pure state
    Just roundIndex -> do
      args <- objectField "args" notification
      case optionalField "card" args of
        Nothing -> pure state
        Just cardValue -> do
          name <- cardName cardTypes cardValue
          owner <- scavengerOwner players state args cardValue
          let line = Choice Optional (playerSeat owner) (ChooseSave name)
              script =
                choiceScriptAt
                  (ChoiceOrder [roundIndex, phaseOrder state, eventIx])
                  (playerSeat owner)
                  [line]
          pure state { scavengerScript = scavengerScript state `appendScript` script }

scavengerOwner :: [Player] -> ScavengerState -> Object -> Value -> Either Text Player
scavengerOwner players state args cardValue =
  case optionalField "player_name" args of
    Just playerNameValue -> do
      playerName <- textValue "scavengerUpdate player_name" playerNameValue
      lookupPlayerByName players playerName
    Nothing ->
      case cardOwnerFromLocation cardValue of
        Just pid -> lookupPlayer players pid
        Nothing -> ownerWithGalacticScavengers players state

cardOwnerFromLocation :: Value -> Maybe PlayerId
cardOwnerFromLocation value = do
  obj <- either (const Nothing) Just (expectObject "scavenger card" value)
  locationValue <- optionalField "location_arg" obj
  pid <- either (const Nothing) (Just . PlayerId) (intValue "scavenger card location_arg" locationValue)
  if pid == PlayerId 0 then Nothing else Just pid

ownerWithGalacticScavengers :: [Player] -> ScavengerState -> Either Text Player
ownerWithGalacticScavengers players state =
  case candidates of
    [player] -> pure player
    [] -> Left "Scavenger save has no Galactic Scavengers owner"
    _ -> Left "Scavenger save has multiple Galactic Scavengers owners"
  where
    candidates =
      [ player
      | player <- players
      , "Galactic Scavengers" `elem` Map.findWithDefault [] (playerId player) (tableauByPlayer state)
      ]

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

appendTableauEntries :: [(PlayerId, Text)] -> Map PlayerId [Text] -> Map PlayerId [Text]
appendTableauEntries entries tableaus =
  foldl (\acc (pid, name) -> appendTableauEntry pid name acc) tableaus entries

appendTableauEntry :: PlayerId -> Text -> Map PlayerId [Text] -> Map PlayerId [Text]
appendTableauEntry pid name =
  Map.alter appendName pid
  where
    appendName Nothing = Just [name]
    appendName (Just names) = Just (names <> [name])

showText :: Show a => a -> Text
showText = Text.pack . show
