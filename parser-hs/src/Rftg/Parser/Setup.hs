{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Setup
  ( gamedataFor
  , parseInitialStartWorlds
  , parseSetup
  , parseStartOptions
  , sortCards
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
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
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  )
import Rftg.Parser.Common
  ( cardId
  , cardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , KeldonScript
  , KeldonChoice (..)
  , ScriptLine (..)
  , appendScript
  , choiceScript
  , preambleScript
  )

parseSetup :: Value -> Either Text KeldonScript
parseSetup rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startOptions <- parseStartOptions players gamedatas cardTypes
  startWorlds <- parseInitialStartWorlds root cardTypes
  roundZeroDiscards <- parseRoundZeroDiscards players gamedatas cardTypes root
  roundZeroSaves <- parseRoundZeroSaves players cardTypes root
  startChoices <- parseStartChoices players startOptions startWorlds roundZeroDiscards roundZeroSaves
  pure $
    preambleScript (renderStartOptions players startOptions)
      `appendScript` mconcatScripts startChoices

parseStartOptions ::
  [Player] ->
  Object ->
  Map Int Text ->
  Either Text (Map PlayerId [Text])
parseStartOptions players gamedatas cardTypes = do
  options <- fmap (Map.fromList . catMaybes) $
    traverse playerStartOptions players
  let complete = not (Map.null options) && Map.size options == length players
  pure $ if complete then options else Map.empty
  where
    playerStartOptions player = do
      gamedata <- gamedataFor gamedatas (playerId player)
      let hidden = maybe [] objectValues (optionalObject "hiddentableau" gamedata)
      cards <- traverse (cardName cardTypes) (sortCards hidden)
      if length cards == 2
        then pure (Just (playerId player, cards))
        else pure Nothing

parseInitialStartWorlds :: Object -> Map Int Text -> Either Text (Map PlayerId Text)
parseInitialStartWorlds root cardTypes = do
  notifications <- notificationObjects root
  case List.find ((== "showTableau") . notificationType) notifications of
    Nothing -> pure Map.empty
    Just notification -> do
      args <- objectField "args" notification
      cardsObject <- objectField "cards" args
      fmap Map.fromList $
        traverse startWorldFromCard (objectValues cardsObject)
  where
    startWorldFromCard value = do
      obj <- expectObject "showTableau card" value
      pid <- PlayerId <$> (intValue "showTableau location_arg" =<< field "location_arg" obj)
      name <- cardName cardTypes value
      pure (pid, name)

parseRoundZeroDiscards ::
  [Player] ->
  Object ->
  Map Int Text ->
  Object ->
  Either Text (Map PlayerId [Text])
parseRoundZeroDiscards players gamedatas cardTypes root = do
  initialCards <- initialCardOwners players gamedatas cardTypes
  notifications <- notificationObjects root
  foldl (appendNotificationDiscard initialCards) (Right Map.empty)
    (takeSetupNotifications notifications)
  where
    appendNotificationDiscard initialCards acc notification = do
      discards <- acc
      if notificationType notification /= "discard"
        then pure discards
        else do
          cardIds <- discardCardIds notification
          discardCards <- traverse (lookupInitialCard initialCards) cardIds
          case discardCards of
            [] -> pure discards
            (owner, _) : rest
              | all ((== owner) . fst) rest ->
                  pure $ Map.alter (appendNames (fmap snd discardCards)) owner discards
              | otherwise ->
                  Left ("setup discard has mixed owners: " <> showText cardIds)

    appendNames newCards Nothing = Just newCards
    appendNames newCards (Just oldCards) = Just (oldCards <> newCards)

takeSetupNotifications :: [Object] -> [Object]
takeSetupNotifications =
  takeWhile ((/= "phase_choices") . notificationType)

discardCardIds :: Object -> Either Text [Int]
discardCardIds notification = do
  args <- objectField "args" notification
  cardsValue <- field "cards" args
  case cardsValue of
    Array values ->
      traverse (intValue "discard card id") (Vector.toList values)
    _ -> Left "discard.cards is not an array"

initialCardOwners ::
  [Player] ->
  Object ->
  Map Int Text ->
  Either Text (Map Int (PlayerId, Text))
initialCardOwners players gamedatas cardTypes =
  fmap Map.unions $ traverse playerInitialCards players
  where
    playerInitialCards player = do
      gamedata <- gamedataFor gamedatas (playerId player)
      handCards <- cardsFromZone gamedata "hand"
      hiddenCards <- cardsFromZone gamedata "hiddentableau"
      fmap Map.fromList $
        traverse (cardOwnerEntry (playerId player)) (handCards <> hiddenCards)

    cardsFromZone gamedata zone =
      pure $ maybe [] objectValues (optionalObject zone gamedata)

    cardOwnerEntry pid value = do
      cid <- cardId value
      name <- cardName cardTypes value
      pure (cid, (pid, name))

lookupInitialCard :: Map Int (PlayerId, Text) -> Int -> Either Text (PlayerId, Text)
lookupInitialCard initialCards cid =
  case Map.lookup cid initialCards of
    Just card -> pure card
    Nothing -> Left ("setup discard references unknown card instance " <> showText cid)

parseRoundZeroSaves ::
  [Player] ->
  Map Int Text ->
  Object ->
  Either Text (Map PlayerId [Text])
parseRoundZeroSaves players cardTypes root = do
  notifications <- notificationObjects root
  foldl appendNotificationSave (Right Map.empty) (takeSetupNotifications notifications)
  where
    playersByName =
      Map.fromList [(playerName player, playerId player) | player <- players]

    appendNotificationSave acc notification = do
      saves <- acc
      if notificationType notification /= "scavengerUpdate"
        then pure saves
        else do
          args <- objectField "args" notification
          playerName <- textValue "scavengerUpdate player_name" =<< field "player_name" args
          pid <- case Map.lookup playerName playersByName of
            Just found -> pure found
            Nothing -> Left ("scavengerUpdate has unknown player " <> playerName)
          cardValue <- field "card" args
          name <- cardName cardTypes cardValue
          pure $ Map.alter (appendSave name) pid saves

    appendSave name Nothing = Just [name]
    appendSave name (Just names) = Just (names <> [name])

parseStartChoices ::
  [Player] ->
  Map PlayerId [Text] ->
  Map PlayerId Text ->
  Map PlayerId [Text] ->
  Map PlayerId [Text] ->
  Either Text [KeldonScript]
parseStartChoices players startOptions startWorlds roundZeroDiscards roundZeroSaves =
  traverse playerStartChoice players
  where
    playerStartChoice player =
      case Map.lookup (playerId player) startOptions of
        Just _ -> startChoiceWithOptions player
        Nothing ->
          forcedStartChoices player

    startChoiceWithOptions player = do
      startWorld <- case Map.lookup (playerId player) startWorlds of
        Just world -> pure world
        Nothing -> Left ("missing chosen start world for player " <> showText (unPlayerId (playerId player)))
      let discards = Map.findWithDefault [] (playerId player) roundZeroDiscards
          saves = Map.findWithDefault [] (playerId player) roundZeroSaves
      case discards of
        firstDiscard : secondDiscard : extraDiscards -> do
          savedExtraDiscards <- removeSavedCards extraDiscards saves
          pure $ choiceScript (playerSeat player) $
            [ Choice Required (playerSeat player)
                (ChooseStart [firstDiscard, secondDiscard] startWorld)
            ]
            <> saveChoices player saves
            <> discardChoice player savedExtraDiscards
        _ ->
          Left ("start choice for player " <> showText (unPlayerId (playerId player)) <> " has fewer than two discarded cards")

    forcedStartChoices player = do
      let discards = Map.findWithDefault [] (playerId player) roundZeroDiscards
          saves = Map.findWithDefault [] (playerId player) roundZeroSaves
      savedDiscards <- removeSavedCards discards saves
      pure $ choiceScript (playerSeat player) $
        saveChoices player saves <> discardChoice player savedDiscards

    discardChoice _ [] = []
    discardChoice player cards =
      [Choice Required (playerSeat player) (ChooseDiscard cards)]

    saveChoices player saves =
      [ Choice Optional (playerSeat player) (ChooseSave card)
      | card <- saves
      ]

removeSavedCards :: [Text] -> [Text] -> Either Text [Text]
removeSavedCards =
  foldM removeOne
  where
    removeOne discards saved =
      case break (== saved) discards of
        (_, []) -> Left ("setup save card was not in setup discards: " <> saved)
        (before, _ : after) -> pure (before <> after)

mconcatScripts :: [KeldonScript] -> KeldonScript
mconcatScripts = foldl appendScript memptyScript

memptyScript :: KeldonScript
memptyScript = preambleScript []

renderStartOptions :: [Player] -> Map PlayerId [Text] -> [ScriptLine]
renderStartOptions players startOptions =
  concatMap renderPlayerStartOptions players
  where
    renderPlayerStartOptions player =
      case Map.lookup (playerId player) startOptions of
        Just [firstOption, secondOption] ->
          [StartOptions (playerSeat player) firstOption secondOption]
        _ -> []

gamedataFor :: Object -> PlayerId -> Either Text Object
gamedataFor gamedatas (PlayerId pid) =
  case KeyMap.lookup (Key.fromText (showText pid)) gamedatas of
    Just value -> expectObject ("gamedatas." <> showText pid) value
    Nothing -> Left ("missing gamedatas for player " <> showText pid)

optionalObject :: Text -> Object -> Maybe Object
optionalObject name obj = do
  value <- optionalField name obj
  either (const Nothing) Just (expectObject name value)

sortCards :: [Value] -> [Value]
sortCards = List.sortOn cardSortKey
  where
    cardSortKey value =
      case cardId value of
        Right cid -> cid
        Left _ -> maxBound

showText :: Show a => a -> Text
showText = Text.pack . show
