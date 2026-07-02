{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Header
  ( parseHeader
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , boolValue
  , field
  , intValue
  , keyText
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  , valueText
  )
import Rftg.Bga.Types
  ( GoalType (..)
  , Player (..)
  , PlayerId (..)
  , Seat (..)
  )
import Rftg.Parser.Common
  ( notificationObjects
  , notificationType
  , parsePlayers
  , ReviewSelection
  , reviewGamedataObject
  , selectReviewPlayer
  , tableInfoObject
  )
import Rftg.Keldon.Script
  ( HeaderLine (..)
  , KeldonScript
  , ScriptLine (..)
  , preambleScript
  )

parseHeader :: ReviewSelection -> Value -> Either Text KeldonScript
parseHeader reviewSelection rootValue = do
  root <- expectObject "root" rootValue
  tableInfo <- tableInfoObject root
  players <- parsePlayers root
  review <- selectReviewPlayer reviewSelection players
  gamedatas <- objectField "gamedatas" root
  reviewGamedata <- reviewGamedataObject gamedatas review

  options <- parseOptions tableInfo
  expansion <- parseExpansion options
  promoValue <- optionInt "New Worlds" 0 options
  let promoOn = promoValue /= 0

  takeoversOn <- parseTakeovers tableInfo reviewGamedata options
  activeGoals <- parseActiveGoals reviewGamedata
  let goalsOn = not (null activeGoals)

  tableId <- intValue "table id" =<< field "id" tableInfo
  conceded <- parseConceded root tableInfo
  finalScores <- parseFinalScores tableInfo players conceded

  pure $ preambleScript $
    fmap Header
      [ TableId tableId
      , PlayerCount (length players)
      , Expanded expansion
      , Advanced (length players == 2)
      , Promo promoOn
      , GoalsEnabled goalsOn
      , TakeoversEnabled takeoversOn
      , Seed (tableId `mod` 1000000)
      , Review (playerSeat review)
      , Concede conceded
      ]
      <> finalScores
      <> fmap renderPlayer players
      <> fmap renderGoal activeGoals

parseOptions :: Object -> Either Text (Map Text Value)
parseOptions tableInfo = do
  options <- objectField "options" tableInfo
  pure $ Map.fromList $ mapMaybe optionPair (objectValues options)
  where
    optionPair value = do
      obj <- either (const Nothing) Just (expectObject "option" value)
      nameValue <- optionalField "name" obj
      valueValue <- optionalField "value" obj
      name <- either (const Nothing) Just (textValue "option name" nameValue)
      pure (name, valueValue)

parseExpansion :: Map Text Value -> Either Text Int
parseExpansion options =
  optionInt "Expansion" 0 options >>= \rawExpansion ->
  case Map.lookup rawExpansion bgaExpansion of
    Just expansion -> Right expansion
    Nothing -> Left ("unknown BGA expansion value " <> showText rawExpansion)
  where
    bgaExpansion = Map.fromList
      [ (0, 0)
      , (1, 1)
      , (2, 2)
      , (3, 2)
      , (4, 3)
      ]

parseTakeovers :: Object -> Object -> Map Text Value -> Either Text Bool
parseTakeovers _tableInfo reviewGamedata options = do
  takeoversOn <- boolValue "gamedatas.takeovers" =<< field "takeovers" reviewGamedata
  takeoverOption <- optionInt "Takeover" 0 options
  case (takeoverOption, takeoversOn) of
    (1, False) -> Left "BGA Takeover option is Allow, but gamedatas disables takeovers"
    (2, True) -> Left "BGA Takeover option is No, but gamedatas enables takeovers"
    _ | takeoverOption `notElem` [0, 1, 2, 3] ->
        Left ("unknown BGA takeover option value " <> showText takeoverOption)
    _ -> pure takeoversOn

parseActiveGoals :: Object -> Either Text [Text]
parseActiveGoals reviewGamedata = do
  goalTypes <- parseGoalTypes reviewGamedata
  goalIds <- parseActiveGoalIds reviewGamedata
  pure
    [ mapGoalName (goalName goalType)
    | gid <- goalIds
    , Just goalType <- [Map.lookup gid goalTypes]
    , goalKind goalType `elem` ["first", "most"]
    ]

parseGoalTypes :: Object -> Either Text (Map Int GoalType)
parseGoalTypes reviewGamedata = do
  goalTypesObject <- objectField "goal_types" reviewGamedata
  fmap Map.fromList $
    traverse parseGoalType (KeyMap.toList goalTypesObject)
  where
    parseGoalType (key, value) = do
      gid <- intText "goal type id" (keyText key)
      obj <- expectObject "goal type" value
      kind <- textValue "goal type kind" =<< field "type" obj
      name <- textValue "goal type name" =<< field "name" obj
      pure (gid, GoalType kind name)

parseActiveGoalIds :: Object -> Either Text [Int]
parseActiveGoalIds reviewGamedata =
  case optionalField "goals" reviewGamedata of
    Nothing -> pure []
    Just Null -> pure []
    Just value -> do
      goals <- expectObject "goals" value
      firstIds <- idsForKind goals "first"
      mostIds <- idsForKind goals "most"
      pure (firstIds <> mostIds)
  where
    idsForKind goals kind =
      case optionalField kind goals of
        Nothing -> pure []
        Just (Object entries) -> traverse goalIdFromEntry (objectValues entries)
        Just (Array entries) -> traverse goalIdFromEntry (toList entries)
        Just _ -> Left ("goals." <> kind <> " is neither an object nor an array")

    goalIdFromEntry value =
      case value of
        Object obj -> intValue "goal type" =<< field "type" obj
        _ -> intValue "goal type" value

parseConceded :: Object -> Object -> Either Text Bool
parseConceded root tableInfo = do
  let result = optionalObject "result" tableInfo
      reason = maybe "" (optionalText "endgame_reason") result
      concedeFlag = maybe "" (maybe "" valueText . optionalField "concede") result
      tableSaysConceded = "concede" `Text.isInfixOf` reason || concedeFlag == "1"
  if tableSaysConceded
    then pure True
    else hasNotificationType root "playerConcedeGame"

parseFinalScores :: Object -> [Player] -> Bool -> Either Text [ScriptLine]
parseFinalScores tableInfo players conceded
  | conceded = pure []
  | otherwise =
      case optionalObject "result" tableInfo >>= optionalField "player" of
        Nothing -> pure []
        Just (Array values) -> fmap concat (traverse parseResultPlayer (toList values))
        Just _ -> Left "result.player is not an array"
  where
    seatsByPlayerId = Map.fromList
      [ (unPlayerId (playerId player), unSeat (playerSeat player))
      | player <- players
      ]

    parseResultPlayer value = do
      obj <- expectObject "result player" value
      pid <- resultPlayerId obj
      case (Map.lookup pid seatsByPlayerId, optionalField "score" obj) of
        (Just seat, Just scoreValue) -> do
          score <- intValue "final score" scoreValue
          pure [Header (FinalScore (Seat seat) score)]
        _ -> pure []

    resultPlayerId obj =
      case optionalField "player_id" obj of
        Just value -> intValue "result player_id" value
        Nothing -> intValue "result id" =<< field "id" obj

hasNotificationType :: Object -> Text -> Either Text Bool
hasNotificationType root target = do
  notifications <- notificationObjects root
  pure $ any ((== target) . notificationType) notifications

renderPlayer :: Player -> ScriptLine
renderPlayer player =
  Header (PlayerName (playerSeat player) (playerName player))

renderGoal :: Text -> ScriptLine
renderGoal name = Header (Goal name)

mapGoalName :: Text -> Text
mapGoalName "System diversity" = "System Diversity"
mapGoalName "Prestige leader" = "Galactic Prestige"
mapGoalName name = name

optionInt :: Text -> Int -> Map Text Value -> Either Text Int
optionInt name defaultValue options =
  case Map.lookup name options of
    Nothing -> Right defaultValue
    Just value -> intValue ("option " <> name) value

optionalObject :: Text -> Object -> Maybe Object
optionalObject name obj = do
  value <- optionalField name obj
  either (const Nothing) Just (expectObject name value)

optionalText :: Text -> Object -> Text
optionalText name obj =
  case optionalField name obj of
    Just (String text) -> text
    _ -> ""

showText :: Show a => a -> Text
showText = Text.pack . show

intText :: Text -> Text -> Either Text Int
intText label value =
  intValue label (String value)

toList :: Foldable f => f a -> [a]
toList = foldr (:) []
