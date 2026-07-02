{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Rftg.Parser.Action
  ( parseActions
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , field
  , intValue
  , keyText
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
import Rftg.Parser.Common
  ( notificationObjects
  , notificationType
  , parsePlayers
  )

type Snapshot = Map PlayerId PhasePicks
type PhasePicks = Map Int Int

data RoundState = RoundState
  { beforeSelectLast :: [Snapshot]
  , afterSelectLast :: [Snapshot]
  , selectLastPlayer :: Maybe PlayerId
  }
  deriving stock (Eq, Show)

emptyRound :: RoundState
emptyRound = RoundState
  { beforeSelectLast = []
  , afterSelectLast = []
  , selectLastPlayer = Nothing
  }

parseActions :: Value -> Either Text KeldonScript
parseActions rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  notifications <- notificationObjects root
  rounds <- collectRounds notifications
  let conceded = any ((== "playerConcedeGame") . notificationType) notifications
  choices <- traverse (uncurry3 (roundChoices players)) (markFinalRounds conceded rounds)
  pure (mconcatScripts choices)

markFinalRounds :: Bool -> [RoundState] -> [(Int, Bool, RoundState)]
markFinalRounds conceded rounds =
  [ (ix, conceded && ix == lastIx, roundState)
  | (ix, roundState) <- zip [0 :: Int ..] rounds
  ]
  where
    lastIx = length rounds - 1

collectRounds :: [Object] -> Either Text [RoundState]
collectRounds notifications =
  finish [] Nothing notifications
  where
    finish done current [] =
      pure $ reverse $
        case current of
          Nothing -> done
          Just roundState -> roundState : done
    finish done current (notification : rest) =
      case notificationType notification of
        "gameStateChange" -> do
          stateId <- gameStateId notification
          case stateId of
            Just 10 ->
              finish (maybe done (: done) current) (Just emptyRound) rest
            Just 12 -> do
              pid <- activePlayer notification
              finish done (Just (setSelectLast pid current)) rest
            _ ->
              finish done current rest
        "phase_choices" ->
          case current of
            Nothing -> finish done current rest
            Just roundState -> do
              snapshot <- decodeActionSnapshot notification
              let updated =
                    if Map.null snapshot
                      then roundState
                      else appendSnapshot snapshot roundState
              finish done (Just updated) rest
        _ ->
          finish done current rest

setSelectLast :: PlayerId -> Maybe RoundState -> RoundState
setSelectLast pid Nothing =
  emptyRound { selectLastPlayer = Just pid }
setSelectLast pid (Just roundState) =
  roundState { selectLastPlayer = Just pid }

appendSnapshot :: Snapshot -> RoundState -> RoundState
appendSnapshot snapshot roundState =
  case selectLastPlayer roundState of
    Nothing ->
      roundState { beforeSelectLast = beforeSelectLast roundState <> [snapshot] }
    Just _ ->
      roundState { afterSelectLast = afterSelectLast roundState <> [snapshot] }

roundChoices :: [Player] -> Int -> Bool -> RoundState -> Either Text KeldonScript
roundChoices players roundIndex allowIncomplete roundState =
  tolerateAllowedIncomplete $
    case selectLastPlayer roundState of
      Nothing -> do
        snapshot <- latestCompleteSnapshot players expectedActs (beforeSelectLast roundState)
        choicesFromSnapshot (ChoiceOrder [roundIndex, 0, 0]) players snapshot
      Just pid -> do
        let expectedInitial player =
              if playerId player == pid then 1 else expectedActs
        initial <- latestCompleteSnapshotBy players expectedInitial (beforeSelectLast roundState)
        final <- latestCompleteSnapshot players expectedActs (afterSelectLast roundState)
        initialChoices <- choicesFromSnapshot (ChoiceOrder [roundIndex, 0, 0]) players initial
        finalChoice <- selectLastFinalChoice (ChoiceOrder [roundIndex, 0, 1]) players pid final
        pure (initialChoices `appendScript` finalChoice)
  where
    expectedActs = if length players == 2 then 2 else 1
    tolerateAllowedIncomplete result =
      case result of
        Left err | allowIncomplete && err == noCompleteSnapshot ->
          pure emptyScript
        _ -> result

latestCompleteSnapshot :: [Player] -> Int -> [Snapshot] -> Either Text Snapshot
latestCompleteSnapshot players expected snapshots =
  latestCompleteSnapshotBy players (const expected) snapshots

latestCompleteSnapshotBy ::
  [Player] ->
  (Player -> Int) ->
  [Snapshot] ->
  Either Text Snapshot
latestCompleteSnapshotBy players expected snapshots =
  case List.find complete (reverse snapshots) of
    Just snapshot -> pure snapshot
    Nothing -> Left noCompleteSnapshot
  where
    complete snapshot =
      all (playerComplete snapshot) players
    playerComplete snapshot player =
      case Map.lookup (playerId player) snapshot of
        Nothing -> False
        Just picks ->
          case actionCodes picks of
            Left _ -> False
            Right actions -> length actions == expected player

choicesFromSnapshot :: ChoiceOrder -> [Player] -> Snapshot -> Either Text KeldonScript
choicesFromSnapshot order players snapshot =
  fmap mconcatScripts $
    traverse (uncurry (actionChoiceFor order))
    [ (player, picks)
    | player <- players
    , Just picks <- [Map.lookup (playerId player) snapshot]
    ]

selectLastFinalChoice :: ChoiceOrder -> [Player] -> PlayerId -> Snapshot -> Either Text KeldonScript
selectLastFinalChoice order players pid snapshot =
  fmap mconcatScripts $
    traverse (uncurry (actionChoiceFor order))
    [ (player, picks)
    | player <- players
    , playerId player == pid
    , Just picks <- [Map.lookup pid snapshot]
    ]

actionChoiceFor :: ChoiceOrder -> Player -> PhasePicks -> Either Text KeldonScript
actionChoiceFor order player picks =
  case actionCodes picks of
    Left err -> Left err
    Right actions ->
      pure $ choiceScriptAt order (playerSeat player)
        [Choice Required (playerSeat player) (ChooseAction actions)]

decodeActionSnapshot :: Object -> Either Text Snapshot
decodeActionSnapshot notification = do
  args <- objectField "args" notification
  fmap (Map.unionsWith Map.union) $
    traverse phaseSnapshot (KeyMap.toList args)
  where
    phaseSnapshot (phaseKey, value) =
      case value of
        Object picks -> do
          phase <- intValue "phase_choices phase" (String (keyText phaseKey))
          fmap Map.fromList $
            traverse (playerPick phase) (KeyMap.toList picks)
        Array _ -> pure Map.empty
        _ -> Left "phase_choices phase value is neither object nor array"

    playerPick phase (playerKey, value) = do
      pid <- PlayerId <$> intValue "phase_choices player id" (String (keyText playerKey))
      variant <- intValue "phase_choices variant" value
      pure (pid, Map.singleton phase variant)

actionCodes :: PhasePicks -> Either Text [Int]
actionCodes picks =
  fmap (List.sortOn (`mod` 128) . concat) $
    traverse phaseActions (Map.toList picks)

phaseActions :: (Int, Int) -> Either Text [Int]
phaseActions (phase, rawVariant)
  | phase == 7 =
      if rawVariant == 10
        then pure [actSearch]
        else Left ("unknown variant " <> showText rawVariant <> " for search phase")
  | otherwise = do
      let (prestige, variant) =
            if rawVariant >= 10
              then (actPrestige, rawVariant - 10)
              else (0, rawVariant)
      case variant of
        2 -> doubledPhase phase prestige
        _
          | phase == 1 ->
              pure [if variant == 1 then actExplore50 + prestige else actExplore11 + prestige]
          | phase == 4 ->
              pure [if variant == 1 then actConsumeX2 + prestige else actConsumeTrade + prestige]
          | variant == 0 ->
              (: []) . (+ prestige) <$> phaseToAct phase
          | otherwise ->
              Left ("unknown variant " <> showText rawVariant <> " for phase " <> showText phase)

doubledPhase :: Int -> Int -> Either Text [Int]
doubledPhase phase prestige =
  case phase of
    1 -> pure [actExplore50 + prestige, actExplore11]
    2 -> pure [actDevelop + prestige, actDevelop2]
    3 -> pure [actSettle + prestige, actSettle2]
    4 -> pure [actConsumeTrade + prestige, actConsumeX2]
    _ -> Left ("doubled phase " <> showText phase <> "?")

phaseToAct :: Int -> Either Text Int
phaseToAct phase =
  case phase of
    1 -> pure actExplore11
    2 -> pure actDevelop
    3 -> pure actSettle
    4 -> pure actConsumeTrade
    5 -> pure actProduce
    _ -> Left ("unknown phase " <> showText phase)

gameStateId :: Object -> Either Text (Maybe Int)
gameStateId notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure Nothing
    Just value -> Just <$> intValue "gameStateChange id" value

activePlayer :: Object -> Either Text PlayerId
activePlayer notification = do
  args <- objectField "args" notification
  PlayerId <$> (intValue "active_player" =<< field "active_player" args)

mconcatScripts :: [KeldonScript] -> KeldonScript
mconcatScripts = foldl appendScript emptyScript

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

noCompleteSnapshot :: Text
noCompleteSnapshot = "action round has no complete phase_choices snapshot"

showText :: Show a => a -> Text
showText = Text.pack . show

actSearch :: Int
actSearch = 0

actExplore50 :: Int
actExplore50 = 1

actExplore11 :: Int
actExplore11 = 2

actDevelop :: Int
actDevelop = 3

actDevelop2 :: Int
actDevelop2 = 4

actSettle :: Int
actSettle = 5

actSettle2 :: Int
actSettle2 = 6

actConsumeTrade :: Int
actConsumeTrade = 7

actConsumeX2 :: Int
actConsumeX2 = 8

actProduce :: Int
actProduce = 9

actPrestige :: Int
actPrestige = 128
