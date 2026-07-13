{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Takeover
  ( parseTakeoverChoices
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
  , textValue
  )
import Rftg.Bga.State
  ( BgaPhase (..)
  , BgaSettleState (..)
  , BgaState (..)
  , bgaStateHasPhase
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
  ( PhaseCursor
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data TakeoverAttempt = TakeoverAttempt
  { takeoverAttacker :: PlayerId
  , takeoverDefender :: Maybe PlayerId
  , takeoverTargetId :: Int
  , takeoverTargetName :: Text
  , takeoverPowerId :: Int
  , takeoverPowerName :: Text
  }
  deriving stock (Eq, Show)

data PendingDefense = PendingDefense
  { defenseCards :: [Text]
  , defenseSpecials :: [Text]
  }
  deriving stock (Eq, Show)

data TakeoverState = TakeoverState
  { phaseCursor :: PhaseCursor
  , currentBgaState :: Maybe BgaState
  , cardIndex :: CardIndex
  , activeAttempt :: Maybe TakeoverAttempt
  , pendingConfirmations :: Map (Int, Int) (Int, TakeoverAttempt)
  , emittedPreventions :: Set (PlayerId, Int, Int)
  , pendingDefenses :: Map PlayerId PendingDefense
  , takeoverScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseTakeoverChoices :: Value -> Either Text KeldonScript
parseTakeoverChoices rootValue = do
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
      (takeoverStep players cardInfosByName cardTypes)
      (emptyTakeoverState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  ensureNoPendingDefenses walked
  ensureAllAttemptsResolved walked
  pure (takeoverScript walked)

emptyTakeoverState :: CardIndex -> TakeoverState
emptyTakeoverState startingCardIndex = TakeoverState
  { phaseCursor = initialPhaseCursor
  , currentBgaState = Nothing
  , cardIndex = startingCardIndex
  , activeAttempt = Nothing
  , pendingConfirmations = Map.empty
  , emittedPreventions = Set.empty
  , pendingDefenses = Map.empty
  , takeoverScript = emptyScript
  }

takeoverStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  TakeoverState ->
  (Int, Object) ->
  Either Text TakeoverState
takeoverStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState players eventIx stateWithCards notification
    "confirmTakeover" -> handleConfirmTakeover players eventIx stateWithCards notification
    "discard" -> handleDefenseDiscard stateWithCards notification
    "mercenary_used" -> handleDefenseSpecial stateWithCards notification
    "consume" -> handleDefenseConsume stateWithCards notification
    "updatePrestige" -> handlePrestige players cardInfosByName eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: [Player] -> Int -> TakeoverState -> Object -> Either Text TakeoverState
handleGameState players eventIx state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      stateBeforeEnter <-
        case bgaState of
          BgaSettleState BgaSettleTakeoverPrevent -> do
            attempt <- takeoverAttemptFromState players state args
            enterConfirmedAttempt players attempt state
          BgaSettleState BgaSettleTakeoverResolution ->
            flushDefense players eventIx state
          _ -> pure state
      let leavingAttempt =
            case (currentBgaState stateBeforeEnter, bgaState) of
              ( Just (BgaSettleState BgaSettleTakeoverResolution)
                , BgaSettleState BgaSettleTakeoverCheck
                ) -> True
              ( Just (BgaSettleState BgaSettleTakeoverPrevent)
                , BgaSettleState BgaSettleTakeoverCheck
                ) -> True
              _ -> False
          completedAttempt = if leavingAttempt then activeAttempt stateBeforeEnter else Nothing
          remainingAttempts =
            if not (bgaStateHasPhase BgaSettle bgaState)
              then Map.empty
              else
                case completedAttempt of
                  Nothing -> pendingConfirmations stateBeforeEnter
                  Just attempt -> Map.delete (attemptKey attempt) (pendingConfirmations stateBeforeEnter)
          remainingPreventions =
            case completedAttempt of
              Nothing -> emittedPreventions stateBeforeEnter
              Just attempt -> removeAttemptPreventions (emittedPreventions stateBeforeEnter) attempt
      pure stateBeforeEnter
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor stateBeforeEnter)
        , currentBgaState = Just bgaState
        , activeAttempt = if leavingAttempt then Nothing else activeAttempt stateBeforeEnter
        , pendingConfirmations = remainingAttempts
        , emittedPreventions = remainingPreventions
        }

handleConfirmTakeover :: [Player] -> Int -> TakeoverState -> Object -> Either Text TakeoverState
handleConfirmTakeover players eventIx state notification = do
  args <- objectField "args" notification
  targetId <- intValue "confirmTakeover target_id" =<< field "target_id" args
  targetName <- canonicalCardName <$> (textValue "confirmTakeover target_name" =<< field "target_name" args)
  powerId <- intValue "confirmTakeover takeovercard_id" =<< field "takeovercard_id" args
  powerName <- canonicalCardName <$> (textValue "confirmTakeover takeovercard_name" =<< field "takeovercard_name" args)
  validateKnownName "takeover target" state targetId targetName
  validateKnownName "takeover power" state powerId powerName
  attacker <- lookupCardOwner powerId state
  _ <- lookupPlayer players attacker
  let attempt = TakeoverAttempt
        { takeoverAttacker = attacker
        , takeoverDefender = Nothing
        , takeoverTargetId = targetId
        , takeoverTargetName = targetName
        , takeoverPowerId = powerId
        , takeoverPowerName = powerName
        }
      key = attemptKey attempt
  case Map.lookup key (pendingConfirmations state) of
    Nothing ->
      pure state
        { pendingConfirmations = Map.insert key (eventIx, attempt) (pendingConfirmations state)
        }
    Just (_, current)
      | takeoverAttacker current == attacker -> pure state
      | otherwise ->
          Left
            ( "takeover confirmation changed attacker for "
                <> renderAttempt attempt
            )

enterConfirmedAttempt :: [Player] -> TakeoverAttempt -> TakeoverState -> Either Text TakeoverState
enterConfirmedAttempt players attempt state =
  case activeAttempt state of
    Just current
      | attemptKey current == attemptKey attempt -> pure state { activeAttempt = Just attempt }
      | otherwise ->
          Left
            ( "takeover resolution overlapped "
                <> renderAttempt current
                <> " and "
                <> renderAttempt attempt
            )
    Nothing ->
      case Map.lookup (attemptKey attempt) (pendingConfirmations state) of
        Nothing -> Left ("takeover entered prevention without confirmTakeover: " <> renderAttempt attempt)
        Just (confirmEventIx, confirmed) -> do
          if takeoverAttacker confirmed == takeoverAttacker attempt
            then pure ()
            else Left ("takeover confirmation attacker mismatch for " <> renderAttempt attempt)
          player <- lookupPlayer players (takeoverAttacker attempt)
          order <- cursorChoiceOrder (phaseCursor state) confirmEventIx
          let seat = playerSeat player
              choice = ChooseTakeover (Just (takeoverTargetName attempt, takeoverPowerName attempt))
              script = choiceScriptAt order seat [Choice Optional seat choice]
          pure state
            { activeAttempt = Just attempt
            , takeoverScript = takeoverScript state `appendScript` script
            }

takeoverAttemptFromState :: [Player] -> TakeoverState -> Object -> Either Text TakeoverAttempt
takeoverAttemptFromState players state outerArgs = do
  args <- objectField "args" outerArgs
  attacker <- PlayerId <$> (intValue "takeover player_id" =<< field "player_id" args)
  defender <- PlayerId <$> (intValue "takeover defender" =<< field "defender" args)
  _ <- lookupPlayer players attacker
  _ <- lookupPlayer players defender
  targetId <- intValue "takeover player_takeover_target" =<< field "player_takeover_target" args
  targetName <- canonicalCardName <$> (textValue "takeover target_world" =<< field "target_world" args)
  powerId <- intValue "takeover player_just_played" =<< field "player_just_played" args
  powerName <- canonicalCardName <$> (textValue "takeover takeovercard_name" =<< field "takeovercard_name" args)
  validateKnownName "takeover target" state targetId targetName
  validateKnownName "takeover power" state powerId powerName
  owner <- lookupCardOwner powerId state
  if owner /= attacker
    then
      Left
        ( "takeover power owner mismatch: card "
            <> powerName
            <> " belongs to "
            <> showText (unPlayerId owner)
            <> ", state says attacker "
            <> showText (unPlayerId attacker)
        )
    else
      pure TakeoverAttempt
        { takeoverAttacker = attacker
        , takeoverDefender = Just defender
        , takeoverTargetId = targetId
        , takeoverTargetName = targetName
        , takeoverPowerId = powerId
        , takeoverPowerName = powerName
        }

handleDefenseDiscard :: TakeoverState -> Object -> Either Text TakeoverState
handleDefenseDiscard state notification
  | not (inTakeoverState BgaSettleTakeoverDefenderBoost state) = pure state
  | otherwise = do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      pid <- discardOwner (cardIndex state) cardIds
      cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      pure state
        { pendingDefenses =
            Map.alter
              (Just . appendDefenseCards cards . maybe emptyPendingDefense id)
              pid
              (pendingDefenses state)
        }

handleDefenseSpecial :: TakeoverState -> Object -> Either Text TakeoverState
handleDefenseSpecial state notification
  | not (inTakeoverState BgaSettleTakeoverDefenderBoost state) = pure state
  | otherwise = do
      args <- objectField "args" notification
      sourceId <- intValue "mercenary_used card" =<< field "card" args
      source <- lookupKnownCardName (cardIndex state) sourceId
      pid <- lookupCardOwner sourceId state
      pure (appendDefenseSpecial pid source state)

handleDefenseConsume :: TakeoverState -> Object -> Either Text TakeoverState
handleDefenseConsume state notification
  | not (inTakeoverState BgaSettleTakeoverDefenderBoost state) = pure state
  | otherwise = do
      args <- objectField "args" notification
      case optionalField "world_id" args of
        Nothing -> pure state
        Just sourceValue -> do
          sourceId <- intValue "defense consume world_id" sourceValue
          source <- lookupKnownCardName (cardIndex state) sourceId
          pid <-
            case optionalField "player_id" args of
              Nothing -> lookupCardOwner sourceId state
              Just pidValue -> pure . PlayerId =<< intValue "defense consume player_id" pidValue
          pure (appendDefenseSpecial pid source state)

handlePrestige ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  TakeoverState ->
  Object ->
  Either Text TakeoverState
handlePrestige players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  amount <- intValue "updatePrestige nbr" =<< field "nbr" args
  case optionalField "card_name" args of
    Nothing -> pure state
    Just sourceValue
      | amount >= 0 -> pure state
      | otherwise -> do
          pid <- PlayerId <$> (intValue "updatePrestige player_id" =<< field "player_id" args)
          _ <- lookupPlayer players pid
          source <- canonicalCardName <$> textValue "updatePrestige card_name" sourceValue
          sourceInfo <- lookupCardInfo cardInfosByName source
          case currentBgaState state of
            Just (BgaSettleState BgaSettleTakeoverDefenderBoost)
              | cardTypeHasPrestigeMilitary sourceInfo ->
                  pure (appendDefenseSpecial pid source state)
              | otherwise ->
                  Left ("takeover defense spent prestige with unsupported source " <> source)
            Just (BgaSettleState BgaSettleTakeoverPrevent)
              | cardTypeHasTakeoverPrevention sourceInfo ->
                  emitPrevention players eventIx pid state
              | otherwise ->
                  Left ("takeover prevention spent prestige with unsupported source " <> source)
            _ -> pure state

emitPrevention :: [Player] -> Int -> PlayerId -> TakeoverState -> Either Text TakeoverState
emitPrevention players eventIx pid state = do
  attempt <- requireActiveAttempt "takeover prevention" state
  player <- lookupPlayer players pid
  let key = (pid, takeoverTargetId attempt, takeoverPowerId attempt)
  if key `Set.member` emittedPreventions state
    then Left ("duplicate takeover prevention by player " <> showText (unPlayerId pid))
    else do
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      let seat = playerSeat player
          choice = ChooseTakeoverPrevent (Just (takeoverTargetName attempt, takeoverPowerName attempt))
          script = choiceScriptAt order seat [Choice Required seat choice]
      pure state
        { emittedPreventions = Set.insert key (emittedPreventions state)
        , takeoverScript = takeoverScript state `appendScript` script
        }

flushDefense :: [Player] -> Int -> TakeoverState -> Either Text TakeoverState
flushDefense players eventIx state = do
  attempt <- requireActiveAttempt "takeover resolution" state
  defender <-
    case takeoverDefender attempt of
      Just pid -> pure pid
      Nothing -> Left ("takeover resolution has no defender: " <> renderAttempt attempt)
  case Map.lookup defender (pendingDefenses state) of
    Nothing -> pure state
    Just pending -> do
      player <- lookupPlayer players defender
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      let seat = playerSeat player
          choice = ChooseDefend (defenseCards pending) (defenseSpecials pending)
          script = choiceScriptAt order seat [Choice Required seat choice]
      pure state
        { pendingDefenses = Map.delete defender (pendingDefenses state)
        , takeoverScript = takeoverScript state `appendScript` script
        }

emptyPendingDefense :: PendingDefense
emptyPendingDefense = PendingDefense
  { defenseCards = []
  , defenseSpecials = []
  }

appendDefenseCards :: [Text] -> PendingDefense -> PendingDefense
appendDefenseCards cards pending = pending
  { defenseCards = defenseCards pending <> cards
  }

appendDefenseSpecial :: PlayerId -> Text -> TakeoverState -> TakeoverState
appendDefenseSpecial pid source state = state
  { pendingDefenses =
      Map.alter
        (Just . addSpecial . maybe emptyPendingDefense id)
        pid
        (pendingDefenses state)
  }
  where
    addSpecial pending = pending
      { defenseSpecials = appendUnique source (defenseSpecials pending)
      }

ensureNoPendingDefenses :: TakeoverState -> Either Text ()
ensureNoPendingDefenses state =
  case Map.toList (pendingDefenses state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved takeover defenses: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid)
                  <> "="
                  <> Text.intercalate "/" (defenseCards defense <> defenseSpecials defense)
              | (pid, defense) <- pending
              ]
        )

ensureAllAttemptsResolved :: TakeoverState -> Either Text ()
ensureAllAttemptsResolved state
  | not (Set.null (emittedPreventions state)) =
      Left ("unresolved takeover preventions: " <> showText (Set.toList (emittedPreventions state)))
  | otherwise =
      case activeAttempt state of
        Nothing -> pure ()
        Just attempt -> Left ("active takeover remained at end of log: " <> renderAttempt attempt)

inTakeoverState :: BgaSettleState -> TakeoverState -> Bool
inTakeoverState expected state =
  currentBgaState state == Just (BgaSettleState expected)

attemptKey :: TakeoverAttempt -> (Int, Int)
attemptKey attempt = (takeoverTargetId attempt, takeoverPowerId attempt)

requireActiveAttempt :: Text -> TakeoverState -> Either Text TakeoverAttempt
requireActiveAttempt label state =
  case activeAttempt state of
    Just attempt -> pure attempt
    Nothing -> Left (label <> " without an active takeover")

validateKnownName :: Text -> TakeoverState -> Int -> Text -> Either Text ()
validateKnownName label state cardId expected = do
  actual <- lookupKnownCardName (cardIndex state) cardId
  if actual == expected
    then pure ()
    else Left (label <> " name mismatch: state says " <> expected <> ", card index says " <> actual)

lookupCardOwner :: Int -> TakeoverState -> Either Text PlayerId
lookupCardOwner cardId state =
  case Map.lookup cardId (knownCardOwners (cardIndex state)) of
    Just owner -> pure owner
    Nothing -> Left ("takeover card owner unknown for " <> showText cardId)

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

appendUnique :: Eq a => a -> [a] -> [a]
appendUnique value values =
  if value `elem` values
    then values
    else values <> [value]

renderAttempt :: TakeoverAttempt -> Text
renderAttempt attempt =
  takeoverPowerName attempt <> " -> " <> takeoverTargetName attempt

removeAttemptPreventions :: Set (PlayerId, Int, Int) -> TakeoverAttempt -> Set (PlayerId, Int, Int)
removeAttemptPreventions preventions attempt =
  Set.filter
    (\(_, targetId, powerId) ->
      targetId /= takeoverTargetId attempt || powerId /= takeoverPowerId attempt
    )
    preventions

showText :: Show a => a -> Text
showText = Text.pack . show
