{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Settle
  ( parseSettleChoices
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
  , objectValues
  , optionalField
  , expectObject
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

data SettleSource = SettleSource
  { settleSourceName :: Text
  , settleSourcePlayer :: Maybe PlayerId
  , settleSourceStep :: Int
  }
  deriving stock (Eq, Show)

data SettleState = SettleState
  { phaseCursor :: PhaseCursor
  , currentStateId :: Maybe Int
  , cardIndex :: CardIndex
  , tableauCards :: Map PlayerId [Text]
  , currentSettleSource :: Maybe SettleSource
  , settleSourceChoices :: Map PlayerId Int
  , extraSettleStep :: Int
  , pendingUpgrades :: Map PlayerId Text
  , discardedTableauCards :: Set Int
  , playedCards :: Set Int
  , settleScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseSettleChoices :: Value -> Either Text KeldonScript
parseSettleChoices rootValue = do
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
      (settleStep players cardInfosByName cardTypes)
      (emptySettleState players startingCardIndex)
      (zip [0 :: Int ..] notifications)
  checked <- clearSettleSource players (length notifications) True walked
  ensureNoPendingUpgrades checked
  pure (settleScript checked)

emptySettleState :: [Player] -> CardIndex -> SettleState
emptySettleState players startingCardIndex = SettleState
  { phaseCursor = initialPhaseCursor
  , currentStateId = Nothing
  , cardIndex = startingCardIndex
  , tableauCards = Map.fromList [(playerId player, []) | player <- players]
  , currentSettleSource = Nothing
  , settleSourceChoices = Map.fromList [(playerId player, 0) | player <- players]
  , extraSettleStep = 0
  , pendingUpgrades = Map.empty
  , discardedTableauCards = Set.empty
  , playedCards = Set.empty
  , settleScript = emptyScript
  }

settleStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  SettleState ->
  (Int, Object) ->
  Either Text SettleState
settleStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState players eventIx stateWithCards notification
    "showTableau" -> handleShowTableau cardTypes stateWithCards notification
    "discardfromtableau" -> handleDiscardFromTableau cardInfosByName stateWithCards notification
    "playcard" -> handlePlayCard players cardInfosByName cardTypes eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: [Player] -> Int -> SettleState -> Object -> Either Text SettleState
handleGameState players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure state
    Just idValue -> do
      stateId <- intValue "gameStateChange id" idValue
      cleared <-
        if stateClearsSettleSource stateId
          then clearSettleSource players eventIx True state
          else pure state
      let advanced = cleared
            { phaseCursor = advancePhaseCursor stateId (phaseCursor cleared)
            , currentStateId = Just stateId
            }
      case extraSettleSourceName stateId of
        Nothing -> pure advanced
        Just sourceName -> do
          sourcePlayer <- optionalActivePlayer players args
          let step = extraSettleStep advanced + 1
          pure advanced
            { currentSettleSource = Just (SettleSource sourceName sourcePlayer step)
            , extraSettleStep = step
            }

stateClearsSettleSource :: Int -> Bool
stateClearsSettleSource stateId =
  stateId
    `elem`
      [ 10
      , 20
      , 21
      , 30
      , 31
      , 230
      , 231
      , 311
      , 40
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
      ]

extraSettleSourceName :: Int -> Maybe Text
extraSettleSourceName stateId =
  case stateId of
    42 -> Just "Improved Logistics"
    242 -> Just "Rebel Sneak Attack"
    342 -> Just "Imperium Supply Convoy"
    442 -> Just "Terraforming Project"
    _ -> Nothing

optionalActivePlayer :: [Player] -> Object -> Either Text (Maybe PlayerId)
optionalActivePlayer players args = do
  pid <- PlayerId <$> (intValue "gameStateChange active_player" =<< field "active_player" args)
  if pid == PlayerId 0
    then pure Nothing
    else do
      _ <- lookupPlayer players pid
      pure (Just pid)

clearSettleSource :: [Player] -> Int -> Bool -> SettleState -> Either Text SettleState
clearSettleSource players eventIx decline state =
  case currentSettleSource state of
    Nothing -> pure state
    Just source
      | not decline -> pure state { currentSettleSource = Nothing }
      | otherwise -> do
          let pids =
                case settleSourcePlayer source of
                  Just pid -> [pid]
                  Nothing -> fmap playerId players
          cleared <- foldM (emitDeclinedSettleSource players eventIx source) state pids
          pure cleared { currentSettleSource = Nothing }

emitDeclinedSettleSource :: [Player] -> Int -> SettleSource -> SettleState -> PlayerId -> Either Text SettleState
emitDeclinedSettleSource players eventIx source state pid =
  if Map.findWithDefault 0 pid (settleSourceChoices state) >= settleSourceStep source
    then pure state
    else emitSettleChoice players eventIx pid Nothing (Just source) state

handleShowTableau :: Map Int Text -> SettleState -> Object -> Either Text SettleState
handleShowTableau cardTypes state notification = do
  args <- objectField "args" notification
  cardsObject <- expectObject "showTableau cards" =<< field "cards" args
  entries <- traverse tableauEntry (objectValues cardsObject)
  pure state { tableauCards = foldl addEntry (tableauCards state) entries }
  where
    tableauEntry cardValue = do
      pid <- cardPlayerId cardValue
      name <- cardName cardTypes cardValue
      pure (pid, name)

    addEntry table (pid, name) =
      Map.alter (Just . appendUnique name . maybe [] id) pid table

handleDiscardFromTableau :: Map Text CardTypeInfo -> SettleState -> Object -> Either Text SettleState
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
          let updatedTableau = Map.adjust (filter (/= name)) owner (tableauCards state)
              stateWithoutCard = state
                { tableauCards = updatedTableau
                , discardedTableauCards = Set.insert cardInstanceId (discardedTableauCards state)
                }
          info <- lookupCardInfo cardInfosByName name
          if currentStateId state == Just 542 && cardTypeType info == "world"
            then
              case Map.lookup owner (pendingUpgrades stateWithoutCard) of
                Nothing ->
                  pure stateWithoutCard
                    { pendingUpgrades = Map.insert owner name (pendingUpgrades stateWithoutCard)
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
            else pure stateWithoutCard

handlePlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  Int ->
  SettleState ->
  Object ->
  Either Text SettleState
handlePlayCard players cardInfosByName cardTypes eventIx state notification = do
  args <- objectField "args" notification
  cardValue <- field "card" args
  cardInstanceId <- cardId cardValue
  if cardInstanceId `Set.member` playedCards state
    then pure state
    else do
      pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
      _ <- lookupPlayer players pid
      name <- cardName cardTypes cardValue
      info <- lookupCardInfo cardInfosByName name
      let stateWithPlayed = state { playedCards = Set.insert cardInstanceId (playedCards state) }
      stateWithSource <-
        if cardTypeType info == "world"
          then maybeEmitActiveSettleSource players eventIx pid stateWithPlayed
          else pure stateWithPlayed
      case (cardTypeType info, Map.lookup pid (pendingUpgrades stateWithSource)) of
        ("world", Just oldWorld) -> emitUpgrade players cardInfosByName eventIx pid name oldWorld stateWithSource
        _ -> pure (addTableauCard pid name stateWithSource)

maybeEmitActiveSettleSource :: [Player] -> Int -> PlayerId -> SettleState -> Either Text SettleState
maybeEmitActiveSettleSource players eventIx pid state =
  case currentSettleSource state of
    Nothing -> pure state
    Just source
      | settleSourceApplies pid source
          && Map.findWithDefault 0 pid (settleSourceChoices state) < settleSourceStep source ->
          emitSettleChoice players eventIx pid (Just (settleSourceName source)) (Just source) state
      | otherwise -> pure state

settleSourceApplies :: PlayerId -> SettleSource -> Bool
settleSourceApplies pid source =
  case settleSourcePlayer source of
    Nothing -> True
    Just sourcePid -> sourcePid == pid

emitUpgrade ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  PlayerId ->
  Text ->
  Text ->
  SettleState ->
  Either Text SettleState
emitUpgrade players cardInfosByName eventIx pid newWorld oldWorld state = do
  sourceName <- settleReplaceSource cardInfosByName pid state
  stateWithSettle <- emitSettleChoice players eventIx pid (Just sourceName) Nothing state
  player <- lookupPlayer players pid
  order <- cursorChoiceOrder (phaseCursor stateWithSettle) eventIx
  let line = Choice Required (playerSeat player) (ChooseUpgrade newWorld oldWorld)
      script = choiceScriptAt order (playerSeat player) [line]
  pure (addTableauCard pid newWorld stateWithSettle)
    { pendingUpgrades = Map.delete pid (pendingUpgrades stateWithSettle)
    , settleScript = settleScript stateWithSettle `appendScript` script
    }

settleReplaceSource :: Map Text CardTypeInfo -> PlayerId -> SettleState -> Either Text Text
settleReplaceSource cardInfosByName pid state =
  case filter hasSettleReplace (Map.findWithDefault [] pid (tableauCards state)) of
    [source] -> pure source
    [] -> Left ("upgrade has no settle-replace source for player " <> showText (unPlayerId pid))
    sources ->
      Left
        ( "upgrade has multiple settle-replace sources for player "
            <> showText (unPlayerId pid)
            <> ": "
            <> Text.intercalate ", " sources
        )
  where
    hasSettleReplace name =
      maybe False cardTypeHasSettleReplace (Map.lookup name cardInfosByName)

emitSettleChoice ::
  [Player] ->
  Int ->
  PlayerId ->
  Maybe Text ->
  Maybe SettleSource ->
  SettleState ->
  Either Text SettleState
emitSettleChoice players eventIx pid chosen source state = do
  player <- lookupPlayer players pid
  order <- cursorChoiceOrder (phaseCursor state) eventIx
  let line = Choice Optional (playerSeat player) (ChooseSettle chosen)
      script = choiceScriptAt order (playerSeat player) [line]
      choiceStep =
        case source of
          Just settleSource -> settleSourceStep settleSource
          Nothing -> Map.findWithDefault 0 pid (settleSourceChoices state)
  pure state
    { settleSourceChoices = Map.insert pid choiceStep (settleSourceChoices state)
    , settleScript = settleScript state `appendScript` script
    }

addTableauCard :: PlayerId -> Text -> SettleState -> SettleState
addTableauCard pid name state =
  state { tableauCards = Map.alter (Just . appendUnique name . maybe [] id) pid (tableauCards state) }

appendUnique :: Eq a => a -> [a] -> [a]
appendUnique value values =
  if value `elem` values
    then values
    else values <> [value]

cardPlayerId :: Value -> Either Text PlayerId
cardPlayerId value = do
  cardObject <- expectObject "card" value
  PlayerId <$> (intValue "card location_arg" =<< field "location_arg" cardObject)

lookupCardOwner :: Int -> SettleState -> Either Text PlayerId
lookupCardOwner cardInstanceId state =
  case Map.lookup cardInstanceId (knownCardOwners (cardIndex state)) of
    Just owner -> pure owner
    Nothing -> Left ("card owner unknown for " <> showText cardInstanceId)

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

ensureNoPendingUpgrades :: SettleState -> Either Text ()
ensureNoPendingUpgrades state =
  case Map.toList (pendingUpgrades state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved upgrades: "
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
