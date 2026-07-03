{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Draw
  ( parseDraws
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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
import Rftg.Bga.State
  ( BgaPhase (..)
  , BgaState
  , bgaStateIsExploreStart
  , bgaStateIsSearchDone
  , bgaStateIsSearchStart
  , bgaStateHasPhase
  , bgaStateLeavesPhase
  , optionalBgaStateField
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  )
import Rftg.Keldon.Script
  ( KeldonScript
  , ScriptLine (..)
  , drawScript
  )
import Rftg.Parser.Common
  ( cardId
  , cardName
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )
import Rftg.Parser.Setup
  ( gamedataFor
  , parseInitialStartWorlds
  , parseStartOptions
  , sortCards
  )

data Phase
  = Explore
  | OtherPhase
  deriving stock (Eq, Show)

data LoggedDraw = LoggedDraw PlayerId [Value]
  deriving stock (Eq, Show)

data DrawState = DrawState
  { drawsByPlayer :: Map PlayerId [Text]
  , activeSearch :: Maybe PlayerId
  , pendingExplores :: [[Value]]
  , pendingLoggedDraws :: [LoggedDraw]
  , scavengerPile :: Set Int
  , currentPhase :: Phase
  }
  deriving stock (Eq, Show)

parseDraws :: Value -> Either Text KeldonScript
parseDraws rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startOptions <- parseStartOptions players gamedatas cardTypes
  startWorlds <- parseInitialStartWorlds root cardTypes
  notifications <- notificationObjects root
  initialState <- initialDrawState players gamedatas cardTypes startOptions startWorlds
  finalState <- foldM (drawStep cardTypes players) initialState notifications
  checkedState <- finishDrawState finalState
  pure $ drawScript (renderDraws players checkedState)

initialDrawState ::
  [Player] ->
  Object ->
  Map Int Text ->
  Map PlayerId [Text] ->
  Map PlayerId Text ->
  Either Text DrawState
initialDrawState players gamedatas cardTypes startOptions startWorlds = do
  entries <- traverse playerInitialDraws players
  pure DrawState
    { drawsByPlayer = Map.fromList entries
    , activeSearch = Nothing
    , pendingExplores = []
    , pendingLoggedDraws = []
    , scavengerPile = Set.empty
    , currentPhase = OtherPhase
    }
  where
    playerInitialDraws player = do
      gamedata <- gamedataFor gamedatas (playerId player)
      handObject <- objectField "hand" gamedata
      handNames <- traverse (cardName cardTypes) (sortCards (objectValues handObject))
      let forcedStartDraw =
            if Map.member (playerId player) startOptions
              then []
              else maybe [] pure (Map.lookup (playerId player) startWorlds)
      pure (playerId player, forcedStartDraw <> handNames)

renderDraws :: [Player] -> DrawState -> [ScriptLine]
renderDraws players state =
  [ Draw (playerSeat player) name
  | player <- players
  , name <- Map.findWithDefault [] (playerId player) (drawsByPlayer state)
  ]

drawStep :: Map Int Text -> [Player] -> DrawState -> Object -> Either Text DrawState
drawStep cardTypes players state notification =
  case notificationType notification of
    "gameStateChange" -> handleGameState cardTypes players state notification
    "revealCard" -> handleRevealCard players state notification
    "explored_choice" -> handleExploredChoice cardTypes state notification
    "drawCards" -> handleDrawCards state notification
    "drawCards_def" -> handleDrawCardsDef cardTypes players state notification
    "explored_choice_log" -> handleExploredChoiceLog cardTypes state notification
    "discard" -> handleDiscard cardTypes state notification
    "scavengerUpdate" -> handleScavengerUpdate state notification
    "scavengeFromExplore" -> handleScavengerUpdate state notification
    _ -> pure state

handleGameState :: Map Int Text -> [Player] -> DrawState -> Object -> Either Text DrawState
handleGameState cardTypes players state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "game state id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      searchedState <- updateActiveSearch players bgaState args state
      case () of
        _
          | bgaStateIsExploreStart bgaState ->
              flushPendingExplores cardTypes searchedState { currentPhase = Explore }
          | bgaStateHasPhase BgaExplore bgaState ->
              pure searchedState { currentPhase = Explore }
          | bgaStateLeavesPhase BgaExplore bgaState ->
              pure searchedState { currentPhase = OtherPhase }
          | otherwise ->
              pure searchedState

updateActiveSearch :: [Player] -> BgaState -> Object -> DrawState -> Either Text DrawState
updateActiveSearch players bgaState args state
  | bgaStateIsSearchStart bgaState = do
      pid <- activePidFromState players args
      case activeSearch state of
        Nothing -> pure state { activeSearch = Just pid }
        Just current ->
          Left
            ( "nested Search state for player "
                <> showText (unPlayerId current)
                <> " while entering Search for "
                <> showText (unPlayerId pid)
            )
  | bgaStateIsSearchDone bgaState = do
      pid <- activePidFromState players args
      case activeSearch state of
        Nothing ->
          Left ("Search done for player " <> showText (unPlayerId pid) <> " without active Search")
        Just current
          | current == pid -> pure state
          | otherwise ->
              Left
                ( "Search done for player "
                    <> showText (unPlayerId pid)
                    <> " while "
                    <> showText (unPlayerId current)
                    <> " is active"
                )
  | otherwise =
      pure state { activeSearch = Nothing }

activePidFromState :: [Player] -> Object -> Either Text PlayerId
activePidFromState players args = do
  pid <- PlayerId <$> (intValue "active_player" =<< field "active_player" args)
  if pid `Set.member` playerIds players
    then pure pid
    else Left ("state has unknown active player " <> showText (unPlayerId pid))

playerIds :: [Player] -> Set PlayerId
playerIds = Set.fromList . fmap playerId

handleRevealCard :: [Player] -> DrawState -> Object -> Either Text DrawState
handleRevealCard players state notification =
  case activeSearch state of
    Nothing -> Left "revealCard notification outside active Search"
    Just searchPid -> do
      args <- objectField "args" notification
      playerName <- textValue "revealCard player_name" =<< field "player_name" args
      pid <- case Map.lookup playerName (playersByName players) of
        Just found -> pure found
        Nothing -> Left ("revealCard has unknown player " <> playerName)
      if pid /= searchPid
        then
          Left
            ( "Search reveal for player "
                <> showText (unPlayerId pid)
                <> " while "
                <> showText (unPlayerId searchPid)
                <> " is active"
            )
        else do
          name <- canonicalCardName <$> (textValue "revealCard reveal" =<< field "reveal" args)
          pure (addDraw pid name state)

playersByName :: [Player] -> Map Text PlayerId
playersByName players =
  Map.fromList [(playerName player, playerId player) | player <- players]

handleExploredChoice :: Map Int Text -> DrawState -> Object -> Either Text DrawState
handleExploredChoice cardTypes state notification = do
  cards <- cardsFromNotification "explored_choice" notification
  case currentPhase state of
    Explore -> recordExploredChoice cardTypes state cards
    OtherPhase ->
      pure state { pendingExplores = pendingExplores state <> [cards] }

flushPendingExplores :: Map Int Text -> DrawState -> Either Text DrawState
flushPendingExplores cardTypes state = do
  flushed <- foldM (recordExploredChoice cardTypes) state (pendingExplores state)
  pure flushed { pendingExplores = [] }

recordExploredChoice :: Map Int Text -> DrawState -> [Value] -> Either Text DrawState
recordExploredChoice cardTypes state cards = do
  pid <- cardsPlayerId "explored_choice" cards
  names <- traverse (cardName cardTypes) cards
  pure (addDraws pid names state)

handleDrawCards :: DrawState -> Object -> Either Text DrawState
handleDrawCards state notification = do
  cards <- cardsFromNotification "drawCards" notification
  if null cards
    then Left "drawCards notification has no cards"
    else do
      location <- cardsLocation "drawCards" cards
      case location of
        "aside" ->
          case activeSearch state of
            Just _
              | length cards == 1 -> pure state
              | otherwise -> Left "Search kept multiple aside cards"
            Nothing -> Left "drawCards location aside outside active Search"
        "retrofit" -> pure state
        "hand" -> queueHandDrawCards state cards
        other -> Left ("unexpected drawCards location " <> other)

queueHandDrawCards :: DrawState -> [Value] -> Either Text DrawState
queueHandDrawCards state cards = do
  pid <- cardsPlayerId "drawCards" cards
  ids <- traverse cardId cards
  let inScavengerPile = fmap (`Set.member` scavengerPile state) ids
  case (and inScavengerPile, or inScavengerPile) of
    (True, _) ->
      pure state { scavengerPile = foldr Set.delete (scavengerPile state) ids }
    (False, True) ->
      Left ("drawCards mixes scavenger-pile and deck cards: " <> showText ids)
    (False, False) ->
      pure state { pendingLoggedDraws = pendingLoggedDraws state <> [LoggedDraw pid cards] }

handleDrawCardsDef :: Map Int Text -> [Player] -> DrawState -> Object -> Either Text DrawState
handleDrawCardsDef cardTypes players state notification = do
  args <- objectField "args" notification
  case optionalField "card_nbr" args of
    Nothing -> pure state
    Just countValue -> do
      count <- intValue "drawCards_def card_nbr" countValue
      playerName <- textValue "drawCards_def player_name" =<< field "player_name" args
      pid <- case Map.lookup playerName (playersByName players) of
        Just found -> pure found
        Nothing -> Left ("drawCards_def has unknown player " <> playerName)
      flushLoggedDraw cardTypes pid count state

handleExploredChoiceLog :: Map Int Text -> DrawState -> Object -> Either Text DrawState
handleExploredChoiceLog cardTypes state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "explored_choice_log player_id" =<< field "player_id" args)
  count <- intValue "explored_choice_log nbr" =<< field "nbr" args
  flushLoggedDraw cardTypes pid count state

flushLoggedDraw :: Map Int Text -> PlayerId -> Int -> DrawState -> Either Text DrawState
flushLoggedDraw cardTypes pid count state =
  case takeLoggedDraw pid count (pendingLoggedDraws state) of
    Nothing -> pure state
    Just (cards, remaining) -> do
      names <- traverse (cardName cardTypes) cards
      pure (addDraws pid names state { pendingLoggedDraws = remaining })

takeLoggedDraw :: PlayerId -> Int -> [LoggedDraw] -> Maybe ([Value], [LoggedDraw])
takeLoggedDraw pid count = go []
  where
    go _ [] = Nothing
    go before (item@(LoggedDraw itemPid cards) : rest)
      | itemPid == pid && length cards == count =
          Just (cards, reverse before <> rest)
      | otherwise =
          go (item : before) rest

handleDiscard :: Map Int Text -> DrawState -> Object -> Either Text DrawState
handleDiscard cardTypes state notification =
  case activeSearch state of
    Just _ -> pure state
    Nothing -> do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      case takeQueuedDrawCards (Set.fromList cardIds) (pendingLoggedDraws state) of
        Nothing -> pure state
        Just (pid, cards, remaining) -> do
          names <- traverse (cardName cardTypes) cards
          pure (addDraws pid names state { pendingLoggedDraws = remaining })

takeQueuedDrawCards :: Set Int -> [LoggedDraw] -> Maybe (PlayerId, [Value], [LoggedDraw])
takeQueuedDrawCards ids = go []
  where
    go _ [] = Nothing
    go before (item@(LoggedDraw pid cards) : rest) =
      let (found, remainingCards) =
            foldr splitCard ([], []) cards
       in if length found == Set.size ids
            then
              let remainingLogged =
                    if null remainingCards
                      then reverse before <> rest
                      else reverse before <> [LoggedDraw pid remainingCards] <> rest
               in Just (pid, found, remainingLogged)
            else go (item : before) rest

    splitCard card (found, remaining) =
      case cardId card of
        Right cid
          | cid `Set.member` ids -> (card : found, remaining)
        _ -> (found, card : remaining)

discardCardIds :: Object -> Either Text [Int]
discardCardIds args = do
  cardsValue <- field "cards" args
  case cardsValue of
    Array values ->
      traverse (intValue "discard card id") (Vector.toList values)
    _ -> Left "discard.cards is not an array"

handleScavengerUpdate :: DrawState -> Object -> Either Text DrawState
handleScavengerUpdate state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardObject <- expectObject "scavenger card" cardValue
      case optionalField "id" cardObject of
        Nothing -> pure state
        Just idValue -> do
          cid <- intValue "scavenger card id" idValue
          pure state { scavengerPile = Set.insert cid (scavengerPile state) }

cardsFromNotification :: Text -> Object -> Either Text [Value]
cardsFromNotification label notification =
  cardsFromValue label =<< field "args" notification

cardsFromValue :: Text -> Value -> Either Text [Value]
cardsFromValue label value =
  case value of
    Array cards -> pure (Vector.toList cards)
    Object cards -> pure (objectValues cards)
    _ -> Left (label <> " args is not a card array or object")

cardsLocation :: Text -> [Value] -> Either Text Text
cardsLocation label cards = do
  locations <- traverse cardLocation cards
  case locations of
    [] -> Left (label <> " has no cards")
    firstLocation : rest
      | all (== firstLocation) rest -> pure firstLocation
      | otherwise -> Left (label <> " has mixed card locations: " <> Text.intercalate ", " locations)

cardLocation :: Value -> Either Text Text
cardLocation value = do
  cardObject <- expectObject "card" value
  textValue "card location" =<< field "location" cardObject

cardsPlayerId :: Text -> [Value] -> Either Text PlayerId
cardsPlayerId label cards = do
  pids <- traverse cardPlayerId cards
  case pids of
    [] -> Left (label <> " has no cards")
    firstPid : rest
      | all (== firstPid) rest -> pure firstPid
      | otherwise ->
          Left
            ( label
                <> " has mixed card owners: "
                <> Text.intercalate ", " (fmap (showText . unPlayerId) pids)
            )

cardPlayerId :: Value -> Either Text PlayerId
cardPlayerId value = do
  cardObject <- expectObject "card" value
  PlayerId <$> (intValue "card location_arg" =<< field "location_arg" cardObject)

addDraw :: PlayerId -> Text -> DrawState -> DrawState
addDraw pid name state =
  addDraws pid [name] state

addDraws :: PlayerId -> [Text] -> DrawState -> DrawState
addDraws pid names state =
  state { drawsByPlayer = Map.alter appendNames pid (drawsByPlayer state) }
  where
    appendNames Nothing = Just names
    appendNames (Just oldNames) = Just (oldNames <> names)

finishDrawState :: DrawState -> Either Text DrawState
finishDrawState state
  | not (null (pendingExplores state)) =
      Left ("unresolved explore choices: " <> showText (pendingExplores state))
  | not (null (pendingLoggedDraws state)) =
      Left ("unresolved logged draws: " <> showText (pendingLoggedDraws state))
  | activeSearch state /= Nothing =
      Left ("unresolved active Search: " <> showText (activeSearch state))
  | otherwise =
      pure state

showText :: Show a => a -> Text
showText = Text.pack . show
