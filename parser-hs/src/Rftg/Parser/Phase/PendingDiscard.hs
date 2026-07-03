{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.PendingDiscard
  ( parseOptionalDiscardChoices
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
  , optionalField
  , expectObject
  , textValue
  )
import Rftg.Bga.State
  ( BgaState
  , bgaStateIsNewActionRound
  , bgaStateIsSearch
  , bgaStateIsSettleMain
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
  ( cardId
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypes
  , parsePlayers
  )
import Rftg.Parser.Phase.Cursor
  ( Phase (..)
  , PhaseCursor (..)
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data PendingDiscard = PendingDiscard
  { pendingDiscardIds :: [Int]
  , pendingDiscardCards :: [Text]
  }
  deriving stock (Eq, Show)

data QueuedDraw = QueuedDraw
  { queuedDrawPlayer :: PlayerId
  , queuedDrawIds :: [Int]
  }
  deriving stock (Eq, Show)

data OptionalDiscardState = OptionalDiscardState
  { phaseCursor :: PhaseCursor
  , currentBgaState :: Maybe BgaState
  , activeSearch :: Bool
  , cardIndex :: CardIndex
  , queuedDraws :: [QueuedDraw]
  , pendingGamblingDiscards :: Map PlayerId PendingDiscard
  , pendingDiscards :: Map PlayerId PendingDiscard
  , optionalDiscardScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseOptionalDiscardChoices :: Value -> Either Text KeldonScript
parseOptionalDiscardChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypes <- parseCardTypes gamedatas
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  walked <-
    foldM
      (optionalDiscardStep players cardTypes)
      (emptyOptionalDiscardState startingCardIndex)
      (zip [0 :: Int ..] notifications)
  ensureNoPendingGambling walked
  finalState <- flushAllPending players (length notifications) walked
  pure (optionalDiscardScript finalState)

emptyOptionalDiscardState :: CardIndex -> OptionalDiscardState
emptyOptionalDiscardState startingCardIndex = OptionalDiscardState
  { phaseCursor = initialPhaseCursor
  , currentBgaState = Nothing
  , activeSearch = False
  , cardIndex = startingCardIndex
  , queuedDraws = []
  , pendingGamblingDiscards = Map.empty
  , pendingDiscards = Map.empty
  , optionalDiscardScript = emptyScript
  }

optionalDiscardStep ::
  [Player] ->
  Map Int Text ->
  OptionalDiscardState ->
  (Int, Object) ->
  Either Text OptionalDiscardState
optionalDiscardStep players cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState players eventIx stateWithCards notification
    "drawCards" -> queueLoggedDraw stateWithCards notification
    "discard" -> handleDiscard players eventIx stateWithCards notification
    "consumecard" -> clearNotificationPlayer "consumecard player_id" stateWithCards notification
    "gambling" -> handleGambling stateWithCards notification
    "playcard" -> flushPlayCardPlayer players eventIx stateWithCards notification
    "keepcards" -> flushKeepCardsPlayer players eventIx stateWithCards notification
    "consume" -> flushNotificationPlayer "consume player_id" players eventIx stateWithCards notification
    "drawCards_def" -> flushDrawCardsDefPlayer players eventIx stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: [Player] -> Int -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
handleGameState players eventIx state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState -> do
      flushed <-
        if bgaStateIsNewActionRound bgaState
          then flushAllPending players eventIx state
          else pure state
      pure flushed
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor flushed)
        , currentBgaState = Just bgaState
        , activeSearch = bgaStateIsSearch bgaState
        }

handleDiscard :: [Player] -> Int -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
handleDiscard players eventIx state notification =
  if not (capturesPendingDiscard state)
    then pure state
    else do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
      case takeQueuedDrawCards cardIds (queuedDraws state) of
        Just (owner, remainingQueuedDraws) ->
          if Map.member owner (pendingGamblingDiscards state)
            then Left ("duplicate pending Gambling discard for player " <> showText (unPlayerId owner))
            else
              pure state
                { queuedDraws = remainingQueuedDraws
                , pendingGamblingDiscards =
                    Map.insert owner (PendingDiscard cardIds cards) (pendingGamblingDiscards state)
                }
        Nothing -> do
          owner <- discardOwner (cardIndex state) cardIds
          flushed <- flushPlayerPending players eventIx owner state
          pure flushed
            { pendingDiscards =
                Map.insert owner (PendingDiscard cardIds cards) (pendingDiscards flushed)
            }

capturesPendingDiscard :: OptionalDiscardState -> Bool
capturesPendingDiscard state =
  case (cursorRound (phaseCursor state), cursorPhase (phaseCursor state)) of
    (Nothing, _) -> False
    (_, Explore) -> False
    (_, Produce) -> False
    (_, Discard) -> False
    _
      | maybe False bgaStateIsSettleMain (currentBgaState state) -> False
      | otherwise -> not (activeSearch state)

queueLoggedDraw :: OptionalDiscardState -> Object -> Either Text OptionalDiscardState
queueLoggedDraw state notification = do
  cards <- cardsFromNotification "drawCards" notification
  pid <- cardsPlayerId "drawCards" cards
  cardIds <- traverse cardId cards
  pure state { queuedDraws = queuedDraws state <> [QueuedDraw pid cardIds] }

takeQueuedDrawCards :: [Int] -> [QueuedDraw] -> Maybe (PlayerId, [QueuedDraw])
takeQueuedDrawCards cardIds = go []
  where
    go _ [] = Nothing
    go seen (draw : rest)
      | all (`elem` queuedDrawIds draw) cardIds =
          let remainingIds = filter (`notElem` cardIds) (queuedDrawIds draw)
              remainingDraws =
                seen
                  <> [draw { queuedDrawIds = remainingIds } | not (null remainingIds)]
                  <> rest
           in Just (queuedDrawPlayer draw, remainingDraws)
      | otherwise = go (seen <> [draw]) rest

handleGambling :: OptionalDiscardState -> Object -> Either Text OptionalDiscardState
handleGambling state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "gambling player_id" =<< field "player_id" args)
  flipped <- canonicalCardName <$> (textValue "gambling card_name" =<< field "card_name" args)
  case Map.lookup pid (pendingGamblingDiscards state) of
    Just transient -> do
      unlessSingleMatching "Gambling discard" flipped transient
      pure state { pendingGamblingDiscards = Map.delete pid (pendingGamblingDiscards state) }
    Nothing ->
      case Map.lookup pid (pendingDiscards state) of
        Just pending
          | singlePendingCardMatches flipped pending ->
              pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }
        _ -> pure state

unlessSingleMatching :: Text -> Text -> PendingDiscard -> Either Text ()
unlessSingleMatching label flipped pending =
  case pendingDiscardCards pending of
    [card]
      | canonicalCardName card == flipped -> pure ()
    cards ->
      Left
        ( label
            <> " "
            <> Text.intercalate ", " cards
            <> " does not match flipped card "
            <> flipped
        )

singlePendingCardMatches :: Text -> PendingDiscard -> Bool
singlePendingCardMatches flipped pending =
  case pendingDiscardCards pending of
    [card] -> canonicalCardName card == flipped
    _ -> False

ensureNoPendingGambling :: OptionalDiscardState -> Either Text ()
ensureNoPendingGambling state =
  case Map.toList (pendingGamblingDiscards state) of
    [] -> pure ()
    pending ->
      Left
        ( "unresolved Gambling discards: "
            <> Text.intercalate
              ", "
              [ showText (unPlayerId pid) <> "=" <> Text.intercalate "/" (pendingDiscardCards discard)
              | (pid, discard) <- pending
              ]
        )

flushPlayCardPlayer :: [Player] -> Int -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
flushPlayCardPlayer players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player" args of
    Nothing -> pure state
    Just pidValue -> do
      pid <- PlayerId <$> intValue "playcard player" pidValue
      flushPlayerPending players eventIx pid state

flushKeepCardsPlayer :: [Player] -> Int -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
flushKeepCardsPlayer players eventIx state notification = do
  cards <- cardsFromNotification "keepcards" notification
  pid <- cardsPlayerId "keepcards" cards
  flushPlayerPending players eventIx pid state

flushNotificationPlayer ::
  Text ->
  [Player] ->
  Int ->
  OptionalDiscardState ->
  Object ->
  Either Text OptionalDiscardState
flushNotificationPlayer label players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player_id" args of
    Nothing -> pure state
    Just pidValue -> do
      pid <- PlayerId <$> intValue label pidValue
      flushPlayerPending players eventIx pid state

flushDrawCardsDefPlayer :: [Player] -> Int -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
flushDrawCardsDefPlayer players eventIx state notification = do
  args <- objectField "args" notification
  case optionalField "player_name" args of
    Nothing -> pure state
    Just playerNameValue -> do
      playerName <- textValue "drawCards_def player_name" playerNameValue
      player <- lookupPlayerByName players playerName
      let pid = playerId player
      stateWithoutLoggedDraw <-
        case optionalField "card_nbr" args of
          Nothing -> pure state
          Just countValue -> do
            count <- intValue "drawCards_def card_nbr" countValue
            pure (flushLoggedDraw pid count state)
      flushPlayerPending players eventIx pid stateWithoutLoggedDraw

flushLoggedDraw :: PlayerId -> Int -> OptionalDiscardState -> OptionalDiscardState
flushLoggedDraw pid count state =
  state { queuedDraws = dropFirstMatchingDraw [] (queuedDraws state) }
  where
    dropFirstMatchingDraw seen [] = seen
    dropFirstMatchingDraw seen (draw : rest)
      | queuedDrawPlayer draw == pid && length (queuedDrawIds draw) == count = seen <> rest
      | otherwise = dropFirstMatchingDraw (seen <> [draw]) rest

clearNotificationPlayer :: Text -> OptionalDiscardState -> Object -> Either Text OptionalDiscardState
clearNotificationPlayer label state notification = do
  args <- objectField "args" notification
  case optionalField "player_id" args of
    Nothing -> pure state
    Just pidValue -> do
      pid <- PlayerId <$> intValue label pidValue
      pure state { pendingDiscards = Map.delete pid (pendingDiscards state) }

flushAllPending :: [Player] -> Int -> OptionalDiscardState -> Either Text OptionalDiscardState
flushAllPending players eventIx state =
  foldM
    (\current pid -> flushPlayerPending players eventIx pid current)
    state
    (Map.keys (pendingDiscards state))

flushPlayerPending :: [Player] -> Int -> PlayerId -> OptionalDiscardState -> Either Text OptionalDiscardState
flushPlayerPending players eventIx pid state =
  case Map.lookup pid (pendingDiscards state) of
    Nothing -> pure state
    Just pending -> do
      player <- lookupPlayer players pid
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      let line = Choice Optional (playerSeat player) (ChooseDiscard (pendingDiscardCards pending))
          script = choiceScriptAt order (playerSeat player) [line]
      pure state
        { pendingDiscards = Map.delete pid (pendingDiscards state)
        , optionalDiscardScript = optionalDiscardScript state `appendScript` script
        }

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

showText :: Show a => a -> Text
showText = Text.pack . show
