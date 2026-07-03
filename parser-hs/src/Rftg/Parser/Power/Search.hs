{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Power.Search
  ( parseSearchChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , field
  , intValue
  , objectField
  , expectObject
  , textValue
  )
import Rftg.Bga.State
  ( bgaStateIsSearchDone
  , bgaStateIsSearchStart
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
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , cardId
  , cardName
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

data SearchProgress = SearchProgress
  { searchPlayer :: PlayerId
  , revealedCards :: [Text]
  , offeredCards :: [Text]
  , currentOffer :: Maybe (Text, Int)
  }
  deriving stock (Eq, Show)

data SearchState = SearchState
  { phaseCursor :: PhaseCursor
  , activeSearch :: Maybe SearchProgress
  , searchScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseSearchChoices :: Value -> Either Text KeldonScript
parseSearchChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypeInfos <- parseCardTypeInfos gamedatas
  let cardTypes = fmap cardTypeName cardTypeInfos
      cardInfosByName = Map.fromList [(cardTypeName info, info) | info <- Map.elems cardTypeInfos]
  notifications <- notificationObjects root
  walked <-
    foldM
      (searchStep players cardInfosByName cardTypes)
      emptySearchState
      (zip [0 :: Int ..] notifications)
  finalState <- finishActiveSearch players cardInfosByName (length notifications) walked
  pure (searchScript finalState)

emptySearchState :: SearchState
emptySearchState = SearchState
  { phaseCursor = initialPhaseCursor
  , activeSearch = Nothing
  , searchScript = emptyScript
  }

searchStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  SearchState ->
  (Int, Object) ->
  Either Text SearchState
searchStep players cardInfosByName cardTypes state (eventIx, notification) =
  case notificationType notification of
    "gameStateChange" -> handleGameState players cardInfosByName eventIx state notification
    "revealCard" -> handleRevealCard players state notification
    "drawCards" -> handleDrawCards cardTypes state notification
    "discard" -> handleDiscard state notification
    _ -> pure state

handleGameState ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  SearchState ->
  Object ->
  Either Text SearchState
handleGameState players cardInfosByName eventIx state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState
      | bgaStateIsSearchStart bgaState -> do
          pid <- activePidFromState players args
          case activeSearch state of
            Nothing ->
              pure state
                { activeSearch = Just (emptySearchProgress pid)
                , phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
                }
            Just current ->
              Left
                ( "nested Search state for player "
                    <> showText (unPlayerId (searchPlayer current))
                    <> " while entering Search for "
                    <> showText (unPlayerId pid)
                )
      | bgaStateIsSearchDone bgaState -> do
          pid <- activePidFromState players args
          case activeSearch state of
            Nothing -> Left ("Search done for player " <> showText (unPlayerId pid) <> " without active Search")
            Just current
              | searchPlayer current == pid ->
                  pure state { phaseCursor = advancePhaseCursor bgaState (phaseCursor state) }
              | otherwise ->
                  Left
                    ( "Search done for player "
                        <> showText (unPlayerId pid)
                        <> " while "
                        <> showText (unPlayerId (searchPlayer current))
                        <> " is active"
                    )
      | otherwise -> do
          finished <- finishActiveSearch players cardInfosByName eventIx state
          pure finished { phaseCursor = advancePhaseCursor bgaState (phaseCursor finished) }

emptySearchProgress :: PlayerId -> SearchProgress
emptySearchProgress pid = SearchProgress
  { searchPlayer = pid
  , revealedCards = []
  , offeredCards = []
  , currentOffer = Nothing
  }

activePidFromState :: [Player] -> Object -> Either Text PlayerId
activePidFromState players args = do
  pid <- PlayerId <$> (intValue "active_player" =<< field "active_player" args)
  if pid `Set.member` Set.fromList (fmap playerId players)
    then pure pid
    else Left ("state has unknown active player " <> showText (unPlayerId pid))

handleRevealCard :: [Player] -> SearchState -> Object -> Either Text SearchState
handleRevealCard players state notification =
  case activeSearch state of
    Nothing -> pure state
    Just progress -> do
      args <- objectField "args" notification
      playerName <- textValue "revealCard player_name" =<< field "player_name" args
      pid <-
        case Map.lookup playerName (playersByName players) of
          Just found -> pure found
          Nothing -> Left ("revealCard has unknown player " <> playerName)
      if pid /= searchPlayer progress
        then
          Left
            ( "Search reveal for player "
                <> showText (unPlayerId pid)
                <> " while "
                <> showText (unPlayerId (searchPlayer progress))
                <> " is active"
            )
        else do
          name <- canonicalCardName <$> (textValue "revealCard reveal" =<< field "reveal" args)
          pure state { activeSearch = Just progress { revealedCards = revealedCards progress <> [name] } }

playersByName :: [Player] -> Map Text PlayerId
playersByName players =
  Map.fromList [(playerName player, playerId player) | player <- players]

handleDrawCards :: Map Int Text -> SearchState -> Object -> Either Text SearchState
handleDrawCards cardTypes state notification =
  case activeSearch state of
    Nothing -> pure state
    Just progress -> do
      cards <- cardsFromNotification "drawCards" notification
      locations <- traverse cardLocation cards
      if not ("aside" `elem` locations)
        then pure state
        else case cards of
          [cardValue] -> do
            location <- cardLocation cardValue
            if location /= "aside"
              then Left ("Search drawCards mixes aside and non-aside cards: " <> showText locations)
              else do
                name <- cardName cardTypes cardValue
                cid <- cardId cardValue
                pure state
                  { activeSearch =
                      Just progress
                        { offeredCards = offeredCards progress <> [name]
                        , currentOffer = Just (name, cid)
                        }
                  }
          [] -> Left "drawCards notification has no cards"
          _ -> Left "Search kept multiple aside cards"

handleDiscard :: SearchState -> Object -> Either Text SearchState
handleDiscard state notification =
  case activeSearch state of
    Nothing -> pure state
    Just progress -> do
      args <- objectField "args" notification
      cardsValue <- field "cards" args
      cardIds <-
        case cardsValue of
          Array values -> traverse (intValue "discard card id") (toList values)
          _ -> Left "discard.cards is not an array"
      case (cardIds, currentOffer progress) of
        ([discardedId], Just (_, offerId))
          | discardedId == offerId ->
              pure state { activeSearch = Just progress { currentOffer = Nothing } }
        _ -> Left ("unexpected discard during Search: " <> showText cardIds)

finishActiveSearch ::
  [Player] ->
  Map Text CardTypeInfo ->
  Int ->
  SearchState ->
  Either Text SearchState
finishActiveSearch players cardInfosByName eventIx state =
  case activeSearch state of
    Nothing -> pure state
    Just progress -> do
      (category, keepChoices) <- searchScriptFor cardInfosByName progress
      player <- lookupPlayer players (searchPlayer progress)
      order <- cursorChoiceOrder (phaseCursor state) eventIx
      let lines_ =
            Choice Required (playerSeat player) (ChooseSearchType category)
              : fmap
                (Choice Required (playerSeat player) . ChooseSearchKeep)
                keepChoices
          script = choiceScriptAt order (playerSeat player) lines_
      pure state
        { activeSearch = Nothing
        , searchScript = searchScript state `appendScript` script
        }

searchScriptFor :: Map Text CardTypeInfo -> SearchProgress -> Either Text (Int, [Int])
searchScriptFor cardInfosByName progress = do
  kept <- keptCard progress
  if null (revealedCards progress)
    then Left "Search ended without reveals"
    else pure ()
  if null (offeredCards progress)
    then Left "Search ended without offered cards"
    else pure ()
  if last (offeredCards progress) /= kept
    then Left ("Search kept card " <> kept <> " is not final offered card")
    else pure ()
  if kept `notElem` revealedCards progress
    then Left ("Search kept card " <> kept <> " was not revealed")
    else pure ()
  candidates <- foldM candidateForCategory [] [0 .. 8]
  case candidates of
    [] ->
      Left
        ( "Search revealed cards are incompatible with BGA-offered matches "
            <> showText (offeredCards progress)
            <> ": "
            <> showText (revealedCards progress)
        )
    _ -> pure (minimumByCategory candidates)
  where
    candidateForCategory candidates category = do
      offeredMatches <- traverse (`searchCategoryMatch` category) (offeredCards progress)
      if not (and offeredMatches)
        then pure candidates
        else do
          matchEvents <- searchMatchEvents cardInfosByName category (revealedCards progress) (offeredCards progress)
          case matchEvents of
            Nothing -> pure candidates
            Just events -> do
              keepChoices <- searchKeepChoices cardInfosByName category events
              case keepChoices of
                Nothing -> pure candidates
                Just choices -> pure (candidates <> [(category, choices)])

    searchCategoryMatch name category =
      categoryMatches cardInfosByName category name

keptCard :: SearchProgress -> Either Text Text
keptCard progress =
  case currentOffer progress of
    Just (name, _) -> pure name
    Nothing -> Left "Search ended without kept card"

searchMatchEvents ::
  Map Text CardTypeInfo ->
  Int ->
  [Text] ->
  [Text] ->
  Either Text (Maybe [(Text, Bool)])
searchMatchEvents cardInfosByName category revealed offered =
  go [] 0 revealed
  where
    go events offeredIx [] =
      if offeredIx == length offered
        then pure (Just events)
        else pure Nothing
    go events offeredIx (name : rest)
      | offeredIx < length offered && name == offered !! offeredIx =
          go (events <> [(name, offeredIx == length offered - 1)]) (offeredIx + 1) rest
      | otherwise = do
          matches <- categoryMatches cardInfosByName category name
          if not matches
            then go events offeredIx rest
            else do
              alienAny <- searchAlienAnyMatch cardInfosByName category name
              if alienAny
                then go (events <> [(name, False)]) offeredIx rest
                else pure Nothing

searchKeepChoices :: Map Text CardTypeInfo -> Int -> [(Text, Bool)] -> Either Text (Maybe [Int])
searchKeepChoices cardInfosByName category events =
  go False False [] 0 events
  where
    go _ _ choices _ [] = pure (Just choices)
    go second third choices index ((name, keep) : rest) = do
      alienAny <- searchAlienAnyMatch cardInfosByName category name
      let (second', third') =
            if second && alienAny
              then (False, True)
              else (second, third)
          isLast = index == length events - 1
      if not second'
        then do
          let choice = if keep then 1 else 0
              choices' = choices <> [choice]
          if keep
            then pure (if isLast then Just choices' else Nothing)
            else do
              let secondNext = not (not third' && alienAny)
              go secondNext third' choices' (index + 1) rest
        else
          if not keep
            then pure Nothing
            else pure (if isLast then Just choices else Nothing)

categoryMatches :: Map Text CardTypeInfo -> Int -> Text -> Either Text Bool
categoryMatches cardInfosByName category name = do
  info <- lookupCardInfo cardInfosByName name
  case category of
    0 ->
      pure $
        cardTypeType info == "development"
          && any (`elem` [1, 2]) (cardTypePhase3MilitaryForces info)
    1 ->
      pure $
        isWorld info
          && hasCategory "windfall" info
          && hasCategory "military" info
          && costBetween 1 2 info
    2 ->
      pure $
        isWorld info
          && hasCategory "windfall" info
          && not (hasCategory "military" info)
          && costBetween 1 2 info
    3 ->
      pure (isWorld info && hasCategory "chromosome" info)
    4 ->
      pure (isWorld info && cardTypeGoodKind info `elem` fmap Just [0, 4])
    5 ->
      pure False
    6 ->
      pure (isWorld info && hasCategory "military" info && maybe False (>= 5) (cardTypeCost info))
    7 ->
      pure (cardTypeType info == "development" && cardTypeCost info == Just 6 && cardTypeHasSixDevScoring info)
    8 ->
      pure (cardTypeHasTakeoverSearchPower info)
    _ -> Left ("unknown Search category " <> showText category)

searchAlienAnyMatch :: Map Text CardTypeInfo -> Int -> Text -> Either Text Bool
searchAlienAnyMatch cardInfosByName category name =
  if category /= 4
    then pure False
    else do
      info <- lookupCardInfo cardInfosByName name
      pure (isWorld info && cardTypeGoodKind info == Just 0)

isWorld :: CardTypeInfo -> Bool
isWorld info =
  cardTypeType info == "world"

hasCategory :: Text -> CardTypeInfo -> Bool
hasCategory category info =
  category `Set.member` cardTypeCategories info

costBetween :: Int -> Int -> CardTypeInfo -> Bool
costBetween low high info =
  maybe False (\cost -> low <= cost && cost <= high) (cardTypeCost info)

minimumByCategory :: [(Int, [Int])] -> (Int, [Int])
minimumByCategory [] = error "minimumByCategory called with empty list"
minimumByCategory (candidate : rest) =
  foldl minCategory candidate rest
  where
    minCategory left@(leftCategory, _) right@(rightCategory, _)
      | rightCategory < leftCategory = right
      | otherwise = left

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

cardsFromNotification :: Text -> Object -> Either Text [Value]
cardsFromNotification label notification =
  cardsFromValue label =<< field "args" notification

cardsFromValue :: Text -> Value -> Either Text [Value]
cardsFromValue label value =
  case value of
    Array cards -> pure (toList cards)
    Object cards -> pure (fmap snd (KeyMap.toList cards))
    _ -> Left (label <> " args is not a card array or object")

cardLocation :: Value -> Either Text Text
cardLocation value = do
  cardObject <- expectObject "card" value
  textValue "card location" =<< field "location" cardObject

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

showText :: Show a => a -> Text
showText = Text.pack . show
