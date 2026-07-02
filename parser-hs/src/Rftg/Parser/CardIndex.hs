{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.CardIndex
  ( CardIndex (..)
  , cardsFromNotification
  , cardsPlayerId
  , discardCardIds
  , discardOwner
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  ) where

import Data.Aeson (Value (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  )
import Rftg.Parser.Common
  ( cardId
  , cardName
  , notificationType
  )
import Rftg.Parser.Setup
  ( gamedataFor
  )

data CardIndex = CardIndex
  { knownCardNames :: Map Int Text
  , knownCardOwners :: Map Int PlayerId
  }
  deriving stock (Eq, Show)

initialCardIndex ::
  [Player] ->
  Object ->
  Map Int Text ->
  Either Text CardIndex
initialCardIndex players gamedatas cardTypes =
  splitEntries . concat <$> traverse playerInitialCards players
  where
    playerInitialCards player = do
      gamedata <- gamedataFor gamedatas (playerId player)
      handCards <- cardsFromOptionalZone gamedata "hand"
      hiddenCards <- cardsFromOptionalZone gamedata "hiddentableau"
      traverse (cardIndexEntry (playerId player)) (handCards <> hiddenCards)

    cardsFromOptionalZone gamedata zone =
      case optionalField zone gamedata of
        Nothing -> pure []
        Just (Array values)
          | Vector.null values -> pure []
        Just value -> objectValues <$> expectObject zone value

    cardIndexEntry pid value = do
      cid <- cardId value
      name <- cardName cardTypes value
      pure (cid, name, pid)

    splitEntries entries = CardIndex
      { knownCardNames = Map.fromList [(cid, name) | (cid, name, _) <- entries]
      , knownCardOwners = Map.fromList [(cid, pid) | (cid, _, pid) <- entries]
      }

learnNotificationCards :: Map Int Text -> CardIndex -> Object -> Either Text CardIndex
learnNotificationCards cardTypes cardIndex notification = do
  cards <- notificationCardValues notification
  entries <- traverse cardEntry cards
  pure CardIndex
    { knownCardNames =
        Map.union
          (Map.fromList [(cid, name) | (cid, name, _) <- entries])
          (knownCardNames cardIndex)
    , knownCardOwners =
        Map.union
          (Map.fromList [(cid, owner) | (cid, _, Just owner) <- entries])
          (knownCardOwners cardIndex)
    }
  where
    cardEntry value = do
      cid <- cardId value
      name <- cardName cardTypes value
      owner <- cardOwner value
      pure (cid, name, owner)

cardOwner :: Value -> Either Text (Maybe PlayerId)
cardOwner value = do
  cardObject <- expectObject "card" value
  case optionalField "location_arg" cardObject of
    Nothing -> pure Nothing
    Just locationValue -> do
      pid <- PlayerId <$> intValue "card location_arg" locationValue
      pure $ if pid == PlayerId 0 then Nothing else Just pid

notificationCardValues :: Object -> Either Text [Value]
notificationCardValues notification =
  case notificationType notification of
    "drawCards" -> cardsFromNotification "drawCards" notification
    "explored_choice" -> cardsFromNotification "explored_choice" notification
    "keepcards" -> cardsFromNotification "keepcards" notification
    "playcard" -> do
      args <- objectField "args" notification
      cardValue <- field "card" args
      pure [cardValue]
    "showTableau" -> do
      args <- objectField "args" notification
      cardsValue <- field "cards" args
      objectValues <$> expectObject "showTableau cards" cardsValue
    _ -> pure []

cardsFromNotification :: Text -> Object -> Either Text [Value]
cardsFromNotification label notification =
  cardsFromValue label =<< field "args" notification

cardsFromValue :: Text -> Value -> Either Text [Value]
cardsFromValue label value =
  case value of
    Array cards -> pure (Vector.toList cards)
    Object cards -> pure (objectValues cards)
    _ -> Left (label <> " args is not a card array or object")

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

discardCardIds :: Object -> Either Text [Int]
discardCardIds args = do
  cardsValue <- field "cards" args
  case cardsValue of
    Array values -> traverse (intValue "discard card id") (Vector.toList values)
    _ -> Left "discard.cards is not an array"

lookupKnownCardName :: CardIndex -> Int -> Either Text Text
lookupKnownCardName cardIndex cid =
  case Map.lookup cid (knownCardNames cardIndex) of
    Just name -> pure name
    Nothing -> Left ("discard references unknown card instance " <> showText cid)

discardOwner :: CardIndex -> [Int] -> Either Text PlayerId
discardOwner cardIndex cardIds = do
  owners <- traverse lookupOwner cardIds
  case owners of
    [] -> Left "discard has no cards"
    owner : rest
      | all (== owner) rest -> pure owner
      | otherwise ->
          Left
            ( "discard has mixed owners: "
                <> Text.intercalate ", " (fmap (showText . unPlayerId) owners)
            )
  where
    lookupOwner cid =
      case Map.lookup cid (knownCardOwners cardIndex) of
        Just owner -> pure owner
        Nothing -> Left ("discard references card with unknown owner " <> showText cid)

showText :: Show a => a -> Text
showText = Text.pack . show
