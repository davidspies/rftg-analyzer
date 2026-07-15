{-# LANGUAGE OverloadedStrings #-}

-- | Versioned JSON output consumed directly by rftg2's native replay tool.
-- The BGA interpretation remains entirely in the existing parser; this
-- module only serializes the parser's typed script result.
module Rftg.Native.Render
  ( renderNativeReplay
  ) where

import Control.Monad (foldM)
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Types (Seat (..))
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , ExpectLine (..)
  , HeaderLine (..)
  , KeldonChoice (..)
  , KeldonScript (..)
  , OrderedScriptLine (..)
  , ScriptLine (..)
  )

renderNativeReplay :: KeldonScript -> Either Text Value
renderNativeReplay script = do
  let headers = [header | Header header <- scriptPreamble script]
  tableId <- required "table id" tableIdOf headers
  playerCount <- required "player count" playerCountOf headers
  expansion <- required "expansion" expansionOf headers
  advanced <- required "advanced flag" advancedOf headers
  promo <- required "promo flag" promoOf headers
  goals <- required "goals flag" goalsOf headers
  takeovers <- required "takeovers flag" takeoversOf headers
  seed <- required "seed" seedOf headers
  reviewSeat <- required "review seat" reviewOf headers
  conceded <- required "concede flag" concedeOf headers
  players <- renderPlayers playerCount headers
  if length players /= playerCount
    then Left "native replay requires exactly one name for every player seat"
    else pure $ object
      [ "format" .= ("rftg2-replay" :: Text)
      , "version" .= (1 :: Int)
      , "source" .= object
          [ "kind" .= ("bga" :: Text)
          , "table_id" .= tableId
          , "parser_expansion" .= expansion
          ]
      , "config" .= object
          [ "players" .= playerCount
          , "expansion" .= nativeExpansion expansion
          , "advanced" .= advanced
          , "promo" .= promo
          , "goals" .= goals
          , "takeovers" .= takeovers
          ]
      , "seed" .= seed
      , "review_seat" .= unSeat reviewSeat
      , "conceded" .= conceded
      , "players" .= players
      , "goals" .= [name | Goal name <- headers]
      , "start_options" .= renderStartOptions (scriptPreamble script)
      , "draws" .= renderDraws playerCount (scriptDraws script)
      , "streams" .= renderStreams playerCount (scriptChoices script)
      ]

-- BGA's current option value 1 is "Base game" while the legacy Keldon
-- target preserves that value as @expanded 1@. The native engine uses a
-- zero-based enum, so normalize it only in the new output target. Value 0
-- is retained for older logs whose Base option was absent/defaulted.
nativeExpansion :: Int -> Int
nativeExpansion 1 = 0
nativeExpansion value = value

required :: Text -> (HeaderLine -> Maybe a) -> [HeaderLine] -> Either Text a
required label project headers =
  case mapMaybe project headers of
    [value] -> Right value
    [] -> Left ("native replay is missing " <> label)
    _ -> Left ("native replay has more than one " <> label)

tableIdOf, playerCountOf, expansionOf, seedOf :: HeaderLine -> Maybe Int
tableIdOf (TableId value) = Just value
tableIdOf _ = Nothing
playerCountOf (PlayerCount value) = Just value
playerCountOf _ = Nothing
expansionOf (Expanded value) = Just value
expansionOf _ = Nothing
seedOf (Seed value) = Just value
seedOf _ = Nothing

advancedOf, promoOf, goalsOf, takeoversOf, concedeOf :: HeaderLine -> Maybe Bool
advancedOf (Advanced value) = Just value
advancedOf _ = Nothing
promoOf (Promo value) = Just value
promoOf _ = Nothing
goalsOf (GoalsEnabled value) = Just value
goalsOf _ = Nothing
takeoversOf (TakeoversEnabled value) = Just value
takeoversOf _ = Nothing
concedeOf (Concede value) = Just value
concedeOf _ = Nothing

reviewOf :: HeaderLine -> Maybe Seat
reviewOf (Review seat) = Just seat
reviewOf _ = Nothing

renderPlayers :: Int -> [HeaderLine] -> Either Text [Value]
renderPlayers playerCount headers = do
  names <- uniqueMap "player name" [(unSeat seat, name) | PlayerName seat name <- headers]
  scores <- uniqueMap "final score" [(unSeat seat, score) | FinalScore seat score <- headers]
  pure
    [ object
        [ "seat" .= seat
        , "name" .= name
        , "final_score" .= Map.lookup seat scores
        ]
    | seat <- [0 .. playerCount - 1]
    , name <- maybeToList (Map.lookup seat names)
    ]

-- Header parsing has already validated the source. Duplicate seats here are
-- an internal serialization error, so fail loudly rather than choose one.
uniqueMap :: (Ord key, Show key) => Text -> [(key, value)] -> Either Text (Map key value)
uniqueMap label = foldM insertUnique Map.empty
  where
    insertUnique values (key, value)
      | Map.member key values = Left ("duplicate " <> label <> " for " <> showText key)
      | otherwise = Right (Map.insert key value values)

showText :: Show a => a -> Text
showText = Text.pack . show

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

renderStartOptions :: [ScriptLine] -> [Value]
renderStartOptions lines_ =
  [ object
      [ "seat" .= unSeat seat
      , "cards" .= [first, second]
      ]
  | StartOptions seat first second <- lines_
  ]

renderDraws :: Int -> [ScriptLine] -> [Value]
renderDraws playerCount lines_ =
  [ object
      [ "seat" .= seat
      , "cards" .= Map.findWithDefault [] seat grouped
      ]
  | seat <- [0 .. playerCount - 1]
  ]
  where
    grouped = foldl appendDraw Map.empty [(unSeat seat, card) | Draw seat card <- lines_]
    appendDraw values (seat, card) = Map.alter (Just . maybe [card] (<> [card])) seat values

renderStreams :: Int -> Map Seat [OrderedScriptLine] -> [Value]
renderStreams playerCount streams =
  [ object
      [ "seat" .= seat
      , "events" .= fmap (renderEvent . orderedLine) ordered
      ]
  | seat <- [0 .. playerCount - 1]
  , let ordered = List.sortOn orderedLineOrder (Map.findWithDefault [] (Seat seat) streams)
  ]

renderEvent :: ScriptLine -> Value
renderEvent line =
  case line of
    Choice mode _ choice -> object
      [ "type" .= ("choice" :: Text)
      , "required" .= (mode == Required)
      , "answer" .= renderChoice choice
      ]
    Expect _ expectation -> object
      [ "type" .= ("expect" :: Text)
      , "expectation" .= renderExpectation expectation
      ]
    _ -> error "non-choice line found in native replay player stream"

renderExpectation :: ExpectLine -> Value
renderExpectation expectation =
  case expectation of
    ExpectGoods value -> scalarExpectation "goods" value
    ExpectPrestige value -> scalarExpectation "prestige" value
    ExpectGoodsDist value -> scalarExpectation "goods_dist" value
    ExpectHand value -> scalarExpectation "hand" value
    ExpectTableau value -> scalarExpectation "tableau" value
    ExpectVp value -> scalarExpectation "vp" value
    ExpectHandNames names -> object
      [ "kind" .= ("hand_names" :: Text)
      , "cards" .= names
      ]
  where
    scalarExpectation :: ToJSON a => Text -> a -> Value
    scalarExpectation kind value = object ["kind" .= kind, "value" .= value]

renderChoice :: KeldonChoice -> Value
renderChoice choice =
  case choice of
    ChooseAction actions -> object ["kind" .= ("ACTION" :: Text), "actions" .= actions]
    ChooseStart discards world -> object
      [ "kind" .= ("START" :: Text), "discards" .= discards, "world" .= world]
    ChooseDiscard cards -> cardsChoice "DISCARD" cards
    ChooseSave card -> cardChoice "SAVE" card
    ChooseWindfall card -> cardChoice "WINDFALL" card
    ChooseLucky value -> valueChoice "LUCKY" value
    ChooseDiscardPrestige card -> cardChoice "DISCARD_PRESTIGE" card
    ChooseSearchType value -> valueChoice "SEARCH_TYPE" value
    ChooseSearchKeep value -> valueChoice "SEARCH_KEEP" value
    ChooseTakeover selection -> takeoverChoice "TAKEOVER" selection
    ChooseDefend cards specials -> object
      [ "kind" .= ("DEFEND" :: Text)
      , "cards" .= cards
      , "specials" .= specials
      ]
    ChooseTakeoverPrevent selection -> takeoverChoice "TAKEOVER_PREVENT" selection
    ChooseProduce card power -> powerChoice "PRODUCE" card power
    ChooseDiscardProduce discard world -> object
      [ "kind" .= ("DISCARD_PRODUCE" :: Text)
      , "discard" .= discard
      , "world" .= world
      ]
    ChooseSettle card -> optionalCardChoice "SETTLE" card
    ChooseUpgrade replacement oldWorld -> object
      [ "kind" .= ("UPGRADE" :: Text)
      , "replacement" .= replacement
      , "world" .= oldWorld
      ]
    ChooseConsume card power -> powerChoice "CONSUME" card power
    ChooseConsumeHand cards -> cardsChoice "CONSUME_HAND" cards
    ChooseGood cards -> cardsChoice "GOOD" cards
    ChooseTrade card -> cardChoice "TRADE" card
    ChoosePlace card -> optionalCardChoice "PLACE" card
    ChoosePayment cards specials -> object
      [ "kind" .= ("PAYMENT" :: Text)
      , "cards" .= cards
      , "specials" .= specials
      ]
  where
    cardChoice kind card = object ["kind" .= (kind :: Text), "card" .= card]
    cardsChoice kind cards = object ["kind" .= (kind :: Text), "cards" .= cards]
    optionalCardChoice kind card = object ["kind" .= (kind :: Text), "card" .= card]
    powerChoice kind card power = object
      ["kind" .= (kind :: Text), "card" .= card, "power" .= power]
    takeoverChoice kind selection = object
      [ "kind" .= (kind :: Text)
      , "target" .= fmap fst selection
      , "power" .= fmap snd selection
      ]
    valueChoice kind value = object ["kind" .= (kind :: Text), "value" .= value]
