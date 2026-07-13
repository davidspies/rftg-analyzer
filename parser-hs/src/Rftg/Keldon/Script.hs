{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Rftg.Keldon.Script
  ( ChoiceMode (..)
  , ChoiceOrder (..)
  , HeaderLine (..)
  , ExpectLine (..)
  , KeldonChoice (..)
  , KeldonScript (..)
  , OrderedScriptLine (..)
  , ScriptLine (..)
  , appendScript
  , choiceScript
  , choiceScriptAt
  , drawScript
  , emptyScript
  , preambleScript
  , renderScript
  , renderScriptLine
  ) where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Types (Seat (..))
import Rftg.Keldon.Render (quote, quoteCard)

data ScriptLine
  = Header HeaderLine
  | StartOptions Seat Text Text
  | Draw Seat Text
  | Expect Seat ExpectLine
  | Choice ChoiceMode Seat KeldonChoice
  deriving stock (Eq, Show)

data HeaderLine
  = TableId Int
  | PlayerCount Int
  | Expanded Int
  | Advanced Bool
  | Promo Bool
  | GoalsEnabled Bool
  | TakeoversEnabled Bool
  | Seed Int
  | Review Seat
  | Concede Bool
  | FinalScore Seat Int
  | PlayerName Seat Text
  | Goal Text
  deriving stock (Eq, Show)

data ChoiceMode
  = Required
  | Optional
  deriving stock (Eq, Show)

data ExpectLine
  = ExpectGoods Int
  | ExpectPrestige Int
  | ExpectGoodsDist Text
  | ExpectHand Int
  | ExpectTableau Int
  | ExpectVp Int
  | ExpectHandNames [Text]
  deriving stock (Eq, Show)

data KeldonChoice
  = ChooseAction [Int]
  | ChooseStart [Text] Text
  | ChooseDiscard [Text]
  | ChooseSave Text
  | ChooseWindfall Text
  | ChooseLucky Int
  | ChooseDiscardPrestige Text
  | ChooseSearchType Int
  | ChooseSearchKeep Int
  | ChooseTakeover (Maybe (Text, Text))
  | ChooseDefend [Text] [Text]
  | ChooseTakeoverPrevent (Maybe (Text, Text))
  | ChooseProduce Text Int
  | ChooseDiscardProduce Text Text
  | ChooseSettle (Maybe Text)
  | ChooseUpgrade Text Text
  | ChooseConsume Text Int
  | ChooseConsumeHand [Text]
  | ChooseGood [Text]
  | ChooseTrade Text
  | ChoosePlace (Maybe Text)
  | ChoosePayment [Text] [Text]
  deriving stock (Eq, Show)

newtype ChoiceOrder = ChoiceOrder { unChoiceOrder :: [Int] }
  deriving stock (Eq, Ord, Show)

data OrderedScriptLine = OrderedScriptLine
  { orderedLineOrder :: ChoiceOrder
  , orderedLine :: ScriptLine
  }
  deriving stock (Eq, Show)

data KeldonScript = KeldonScript
  { scriptPreamble :: [ScriptLine]
  , scriptDraws :: [ScriptLine]
  , scriptChoices :: Map Seat [OrderedScriptLine]
  }
  deriving stock (Eq, Show)

emptyScript :: KeldonScript
emptyScript = KeldonScript
  { scriptPreamble = []
  , scriptDraws = []
  , scriptChoices = Map.empty
  }

preambleScript :: [ScriptLine] -> KeldonScript
preambleScript lines_ = emptyScript { scriptPreamble = lines_ }

drawScript :: [ScriptLine] -> KeldonScript
drawScript lines_ = emptyScript { scriptDraws = lines_ }

choiceScript :: Seat -> [ScriptLine] -> KeldonScript
choiceScript =
  choiceScriptAt (ChoiceOrder [])

choiceScriptAt :: ChoiceOrder -> Seat -> [ScriptLine] -> KeldonScript
choiceScriptAt order seat lines_ =
  emptyScript { scriptChoices = Map.singleton seat (fmap (OrderedScriptLine order) lines_) }

appendScript :: KeldonScript -> KeldonScript -> KeldonScript
appendScript left right = KeldonScript
  { scriptPreamble = scriptPreamble left <> scriptPreamble right
  , scriptDraws = scriptDraws left <> scriptDraws right
  , scriptChoices = Map.unionWith (<>) (scriptChoices left) (scriptChoices right)
  }

renderScript :: KeldonScript -> Text
renderScript script =
  Text.unlines $
    fmap renderScriptLine (scriptPreamble script)
      <> fmap renderScriptLine (scriptDraws script)
      <> concatMap (fmap (renderScriptLine . orderedLine) . orderedChoices . snd)
        (Map.toAscList (scriptChoices script))

orderedChoices :: [OrderedScriptLine] -> [OrderedScriptLine]
orderedChoices =
  List.sortOn orderedLineOrder

renderScriptLine :: ScriptLine -> Text
renderScriptLine line =
  case line of
    Header header -> renderHeader header
    StartOptions seat firstOption secondOption ->
      "startoptions "
        <> renderSeat seat
        <> " "
        <> quoteCard firstOption
        <> " "
        <> quoteCard secondOption
    Draw seat cardName ->
      "draw " <> renderSeat seat <> " " <> quoteCard cardName
    Expect seat expectation ->
      "expect " <> renderSeat seat <> " " <> renderExpectation expectation
    Choice mode seat choice ->
      choicePrefix mode
        <> " "
        <> renderSeat seat
        <> " "
        <> renderChoice choice

renderHeader :: HeaderLine -> Text
renderHeader header =
  case header of
    TableId tableId -> "# bga table " <> showText tableId
    PlayerCount n -> "players " <> showText n
    Expanded n -> "expanded " <> showText n
    Advanced enabled -> "advanced " <> boolFlag enabled
    Promo enabled -> "promo " <> boolFlag enabled
    GoalsEnabled enabled -> "goals " <> boolFlag enabled
    TakeoversEnabled enabled -> "takeovers " <> boolFlag enabled
    Seed n -> "seed " <> showText n
    Review seat -> "review " <> renderSeat seat
    Concede conceded -> "concede " <> boolFlag conceded
    FinalScore seat score ->
      "finalscore " <> renderSeat seat <> " " <> showText score
    PlayerName seat name ->
      "name " <> renderSeat seat <> " " <> quote name
    Goal name -> "goal " <> quote name

renderExpectation :: ExpectLine -> Text
renderExpectation expectation =
  case expectation of
    ExpectGoods goods ->
      "goods " <> showText goods
    ExpectPrestige prestige ->
      "prestige " <> showText prestige
    ExpectGoodsDist goodsDist ->
      "goodsdist " <> quote goodsDist
    ExpectHand hand ->
      "hand " <> showText hand
    ExpectTableau tableau ->
      "tableau " <> showText tableau
    ExpectVp vp ->
      "vp " <> showText vp
    ExpectHandNames names ->
      "handnames " <> Text.unwords (fmap quoteCard names)

renderChoice :: KeldonChoice -> Text
renderChoice choice =
  case choice of
    ChooseAction actions ->
      "ACTION " <> Text.unwords (fmap showText actions)
    ChooseStart discards startWorld ->
      "START "
        <> Text.unwords (fmap quoteCard discards)
        <> " : "
        <> quoteCard startWorld
    ChooseDiscard cards ->
      "DISCARD " <> Text.unwords (fmap quoteCard cards)
    ChooseSave card ->
      "SAVE " <> quoteCard card
    ChooseWindfall world ->
      "WINDFALL " <> quoteCard world
    ChooseLucky number ->
      "LUCKY " <> showText number
    ChooseDiscardPrestige card ->
      "DISCARD_PRESTIGE " <> quoteCard card
    ChooseSearchType category ->
      "SEARCH_TYPE " <> showText category
    ChooseSearchKeep keep ->
      "SEARCH_KEEP " <> showText keep
    ChooseTakeover Nothing ->
      "TAKEOVER none"
    ChooseTakeover (Just (target, power)) ->
      "TAKEOVER " <> quoteCard target <> " : " <> quoteCard power
    ChooseDefend cards specials ->
      "DEFEND " <> renderCardLists cards specials
    ChooseTakeoverPrevent Nothing ->
      "TAKEOVER_PREVENT none"
    ChooseTakeoverPrevent (Just (target, power)) ->
      "TAKEOVER_PREVENT " <> quoteCard target <> " : " <> quoteCard power
    ChooseProduce card powerIndex ->
      "PRODUCE " <> quoteCard card <> " " <> showText powerIndex
    ChooseDiscardProduce discard world ->
      "DISCARD_PRODUCE " <> quoteCard discard <> " : " <> quoteCard world
    ChooseSettle Nothing ->
      "SETTLE none"
    ChooseSettle (Just card) ->
      "SETTLE " <> quoteCard card
    ChooseUpgrade newWorld oldWorld ->
      "UPGRADE " <> quoteCard newWorld <> " : " <> quoteCard oldWorld
    ChooseConsume card powerIndex ->
      "CONSUME " <> quoteCard card <> " " <> showText powerIndex
    ChooseConsumeHand cards ->
      "CONSUME_HAND " <> Text.unwords (fmap quoteCard cards)
    ChooseGood worlds ->
      "GOOD " <> Text.unwords (fmap quoteCard worlds)
    ChooseTrade world ->
      "TRADE " <> quoteCard world
    ChoosePlace Nothing ->
      "PLACE none"
    ChoosePlace (Just card) ->
      "PLACE " <> quoteCard card
    ChoosePayment cards specials ->
      "PAYMENT " <> renderCardLists cards specials

renderCardLists :: [Text] -> [Text] -> Text
renderCardLists cards specials =
  (if null cards then "none" else Text.unwords (fmap quoteCard cards))
    <> (if null specials then "" else " : " <> Text.unwords (fmap quoteCard specials))

choicePrefix :: ChoiceMode -> Text
choicePrefix Required = "choice"
choicePrefix Optional = "choice?"

renderSeat :: Seat -> Text
renderSeat = showText . unSeat

boolFlag :: Bool -> Text
boolFlag True = "1"
boolFlag False = "0"

showText :: Show a => a -> Text
showText = Text.pack . show
