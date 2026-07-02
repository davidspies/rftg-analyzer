{-# LANGUAGE OverloadedStrings #-}

module Rftg.Keldon.Render
  ( keldonCardName
  , quote
  , quoteCard
  , renderLines
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

quote :: Text -> Text
quote value = "\"" <> Text.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = Text.singleton c

quoteCard :: Text -> Text
quoteCard = quote . keldonCardName

keldonCardName :: Text -> Text
keldonCardName "Malevolent Life Forms" = "Malevolent Lifeforms"
keldonCardName "Blaster Gem Mine" = "Blaster Gem Mines"
keldonCardName name = name

renderLines :: [Text] -> Text
renderLines lines_ = Text.unlines lines_
