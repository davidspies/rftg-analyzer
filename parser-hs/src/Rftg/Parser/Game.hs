{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Game
  ( parseGameScript
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Rftg.Keldon.Script (KeldonScript, appendScript, emptyScript)
import Rftg.Parser.Action (parseActions)
import Rftg.Parser.Common (ReviewSelection)
import Rftg.Parser.Draw (parseDraws)
import Rftg.Parser.Expect (parseExpectations)
import Rftg.Parser.Expect.HandNames (applyHandNameExpectations)
import Rftg.Parser.Header (parseHeader)
import Rftg.Parser.Phase.Consume (parseConsumeChoices)
import Rftg.Parser.Phase.Discard (parseDiscardChoices)
import Rftg.Parser.Phase.Explore (parseExploreChoices)
import Rftg.Parser.Phase.PendingDiscard (parseOptionalDiscardChoices)
import Rftg.Parser.Phase.Payment (parsePaymentChoices)
import Rftg.Parser.Phase.Place (parsePlaceChoices)
import Rftg.Parser.Phase.Produce (parseProduceChoices)
import Rftg.Parser.Phase.Settle (parseSettleChoices)
import Rftg.Parser.Phase.Takeover (parseTakeoverChoices)
import Rftg.Parser.Power.DiscardPrestige (parseDiscardPrestigeChoices)
import Rftg.Parser.Power.Gambling (parseGamblingChoices)
import Rftg.Parser.Power.Search (parseSearchChoices)
import Rftg.Parser.Power.Scavenger (parseScavengerChoices)
import Rftg.Parser.Setup (parseSetup)

parseGameScript :: ReviewSelection -> Value -> Either Text KeldonScript
parseGameScript reviewSelection value = do
  header <- parseHeader reviewSelection value
  setup <- parseSetup value
  draws <- parseDraws value
  expectations <- parseExpectations reviewSelection value
  actions <- parseActions value
  explore <- parseExploreChoices value
  discard <- parseDiscardChoices value
  scavenger <- parseScavengerChoices value
  place <- parsePlaceChoices value
  produce <- parseProduceChoices value
  settle <- parseSettleChoices value
  takeover <- parseTakeoverChoices value
  consume <- parseConsumeChoices value
  gambling <- parseGamblingChoices value
  discardPrestige <- parseDiscardPrestigeChoices value
  search <- parseSearchChoices value
  optionalDiscard <- parseOptionalDiscardChoices value
  payment <- parsePaymentChoices value
  let script =
        foldl
          appendScript
          emptyScript
          [ header
          , setup
          , draws
          , expectations
          , actions
          , explore
          , discard
          , scavenger
          , optionalDiscard
          , settle
          , takeover
          , place
          , payment
          , produce
          , consume
          , gambling
          , discardPrestige
          , search
          ]
  applyHandNameExpectations reviewSelection value script
