{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.Text qualified as Text

import Rftg.Bga.Types (Seat (..))
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , ExpectLine (..)
  , HeaderLine (..)
  , KeldonChoice (..)
  , KeldonScript
  , ScriptLine (..)
  , appendScript
  , choiceScript
  , drawScript
  , preambleScript
  , renderScript
  )
import Rftg.Native.Render (renderNativeReplay)

main :: IO ()
main = do
  unless (renderNativeReplay fixture == Right expectedNative) $
    error "native renderer did not preserve the typed script"
  unless (renderScript fixture == expectedLegacy) $
    error "adding the native target changed legacy Keldon rendering"

fixture :: KeldonScript
fixture =
  preambleScript
    [ Header (TableId 123456)
    , Header (PlayerCount 2)
    , Header (Expanded 1)
    , Header (Advanced True)
    , Header (Promo True)
    , Header (GoalsEnabled False)
    , Header (TakeoversEnabled False)
    , Header (Seed 123456)
    , Header (Review (Seat 0))
    , Header (Concede False)
    , Header (FinalScore (Seat 0) 31)
    , Header (FinalScore (Seat 1) 27)
    , Header (PlayerName (Seat 0) "Ada")
    , Header (PlayerName (Seat 1) "Grace")
    , StartOptions (Seat 0) "Epsilon Eridani" "Alpha Centauri"
    , StartOptions (Seat 1) "New Sparta" "Gateway Station"
    ]
    `appendScript` drawScript
      [ Draw (Seat 0) "Earth's Lost Colony"
      , Draw (Seat 1) "Old Earth"
      ]
    `appendScript` choiceScript (Seat 0)
      [ Choice Required (Seat 0) (ChooseAction [3, 5])
      , Expect (Seat 0) (ExpectHand 4)
      ]
    `appendScript` choiceScript (Seat 1)
      [ Choice Optional (Seat 1) (ChoosePlace Nothing)
      ]

expectedNative :: Value
expectedNative = object
  [ "format" .= ("rftg2-replay" :: Text.Text)
  , "version" .= (1 :: Int)
  , "source" .= object
      [ "kind" .= ("bga" :: Text.Text)
      , "table_id" .= (123456 :: Int)
      , "parser_expansion" .= (1 :: Int)
      ]
  , "config" .= object
      [ "players" .= (2 :: Int)
      , "expansion" .= (0 :: Int)
      , "advanced" .= True
      , "promo" .= True
      , "goals" .= False
      , "takeovers" .= False
      ]
  , "seed" .= (123456 :: Int)
  , "review_seat" .= (0 :: Int)
  , "conceded" .= False
  , "players" .=
      [ object ["seat" .= (0 :: Int), "name" .= ("Ada" :: Text.Text), "final_score" .= (Just (31 :: Int))]
      , object ["seat" .= (1 :: Int), "name" .= ("Grace" :: Text.Text), "final_score" .= (Just (27 :: Int))]
      ]
  , "goals" .= ([] :: [Text.Text])
  , "start_options" .=
      [ object ["seat" .= (0 :: Int), "cards" .= (["Epsilon Eridani", "Alpha Centauri"] :: [Text.Text])]
      , object ["seat" .= (1 :: Int), "cards" .= (["New Sparta", "Gateway Station"] :: [Text.Text])]
      ]
  , "draws" .=
      [ object ["seat" .= (0 :: Int), "cards" .= (["Earth's Lost Colony"] :: [Text.Text])]
      , object ["seat" .= (1 :: Int), "cards" .= (["Old Earth"] :: [Text.Text])]
      ]
  , "streams" .=
      [ object
          [ "seat" .= (0 :: Int)
          , "events" .=
              [ object
                  [ "type" .= ("choice" :: Text.Text)
                  , "required" .= True
                  , "answer" .= object
                      [ "kind" .= ("ACTION" :: Text.Text)
                      , "actions" .= ([3, 5] :: [Int])
                      ]
                  ]
              , object
                  [ "type" .= ("expect" :: Text.Text)
                  , "expectation" .= object
                      [ "kind" .= ("hand" :: Text.Text)
                      , "value" .= (4 :: Int)
                      ]
                  ]
              ]
          ]
      , object
          [ "seat" .= (1 :: Int)
          , "events" .=
              [ object
                  [ "type" .= ("choice" :: Text.Text)
                  , "required" .= False
                  , "answer" .= object
                      [ "kind" .= ("PLACE" :: Text.Text)
                      , "card" .= (Nothing :: Maybe Text.Text)
                      ]
                  ]
              ]
          ]
      ]
  ]

expectedLegacy :: Text.Text
expectedLegacy = Text.unlines
  [ "# bga table 123456"
  , "players 2"
  , "expanded 1"
  , "advanced 1"
  , "promo 1"
  , "goals 0"
  , "takeovers 0"
  , "seed 123456"
  , "review 0"
  , "concede 0"
  , "finalscore 0 31"
  , "finalscore 1 27"
  , "name 0 \"Ada\""
  , "name 1 \"Grace\""
  , "startoptions 0 \"Epsilon Eridani\" \"Alpha Centauri\""
  , "startoptions 1 \"New Sparta\" \"Gateway Station\""
  , "draw 0 \"Earth's Lost Colony\""
  , "draw 1 \"Old Earth\""
  , "choice 0 ACTION 3 5"
  , "expect 0 hand 4"
  , "choice? 1 PLACE none"
  ]
