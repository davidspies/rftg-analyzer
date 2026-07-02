{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Cursor
  ( Phase (..)
  , PhaseCursor (..)
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  , phaseOrder
  ) where

import Data.Text (Text)

import Rftg.Keldon.Script (ChoiceOrder (..))

data Phase
  = BeforeFirstAction
  | ActionSelection
  | ActionReveal
  | Explore
  | Develop
  | Settle
  | Consume
  | Produce
  | Discard
  | GameOver
  deriving stock (Eq, Show)

data PhaseCursor = PhaseCursor
  { cursorRound :: Maybe Int
  , cursorPhase :: Phase
  }
  deriving stock (Eq, Show)

initialPhaseCursor :: PhaseCursor
initialPhaseCursor = PhaseCursor
  { cursorRound = Nothing
  , cursorPhase = BeforeFirstAction
  }

advancePhaseCursor :: Int -> PhaseCursor -> PhaseCursor
advancePhaseCursor stateId cursor =
  case stateId of
    10 ->
      cursor
        { cursorRound = Just (nextRound cursor)
        , cursorPhase = ActionSelection
        }
    _ -> cursor { cursorPhase = phaseForState stateId (cursorPhase cursor) }

cursorChoiceOrder :: PhaseCursor -> Int -> Either Text ChoiceOrder
cursorChoiceOrder cursor eventIx =
  case cursorRound cursor of
    Nothing -> Left "choice before first action round"
    Just roundIndex ->
      Right (ChoiceOrder [roundIndex, phaseOrder (cursorPhase cursor), eventIx])

nextRound :: PhaseCursor -> Int
nextRound cursor =
  case cursorRound cursor of
    Nothing -> 0
    Just n -> n + 1

phaseOrder :: Phase -> Int
phaseOrder phase =
  case phase of
    BeforeFirstAction -> 0
    ActionSelection -> 0
    ActionReveal -> 0
    Explore -> 1
    Develop -> 2
    Settle -> 3
    Consume -> 4
    Produce -> 5
    Discard -> 6
    GameOver -> 9

phaseForState :: Int -> Phase -> Phase
phaseForState stateId current =
  case stateId of
    11 -> ActionReveal
    12 -> ActionReveal
    20 -> Explore
    21 -> Explore
    30 -> Develop
    31 -> Develop
    230 -> Develop
    231 -> Develop
    311 -> Develop
    40 -> Settle
    41 -> Settle
    42 -> Settle
    43 -> Settle
    241 -> Settle
    242 -> Settle
    341 -> Settle
    342 -> Settle
    442 -> Settle
    542 -> Settle
    50 -> Consume
    51 -> Consume
    52 -> Consume
    60 -> Produce
    61 -> Produce
    62 -> Produce
    69 -> Produce
    70 -> Discard
    71 -> Discard
    98 -> GameOver
    99 -> GameOver
    100 -> GameOver
    _ -> current
