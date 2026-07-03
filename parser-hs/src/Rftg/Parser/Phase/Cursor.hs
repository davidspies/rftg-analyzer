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

import Rftg.Bga.State
  ( BgaPhase (..)
  , BgaState
  , bgaStateIsNewActionRound
  , bgaStatePhase
  )
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

advancePhaseCursor :: BgaState -> PhaseCursor -> PhaseCursor
advancePhaseCursor bgaState cursor
  | bgaStateIsNewActionRound bgaState =
      cursor
        { cursorRound = Just (nextRound cursor)
        , cursorPhase = ActionSelection
        }
  | otherwise =
      case bgaStatePhase bgaState of
        Nothing -> cursor
        Just phase -> cursor { cursorPhase = phaseForBgaPhase phase }

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

phaseForBgaPhase :: BgaPhase -> Phase
phaseForBgaPhase phase =
  case phase of
    BgaAction -> ActionReveal
    BgaExplore -> Explore
    BgaDevelop -> Develop
    BgaSettle -> Settle
    BgaConsume -> Consume
    BgaProduce -> Produce
    BgaDiscard -> Discard
    BgaGameOver -> GameOver
