{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Rftg.Bga.Types
  ( CardName (..)
  , GoalType (..)
  , Player (..)
  , PlayerId (..)
  , Seat (..)
  , StateId (..)
  ) where

import Data.Text (Text)

newtype PlayerId = PlayerId { unPlayerId :: Int }
  deriving newtype (Eq, Ord, Show)

newtype Seat = Seat { unSeat :: Int }
  deriving newtype (Eq, Ord, Show)

newtype CardName = CardName { unCardName :: Text }
  deriving newtype (Eq, Ord, Show)

newtype StateId = StateId { unStateId :: Int }
  deriving newtype (Eq, Ord, Show)

data Player = Player
  { playerSeat :: Seat
  , playerId :: PlayerId
  , playerName :: Text
  }
  deriving stock (Eq, Show)

data GoalType = GoalType
  { goalKind :: Text
  , goalName :: Text
  }
  deriving stock (Eq, Show)
