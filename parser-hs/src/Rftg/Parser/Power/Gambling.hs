{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Power.Gambling
  ( parseGamblingChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , field
  , intValue
  , objectField
  , optionalField
  , expectObject
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
  ( notificationObjects
  , notificationType
  , parsePlayers
  )
import Rftg.Parser.Phase.Cursor
  ( PhaseCursor
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data GamblingState = GamblingState
  { phaseCursor :: PhaseCursor
  , gamblingScript :: KeldonScript
  }
  deriving stock (Eq, Show)

parseGamblingChoices :: Value -> Either Text KeldonScript
parseGamblingChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  notifications <- notificationObjects root
  finalState <-
    foldM
      (gamblingStep players)
      emptyGamblingState
      (zip [0 :: Int ..] notifications)
  pure (gamblingScript finalState)

emptyGamblingState :: GamblingState
emptyGamblingState = GamblingState
  { phaseCursor = initialPhaseCursor
  , gamblingScript = emptyScript
  }

gamblingStep :: [Player] -> GamblingState -> (Int, Object) -> Either Text GamblingState
gamblingStep players state (eventIx, notification) =
  case notificationType notification of
    "gameStateChange" -> handleGameState state notification
    "gambling" -> handleGambling players eventIx state notification
    _ -> pure state

handleGameState :: GamblingState -> Object -> Either Text GamblingState
handleGameState state notification = do
  args <- objectField "args" notification
  case optionalField "id" args of
    Nothing -> pure state
    Just idValue -> do
      stateId <- intValue "gameStateChange id" idValue
      pure state { phaseCursor = advancePhaseCursor stateId (phaseCursor state) }

handleGambling :: [Player] -> Int -> GamblingState -> Object -> Either Text GamblingState
handleGambling players eventIx state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "gambling player_id" =<< field "player_id" args)
  player <- lookupPlayer players pid
  number <- intValue "gambling number" =<< field "number" args
  order <- cursorChoiceOrder (phaseCursor state) eventIx
  let line = Choice Optional (playerSeat player) (ChooseLucky number)
      script = choiceScriptAt order (playerSeat player) [line]
  pure state { gamblingScript = gamblingScript state `appendScript` script }

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

showText :: Show a => a -> Text
showText = Text.pack . show
