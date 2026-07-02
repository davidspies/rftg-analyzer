{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import Rftg.Bga.Types (PlayerId (..))
import Rftg.Keldon.Script (renderScript)
import Rftg.Parser.Common (ReviewSelection (..))
import Rftg.Parser.Game (parseGameScript)

main :: IO ()
main = do
  args <- getArgs
  (path, reviewSelection) <- parseArgs args
  decoded <- eitherDecodeFileStrict' path
  value <- either (die . ("failed to decode BGA JSON: " <>)) pure decoded
  script <- either (die . ("failed to parse BGA game: " <>) . show) pure $
    parseGameScript reviewSelection value
  TextIO.putStr (renderScript script)

parseArgs :: [String] -> IO (FilePath, ReviewSelection)
parseArgs args =
  case go Nothing ReviewDefault args of
    Right (Just path, reviewSelection) -> pure (path, reviewSelection)
    Right (Nothing, _) -> usage
    Left err -> die err
  where
    usage = die "usage: rftg-bga-parser <bga-game.json> [--user USER | --player-id PLAYER_ID]"

    go path reviewSelection [] = Right (path, reviewSelection)
    go path ReviewDefault ("--user" : user : rest)
      | null user = Left "--user requires a nonempty username"
      | otherwise = go path (ReviewByName (Text.pack user)) rest
    go _ _ ("--user" : _ : _) =
      Left "choose only one review selector: --user or --player-id"
    go _ _ ["--user"] = Left "--user requires a username"
    go path ReviewDefault ("--player-id" : raw : rest) =
      case readMaybe raw of
        Just playerId -> go path (ReviewByPlayerId (PlayerId playerId)) rest
        Nothing -> Left ("invalid --player-id: " <> raw)
    go _ _ ("--player-id" : _ : _) =
      Left "choose only one review selector: --user or --player-id"
    go _ _ ["--player-id"] = Left "--player-id requires a player id"
    go Nothing reviewSelection (arg : rest) = go (Just arg) reviewSelection rest
    go (Just _) _ (arg : _) = Left ("unexpected argument: " <> arg)
