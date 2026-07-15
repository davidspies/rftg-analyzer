{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import Rftg.Bga.Types (PlayerId (..))
import Rftg.Keldon.Script (renderScript)
import Rftg.Native.Render (renderNativeReplay)
import Rftg.Parser.Common (ReviewSelection (..))
import Rftg.Parser.Game (parseGameScript)

main :: IO ()
main = do
  args <- getArgs
  (path, reviewSelection, outputFormat) <- parseArgs args
  decoded <- eitherDecodeFileStrict' path
  value <- either (die . ("failed to decode BGA JSON: " <>)) pure decoded
  script <- either (die . ("failed to parse BGA game: " <>) . show) pure $
    parseGameScript reviewSelection value
  case outputFormat of
    KeldonOutput -> TextIO.putStr (renderScript script)
    Rftg2Output -> do
      replay <- either (die . ("failed to render rftg2 replay: " <>) . Text.unpack) pure $
        renderNativeReplay script
      LazyByteString.putStrLn (Aeson.encode replay)

data OutputFormat = KeldonOutput | Rftg2Output

parseArgs :: [String] -> IO (FilePath, ReviewSelection, OutputFormat)
parseArgs args =
  case go Nothing ReviewDefault Nothing args of
    Right (Just path, reviewSelection, outputFormat) ->
      pure (path, reviewSelection, maybe KeldonOutput id outputFormat)
    Right (Nothing, _, _) -> usage
    Left err -> die err
  where
    usage = die "usage: rftg-bga-parser <bga-game.json> [--format keldon|rftg2] [--user USER | --player-id PLAYER_ID]"

    go path reviewSelection outputFormat [] = Right (path, reviewSelection, outputFormat)
    go path ReviewDefault outputFormat ("--user" : user : rest)
      | null user = Left "--user requires a nonempty username"
      | otherwise = go path (ReviewByName (Text.pack user)) outputFormat rest
    go _ _ _ ("--user" : _ : _) =
      Left "choose only one review selector: --user or --player-id"
    go _ _ _ ["--user"] = Left "--user requires a username"
    go path ReviewDefault outputFormat ("--player-id" : raw : rest) =
      case readMaybe raw of
        Just playerId -> go path (ReviewByPlayerId (PlayerId playerId)) outputFormat rest
        Nothing -> Left ("invalid --player-id: " <> raw)
    go _ _ _ ("--player-id" : _ : _) =
      Left "choose only one review selector: --user or --player-id"
    go _ _ _ ["--player-id"] = Left "--player-id requires a player id"
    go path reviewSelection Nothing ("--format" : raw : rest) =
      case raw of
        "keldon" -> go path reviewSelection (Just KeldonOutput) rest
        "rftg2" -> go path reviewSelection (Just Rftg2Output) rest
        _ -> Left ("unknown output format: " <> raw)
    go _ _ (Just _) ("--format" : _ : _) = Left "--format may be specified only once"
    go _ _ _ ["--format"] = Left "--format requires keldon or rftg2"
    go Nothing reviewSelection outputFormat (arg : rest) =
      go (Just arg) reviewSelection outputFormat rest
    go (Just _) _ _ (arg : _) = Left ("unexpected argument: " <> arg)
