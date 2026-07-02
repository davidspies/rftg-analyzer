{-# LANGUAGE OverloadedStrings #-}

module Rftg.Bga.Json
  ( Object
  , arrayField
  , boolValue
  , field
  , intValue
  , keyText
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  , valueText
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Text.Read (readMaybe)

type Object = KeyMap.KeyMap Value

field :: Text -> Object -> Either Text Value
field name obj =
  case KeyMap.lookup (Key.fromText name) obj of
    Just value -> Right value
    Nothing -> Left ("missing JSON field `" <> name <> "`")

optionalField :: Text -> Object -> Maybe Value
optionalField name = KeyMap.lookup (Key.fromText name)

objectField :: Text -> Object -> Either Text Object
objectField name obj = field name obj >>= expectObject name

arrayField :: Text -> Object -> Either Text [Value]
arrayField name obj = do
  value <- field name obj
  case value of
    Array xs -> Right (Vector.toList xs)
    _ -> Left ("JSON field `" <> name <> "` is not an array")

expectObject :: Text -> Value -> Either Text Object
expectObject label value =
  case value of
    Object obj -> Right obj
    _ -> Left (label <> " is not a JSON object")

objectValues :: Object -> [Value]
objectValues = fmap snd . KeyMap.toList

keyText :: Key.Key -> Text
keyText = Key.toText

intValue :: Text -> Value -> Either Text Int
intValue label value =
  case value of
    Number n ->
      case toBoundedInteger n of
        Just i -> Right i
        Nothing -> Left (label <> " is not an integral number")
    String t ->
      case readMaybe (Text.unpack t) of
        Just i -> Right i
        Nothing -> Left (label <> " is not an integer string")
    _ -> Left (label <> " is not an integer")

boolValue :: Text -> Value -> Either Text Bool
boolValue label value =
  case value of
    Bool b -> Right b
    _ -> Left (label <> " is not a boolean")

textValue :: Text -> Value -> Either Text Text
textValue label value =
  case value of
    String t -> Right t
    _ -> Left (label <> " is not text")

valueText :: Value -> Text
valueText value =
  case value of
    String t -> t
    Number n -> Text.pack (show n)
    Bool True -> "true"
    Bool False -> "false"
    Null -> "null"
    Array _ -> "<array>"
    Object _ -> "<object>"
