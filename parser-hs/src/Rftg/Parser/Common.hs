{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Common
  ( cardId
  , cardName
  , CardTypeInfo (..)
  , PayMilitaryCondition (..)
  , PayMilitaryPower (..)
  , ReviewSelection (..)
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parseCardTypes
  , parsePlayers
  , reviewGamedataObject
  , selectReviewPlayer
  , tableInfoObject
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector

import Rftg.Bga.Json
  ( Object
  , arrayField
  , field
  , intValue
  , keyText
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  , Seat (..)
  )

data CardTypeInfo = CardTypeInfo
  { cardTypeName :: Text
  , cardTypeType :: Text
  , cardTypeCost :: Maybe Int
  , cardTypeGoodKind :: Maybe Int
  , cardTypeCategories :: Set Text
  , cardTypePhase3MilitaryForces :: [Int]
  , cardTypeHasSixDevScoring :: Bool
  , cardTypeHasTakeoverSearchPower :: Bool
  , cardTypeHasTakeoverPrevention :: Bool
  , cardTypeHasWindfallProduceIfDiscard :: Bool
  , cardTypeHasDiscardPrestige :: Bool
  , cardTypeHasSettleReplace :: Bool
  , cardTypeHasConsumeForSell :: Bool
  , cardTypeHasGoodForSettleCost :: Bool
  , cardTypeHasPhase4Draw :: Bool
  , cardTypeIsMilitary :: Bool
  , cardTypeIsAlien :: Bool
  , cardTypeHasNoAlienDiplomat :: Bool
  , cardTypeHasAlienDiplomat :: Bool
  , cardTypeHasRebelDiplomat :: Bool
  , cardTypeHasChromosomeDiplomat :: Bool
  , cardTypeHasAnyDiplomat :: Bool
  , cardTypePayMilitaryPowers :: [PayMilitaryPower]
  , cardTypeHasTemporaryMilitaryDiscard :: Bool
  , cardTypeHasPrestigeMilitary :: Bool
  , cardTypeHasDiplomatBonus :: Bool
  , cardTypeIsSettlePaymentDiscardSource :: Bool
  }
  deriving stock (Eq, Show)

data ReviewSelection
  = ReviewDefault
  | ReviewByName Text
  | ReviewByPlayerId PlayerId
  deriving stock (Eq, Show)

data PayMilitaryCondition
  = PayMilitaryNonAlien
  | PayMilitaryAlien
  | PayMilitaryRebel
  | PayMilitaryChromosome
  deriving stock (Eq, Show)

data PayMilitaryPower = PayMilitaryPower
  { payMilitaryCondition :: PayMilitaryCondition
  , payMilitaryReduction :: Int
  }
  deriving stock (Eq, Show)

tableInfoObject :: Object -> Either Text Object
tableInfoObject root = do
  outer <- objectField "tableinfos" root
  case optionalField "data" outer of
    Just value -> expectObject "tableinfos.data" value
    Nothing -> pure outer

parsePlayers :: Object -> Either Text [Player]
parsePlayers root = do
  logs <- objectField "logs" root
  logsData <- objectField "data" logs
  values <- arrayField "players" logsData
  traverse (uncurry parsePlayer) (zip [0 ..] values)

parsePlayer :: Int -> Value -> Either Text Player
parsePlayer index value = do
  obj <- expectObject "player" value
  pid <- PlayerId <$> (intValue "player id" =<< field "id" obj)
  name <- textValue "player name" =<< field "name" obj
  pure Player
    { playerSeat = Seat index
    , playerId = pid
    , playerName = name
    }

selectReviewPlayer :: ReviewSelection -> [Player] -> Either Text Player
selectReviewPlayer _ [] = Left "game has no players"
selectReviewPlayer ReviewDefault (player : _) = pure player
selectReviewPlayer (ReviewByName name) players =
  case filter ((== name) . playerName) players of
    [player] -> pure player
    [] ->
      Left $
        "review username " <> name <> " is not in table; players: "
          <> Text.intercalate ", " (fmap playerName players)
    matches ->
      Left $
        "review username " <> name <> " is ambiguous; player ids: "
          <> Text.intercalate ", " (fmap (showText . unPlayerId . playerId) matches)
selectReviewPlayer (ReviewByPlayerId wanted) players =
  case filter ((== wanted) . playerId) players of
    [player] -> pure player
    [] ->
      Left $
        "review player id " <> showText (unPlayerId wanted)
          <> " is not in table; players: "
          <> Text.intercalate ", " (fmap playerLabel players)
    matches ->
      Left $
        "review player id " <> showText (unPlayerId wanted)
          <> " is ambiguous; players: "
          <> Text.intercalate ", " (fmap playerLabel matches)
  where
    playerLabel player =
      playerName player <> " (" <> showText (unPlayerId (playerId player)) <> ")"

reviewGamedataObject :: Object -> Player -> Either Text Object
reviewGamedataObject gamedatas player = do
  let key = showText (unPlayerId (playerId player))
  case KeyMap.lookup (Key.fromText key) gamedatas of
    Just value -> expectObject ("gamedatas." <> key) value
    Nothing -> Left ("missing gamedatas for review player " <> key)

parseCardTypes :: Object -> Either Text (Map Int Text)
parseCardTypes gamedatas =
  fmap cardTypeName <$> parseCardTypeInfos gamedatas

parseCardTypeInfos :: Object -> Either Text (Map Int CardTypeInfo)
parseCardTypeInfos gamedatas =
  case objectValues gamedatas of
    [] -> Left "missing gamedatas"
    firstGamedataValue : _ -> do
      firstGamedata <- expectObject "gamedata" firstGamedataValue
      cardTypesObject <- objectField "card_types" firstGamedata
      fmap Map.fromList $
        traverse parseCardType (KeyMap.toList cardTypesObject)
  where
    parseCardType (key, value) = do
      typeId <- intValue "card type id" (String (keyText key))
      obj <- expectObject "card type" value
      name <- canonicalCardName <$> (textValue "card type name" =<< field "name" obj)
      cardKind <- textValue "card type type" =<< field "type" obj
      cost <- optionalIntField "cost" obj
      goodKind <- optionalIntField "kind" obj
      categories <- cardTypeCategoriesFrom obj
      militaryForces <- cardTypePhase3MilitaryForcesFrom obj
      let hasSixDevScoring =
            case optionalField "sixdev_scoring" obj of
              Just Null -> False
              Nothing -> False
              Just _ -> True
      hasTakeoverSearchPower <- cardTypeHasAnyPower takeoverSearchPowers obj
      hasTakeoverPrevention <- cardTypeHasPower "blocktakeover" obj
      hasDiscardProduce <- cardTypeHasAnyPower discardProducePowers obj
      hasDiscardPrestige <- cardTypeHasConsumeCardPrestige obj
      hasSettleReplace <- cardTypeHasPower "settlereplace" obj
      hasConsumeForSell <- cardTypeHasPowerInPhase "4" "consumeforsell" obj
      hasGoodForSettleCost <- cardTypeHasPower "good_for_settlecost" obj
      hasPhase4Draw <- cardTypeHasPowerInPhase "4" "draw" obj
      diplomat <- cardTypeDiplomatInfo obj
      hasTemporaryMilitaryDiscard <- cardTypeHasAnyPowerInPhase "3" temporaryMilitaryDiscardPowers obj
      hasPrestigeMilitary <- cardTypeHasAnyPowerInPhase "3" (Set.singleton "militaryforcetmp_prestige") obj
      hasDiplomatBonus <- cardTypeHasPower "diplomatbonus" obj
      hasColonyShip <- cardTypeHasAnyPowerInPhase "3" (Set.singleton "colonyship") obj
      pure
        ( typeId
        , CardTypeInfo
            { cardTypeName = name
            , cardTypeType = cardKind
            , cardTypeCost = cost
            , cardTypeGoodKind = goodKind
            , cardTypeCategories = categories
            , cardTypePhase3MilitaryForces = militaryForces
            , cardTypeHasSixDevScoring = hasSixDevScoring
            , cardTypeHasTakeoverSearchPower = hasTakeoverSearchPower
            , cardTypeHasTakeoverPrevention = hasTakeoverPrevention
            , cardTypeHasWindfallProduceIfDiscard = hasDiscardProduce
            , cardTypeHasDiscardPrestige = hasDiscardPrestige
            , cardTypeHasSettleReplace = hasSettleReplace
            , cardTypeHasConsumeForSell = hasConsumeForSell
            , cardTypeHasGoodForSettleCost = hasGoodForSettleCost
            , cardTypeHasPhase4Draw = hasPhase4Draw
            , cardTypeIsMilitary = "military" `Set.member` categories
            , cardTypeIsAlien = "alien" `Set.member` categories
            , cardTypeHasNoAlienDiplomat = diplomatHasNoAlien diplomat
            , cardTypeHasAlienDiplomat = diplomatHasAlien diplomat
            , cardTypeHasRebelDiplomat = diplomatHasRebel diplomat
            , cardTypeHasChromosomeDiplomat = diplomatHasChromosome diplomat
            , cardTypeHasAnyDiplomat = diplomatHasAny diplomat
            , cardTypePayMilitaryPowers = diplomatPayMilitaryPowers diplomat
            , cardTypeHasTemporaryMilitaryDiscard = hasTemporaryMilitaryDiscard
            , cardTypeHasPrestigeMilitary = hasPrestigeMilitary
            , cardTypeHasDiplomatBonus = hasDiplomatBonus
            , cardTypeIsSettlePaymentDiscardSource = hasTemporaryMilitaryDiscard || hasPrestigeMilitary || hasColonyShip
            }
        )

optionalIntField :: Text -> Object -> Either Text (Maybe Int)
optionalIntField name obj =
  case optionalField name obj of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just value -> Just <$> intValue ("card type " <> name) value

cardTypeCategoriesFrom :: Object -> Either Text (Set Text)
cardTypeCategoriesFrom obj =
  case optionalField "category" obj of
    Nothing -> pure Set.empty
    Just Null -> pure Set.empty
    Just (Array values) ->
      Set.fromList <$> traverse (textValue "card type category") (Vector.toList values)
    Just _ -> Left "card type category is not an array"

cardTypeHasPower :: Text -> Object -> Either Text Bool
cardTypeHasPower wanted obj =
  cardTypeHasAnyPower (Set.singleton wanted) obj

cardTypeHasPowerInPhase :: Text -> Text -> Object -> Either Text Bool
cardTypeHasPowerInPhase phase wanted obj =
  cardTypeHasAnyPowerInPhase phase (Set.singleton wanted) obj

cardTypeHasAnyPowerInPhase :: Text -> Set Text -> Object -> Either Text Bool
cardTypeHasAnyPowerInPhase phase wanted obj =
  case optionalField "powers" obj of
    Nothing -> pure False
    Just Null -> pure False
    Just (Array powers)
      | Vector.null powers -> pure False
      | otherwise -> Left "card type powers is a non-empty array"
    Just (Object powersByPhase) ->
      case KeyMap.lookup (Key.fromText phase) powersByPhase of
        Nothing -> pure False
        Just phasePowers -> anyPowerMatches wanted phasePowers
    Just _ -> Left "card type powers is not an object"

data DiplomatInfo = DiplomatInfo
  { diplomatHasNoAlien :: Bool
  , diplomatHasAlien :: Bool
  , diplomatHasRebel :: Bool
  , diplomatHasChromosome :: Bool
  , diplomatHasAny :: Bool
  , diplomatPayMilitaryPowers :: [PayMilitaryPower]
  }
  deriving stock (Eq, Show)

cardTypeDiplomatInfo :: Object -> Either Text DiplomatInfo
cardTypeDiplomatInfo obj =
  case optionalField "powers" obj of
    Nothing -> pure emptyDiplomatInfo
    Just Null -> pure emptyDiplomatInfo
    Just (Array powers)
      | Vector.null powers -> pure emptyDiplomatInfo
      | otherwise -> Left "card type powers is a non-empty array"
    Just (Object powersByPhase) ->
      case KeyMap.lookup (Key.fromText "3") powersByPhase of
        Nothing -> pure emptyDiplomatInfo
        Just phasePowers -> diplomatInfoFromPowers phasePowers
    Just _ -> Left "card type powers is not an object"

emptyDiplomatInfo :: DiplomatInfo
emptyDiplomatInfo = DiplomatInfo False False False False False []

diplomatInfoFromPowers :: Value -> Either Text DiplomatInfo
diplomatInfoFromPowers value =
  case value of
    Array powers -> foldM addPower emptyDiplomatInfo (Vector.toList powers)
    _ -> Left "card type phase powers is not an array"
  where
    addPower info powerValue = do
      power <- expectObject "card type power" powerValue
      case optionalField "power" power of
        Just powerNameValue -> do
          powerName <- textValue "card type power name" powerNameValue
          if powerName /= "diplomat"
            then pure info
            else do
              let noAlien = optionalBoolFlag "noalien" power
                  alien = optionalBoolFlag "alien" power
                  rebel = optionalBoolFlag "rebel" power
                  chromosome = optionalBoolFlag "chromosome" power
              payMilitaryPower <- payMilitaryPowerFromDiplomat power noAlien alien rebel chromosome
              pure info
                { diplomatHasNoAlien = diplomatHasNoAlien info || noAlien
                , diplomatHasAlien = diplomatHasAlien info || alien
                , diplomatHasRebel = diplomatHasRebel info || rebel
                , diplomatHasChromosome = diplomatHasChromosome info || chromosome
                , diplomatHasAny = diplomatHasAny info || not noAlien && not alien && not rebel && not chromosome
                , diplomatPayMilitaryPowers = diplomatPayMilitaryPowers info <> [payMilitaryPower]
                }
        Nothing -> pure info

payMilitaryPowerFromDiplomat :: Object -> Bool -> Bool -> Bool -> Bool -> Either Text PayMilitaryPower
payMilitaryPowerFromDiplomat power noAlien alien rebel chromosome = do
  condition <-
    case filter snd
      [ (PayMilitaryNonAlien, noAlien)
      , (PayMilitaryAlien, alien)
      , (PayMilitaryRebel, rebel)
      , (PayMilitaryChromosome, chromosome)
      ] of
      [] -> pure PayMilitaryNonAlien
      [(condition, _)] -> pure condition
      matches ->
        Left
          ( "card type diplomat has multiple pay-military conditions: "
              <> Text.intercalate ", " (fmap (showText . fst) matches)
          )
  rawDiscount <- optionalIntDefault 0 "card type diplomat discount" "discount" power
  if rawDiscount > 0
    then Left ("card type diplomat discount is positive: " <> showText rawDiscount)
    else
      pure PayMilitaryPower
        { payMilitaryCondition = condition
        , payMilitaryReduction = negate rawDiscount
        }

optionalBoolFlag :: Text -> Object -> Bool
optionalBoolFlag name obj =
  case optionalField name obj of
    Just (Bool True) -> True
    _ -> False

optionalIntDefault :: Int -> Text -> Text -> Object -> Either Text Int
optionalIntDefault defaultValue label name obj =
  case optionalField name obj of
    Nothing -> pure defaultValue
    Just Null -> pure defaultValue
    Just value -> intValue label value

temporaryMilitaryDiscardPowers :: Set Text
temporaryMilitaryDiscardPowers =
  Set.fromList
    [ "militaryforcetmp"
    , "militaryforcetmp_discard"
    ]

cardTypeHasAnyPower :: Set Text -> Object -> Either Text Bool
cardTypeHasAnyPower wanted obj =
  case optionalField "powers" obj of
    Nothing -> pure False
    Just Null -> pure False
    Just (Array powers)
      | Vector.null powers -> pure False
      | otherwise -> Left "card type powers is a non-empty array"
    Just (Object powersByPhase) ->
      any id <$> traverse (anyPowerMatches wanted) (KeyMap.elems powersByPhase)
    Just _ -> Left "card type powers is not an object"

anyPowerMatches :: Set Text -> Value -> Either Text Bool
anyPowerMatches wanted value =
  case value of
    Array powers -> any id <$> traverse powerMatches (Vector.toList powers)
    _ -> Left "card type phase powers is not an array"
  where
    powerMatches powerValue = do
      power <- expectObject "card type power" powerValue
      case optionalField "power" power of
        Just powerNameValue -> (`Set.member` wanted) <$> textValue "card type power name" powerNameValue
        Nothing -> pure False

takeoverSearchPowers :: Set Text
takeoverSearchPowers =
  Set.fromList
    [ "takeover"
    , "discardtotakeover"
    , "blocktakeover"
    , "cloaking"
    ]

discardProducePowers :: Set Text
discardProducePowers =
  Set.fromList
    [ "produceifdiscard"
    , "windfallproduceifdiscard"
    ]

cardTypePhase3MilitaryForcesFrom :: Object -> Either Text [Int]
cardTypePhase3MilitaryForcesFrom obj =
  case optionalField "powers" obj of
    Nothing -> pure []
    Just Null -> pure []
    Just (Array powers)
      | Vector.null powers -> pure []
      | otherwise -> Left "card type powers is a non-empty array"
    Just (Object powersByPhase) ->
      case KeyMap.lookup (Key.fromText "3") powersByPhase of
        Nothing -> pure []
        Just phasePowers -> phaseMilitaryForces phasePowers
    Just _ -> Left "card type powers is not an object"
  where
    phaseMilitaryForces value =
      case value of
        Array powers -> concat <$> traverse powerValueMilitaryForce (Vector.toList powers)
        _ -> Left "card type phase powers is not an array"

    powerValueMilitaryForce value = do
      power <- expectObject "card type power" value
      case optionalField "power" power of
        Just powerNameValue -> do
          powerName <- textValue "card type power name" powerNameValue
          if powerName == "militaryforce"
            then militaryForceValue power
            else pure []
        Nothing -> pure []

    militaryForceValue power =
      case optionalField "arg" power of
        Nothing -> pure []
        Just Null -> pure []
        Just argValue -> do
          arg <- expectObject "card type power arg" argValue
          case optionalField "force" arg of
            Nothing -> pure []
            Just forceValue -> (: []) <$> intValue "card type military force" forceValue

cardTypeHasConsumeCardPrestige :: Object -> Either Text Bool
cardTypeHasConsumeCardPrestige obj =
  case optionalField "powers" obj of
    Nothing -> pure False
    Just Null -> pure False
    Just (Array powers)
      | Vector.null powers -> pure False
      | otherwise -> Left "card type powers is a non-empty array"
    Just (Object powersByPhase) ->
      case KeyMap.lookup (Key.fromText "1") powersByPhase of
        Nothing -> pure False
        Just phasePowers -> phaseHasDiscardPrestige phasePowers
    Just _ -> Left "card type powers is not an object"
  where
    phaseHasDiscardPrestige value =
      case value of
        Array powers -> any id <$> traverse powerValueConsumesCardForPrestige (Vector.toList powers)
        _ -> Left "card type phase powers is not an array"

    powerValueConsumesCardForPrestige value =
      powerConsumesCardForPrestige =<< expectObject "card type power" value

powerConsumesCardForPrestige :: Object -> Either Text Bool
powerConsumesCardForPrestige power =
  case optionalField "power" power of
    Just powerNameValue -> do
      powerName <- textValue "card type power name" powerNameValue
      if powerName /= "consumecard"
        then pure False
        else powerOutputHasPrestige power
    Nothing -> pure False

powerOutputHasPrestige :: Object -> Either Text Bool
powerOutputHasPrestige power =
  case optionalField "arg" power of
    Nothing -> pure False
    Just Null -> pure False
    Just argValue -> do
      arg <- expectObject "card type power arg" argValue
      case optionalField "output" arg of
        Nothing -> pure False
        Just Null -> pure False
        Just outputValue -> do
          output <- expectObject "card type power output" outputValue
          pure (optionalField "pr" output /= Nothing)

cardName :: Map Int Text -> Value -> Either Text Text
cardName cardTypes value = do
  obj <- expectObject "card" value
  typeId <- intValue "card type" =<< field "type" obj
  case Map.lookup typeId cardTypes of
    Just name -> pure (canonicalCardName name)
    Nothing -> Left ("unknown card type " <> showText typeId)

canonicalCardName :: Text -> Text
canonicalCardName "Alien toy shop" = "Alien Toy Shop"
canonicalCardName "Malevolent Life Forms" = "Malevolent Lifeforms"
canonicalCardName "Blaster Gem Mine" = "Blaster Gem Mines"
canonicalCardName "Deep Space Symbionts, LTD." = "Deep Space Symbionts, Ltd."
canonicalCardName "Designer Species, ULTD." = "Designer Species, Ultd."
canonicalCardName "Galactic Clearinghouse" = "Galactic Clearing House"
canonicalCardName "Lifeforms, inc." = "Lifeforms, Inc"
canonicalCardName "Rebel Sympathisers" = "Rebel Sympathizers"
canonicalCardName "Retrofit & Salvage, inc." = "Retrofit & Salvage, Inc"
canonicalCardName "Terraforming project" = "Terraforming Project"
canonicalCardName name = name

cardId :: Value -> Either Text Int
cardId value = do
  obj <- expectObject "card" value
  intValue "card id" =<< field "id" obj

notificationObjects :: Object -> Either Text [Object]
notificationObjects root = do
  streams <- notificationStreams root
  let indexedPackets =
        [ (packetId packet, streamIx, packetIx, packet)
        | (streamIx, packets) <- zip [0 :: Int ..] streams
        , (packetIx, packet) <- zip [0 :: Int ..] packets
        ]
      sortedPackets = fmap fourth (List.sortOn packetSortKey indexedPackets)
  dedupPackets Set.empty sortedPackets
  where
    packetSortKey (pid, streamIx, packetIx, _) = (pid, streamIx, packetIx)
    fourth (_, _, _, value) = value

notificationStreams :: Object -> Either Text [[Value]]
notificationStreams root = do
  logs <- objectField "logs" root
  logsData <- objectField "data" logs
  publicPackets <- arrayField "logs" logsData
  privatePackets <- privateNotificationStreams root
  pure (publicPackets : privatePackets)

privateNotificationStreams :: Object -> Either Text [[Value]]
privateNotificationStreams root =
  case optionalField "gamelogs" root of
    Nothing -> pure []
    Just (Object gamelogs) ->
      traverse streamPackets (objectValues gamelogs)
    Just _ -> Left "gamelogs is not an object"

streamPackets :: Value -> Either Text [Value]
streamPackets value =
  case value of
    Array packets -> pure (Vector.toList packets)
    Object obj ->
      case optionalField "data" obj of
        Just inner -> streamPackets inner
        Nothing ->
          case optionalField "logs" obj of
            Just inner -> streamPackets inner
            Nothing -> pure []
    _ -> pure []

packetId :: Value -> Int
packetId value =
  case value of
    Object packet ->
      case optionalField "packet_id" packet of
        Just packetIdValue ->
          either (const 0) id (intValue "packet_id" packetIdValue)
        Nothing -> 0
    _ -> 0

dedupPackets :: Set Text -> [Value] -> Either Text [Object]
dedupPackets _ [] = pure []
dedupPackets seen (packetValue : rest) = do
  notifications <- packetNotifications packetValue
  let (seen', kept) = dedupNotifications seen notifications
  (kept <>) <$> dedupPackets seen' rest

packetNotifications :: Value -> Either Text [Object]
packetNotifications value = do
      packet <- expectObject "log packet" value
      case optionalField "data" packet of
        Nothing -> pure []
        Just (Array notifications) ->
          traverse (expectObject "notification") (Vector.toList notifications)
        Just _ -> Left "log packet data is not an array"

dedupNotifications :: Set Text -> [Object] -> (Set Text, [Object])
dedupNotifications seen notifications =
  foldl step (seen, []) notifications
  where
    step (currentSeen, kept) notification =
      case notificationUid notification of
        Nothing -> (currentSeen, kept <> [notification])
        Just uid
          | uid `Set.member` currentSeen -> (currentSeen, kept)
          | otherwise -> (Set.insert uid currentSeen, kept <> [notification])

notificationUid :: Object -> Maybe Text
notificationUid notification =
  case optionalField "uid" notification of
    Just (String uid) -> Just uid
    _ -> Nothing

notificationType :: Object -> Text
notificationType notification =
  case optionalField "type" notification of
    Just (String typ) -> typ
    _ -> ""

showText :: Show a => a -> Text
showText = Text.pack . show
