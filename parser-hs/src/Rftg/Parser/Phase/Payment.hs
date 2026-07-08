{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Parser.Phase.Payment
  ( parsePaymentChoices
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
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
  , objectField
  , objectValues
  , optionalField
  , expectObject
  , textValue
  , valueText
  )
import Rftg.Bga.State
  ( BgaState
  , bgaStateIsTerraformingEngineers
  , optionalBgaStateField
  )
import Rftg.Bga.Types
  ( Player (..)
  , PlayerId (..)
  )
import Rftg.Keldon.Script
  ( ChoiceMode (..)
  , ChoiceOrder
  , KeldonChoice (..)
  , KeldonScript
  , ScriptLine (..)
  , appendScript
  , choiceScriptAt
  , emptyScript
  )
import Rftg.Parser.CardIndex
  ( CardIndex (..)
  , discardCardIds
  , initialCardIndex
  , learnNotificationCards
  , lookupKnownCardName
  )
import Rftg.Parser.Common
  ( CardTypeInfo (..)
  , PayMilitaryCondition (..)
  , PayMilitaryPower (..)
  , cardId
  , cardName
  , canonicalCardName
  , notificationObjects
  , notificationType
  , parseCardTypeInfos
  , parsePlayers
  )
import Rftg.Parser.Phase.Cursor
  ( Phase (..)
  , PhaseCursor (..)
  , advancePhaseCursor
  , cursorChoiceOrder
  , initialPhaseCursor
  )

data PendingDiscard = PendingDiscard
  { pendingDiscardIds :: [Int]
  , pendingDiscardCards :: [Text]
  }
  deriving stock (Eq, Show)

data CardCostInfo = CardCostInfo
  { cardCostMilitaryForce :: Bool
  , cardCostUseContactSpecialist :: Bool
  , cardCostHasPayMilitaryOption :: Bool
  }
  deriving stock (Eq, Show)

data PaymentLine = PaymentLine
  { paymentLinePlayer :: PlayerId
  , paymentLineOrder :: ChoiceOrder
  , paymentLineMode :: ChoiceMode
  , paymentLineCards :: [Text]
  , paymentLineSpecials :: [Text]
  }
  deriving stock (Eq, Show)

data PayMilitaryCandidate = PayMilitaryCandidate
  { payMilitaryCandidateName :: Text
  , payMilitaryCandidateReduction :: Int
  , payMilitaryCandidatePreferContact :: Bool
  }
  deriving stock (Eq, Show)

data PaymentState = PaymentState
  { phaseCursor :: PhaseCursor
  , currentBgaState :: Maybe BgaState
  , cardIndex :: CardIndex
  , startSeen :: Bool
  , tableauCards :: Map PlayerId [Text]
  , cardCosts :: Map Int CardCostInfo
  , pendingTableauPayments :: Map PlayerId [Text]
  , pendingMercenaries :: Map PlayerId [Text]
  , pendingDiscards :: Map PlayerId PendingDiscard
  , pendingZeroPaymentWorlds :: Map PlayerId Text
  , pendingUpgrades :: Map PlayerId Text
  , discardedTableauCards :: Set Int
  , playedCards :: Set Int
  , paymentLines :: [PaymentLine]
  }
  deriving stock (Eq, Show)

parsePaymentChoices :: Value -> Either Text KeldonScript
parsePaymentChoices rootValue = do
  root <- expectObject "root" rootValue
  players <- parsePlayers root
  gamedatas <- objectField "gamedatas" root
  cardTypeInfos <- parseCardTypeInfos gamedatas
  let cardTypes = fmap cardTypeName cardTypeInfos
      cardInfosByName = Map.fromList [(cardTypeName info, info) | info <- Map.elems cardTypeInfos]
  startingCardIndex <- initialCardIndex players gamedatas cardTypes
  notifications <- notificationObjects root
  walked <-
    foldM
      (paymentStep players cardInfosByName cardTypes)
      (emptyPaymentState players startingCardIndex)
      (zip [0 :: Int ..] notifications)
  paymentScript players walked

emptyPaymentState :: [Player] -> CardIndex -> PaymentState
emptyPaymentState players startingCardIndex = PaymentState
  { phaseCursor = initialPhaseCursor
  , currentBgaState = Nothing
  , cardIndex = startingCardIndex
  , startSeen = False
  , tableauCards = Map.fromList [(playerId player, []) | player <- players]
  , cardCosts = Map.empty
  , pendingTableauPayments = Map.empty
  , pendingMercenaries = Map.empty
  , pendingDiscards = Map.empty
  , pendingZeroPaymentWorlds = Map.empty
  , pendingUpgrades = Map.empty
  , discardedTableauCards = Set.empty
  , playedCards = Set.empty
  , paymentLines = []
  }

paymentStep ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  PaymentState ->
  (Int, Object) ->
  Either Text PaymentState
paymentStep players cardInfosByName cardTypes state (eventIx, notification) = do
  updatedCardIndex <- learnNotificationCards cardTypes (cardIndex state) notification
  let stateWithCards = state { cardIndex = updatedCardIndex }
  case notificationType notification of
    "gameStateChange" -> handleGameState stateWithCards notification
    "showTableau" -> handleShowTableau cardTypes stateWithCards notification
    "cardcost" -> handleCardCost stateWithCards notification
    "discard" -> handleDiscard stateWithCards notification
    "discardfromtableau" -> handleDiscardFromTableau cardInfosByName stateWithCards notification
    "mercenary_used" -> handleMercenaryUsed stateWithCards notification
    "playcard" -> handlePlayCard players cardInfosByName cardTypes eventIx stateWithCards notification
    "consume" -> handleSettleCostConsume players cardInfosByName stateWithCards notification
    "updatePrestige" -> handleUpdatePrestige cardInfosByName stateWithCards notification
    _ -> pure stateWithCards

handleGameState :: PaymentState -> Object -> Either Text PaymentState
handleGameState state notification = do
  args <- objectField "args" notification
  maybeBgaState <- optionalBgaStateField "gameStateChange id" args
  case maybeBgaState of
    Nothing -> pure state
    Just bgaState ->
      pure state
        { phaseCursor = advancePhaseCursor bgaState (phaseCursor state)
        , currentBgaState = Just bgaState
        }

handleShowTableau :: Map Int Text -> PaymentState -> Object -> Either Text PaymentState
handleShowTableau cardTypes state notification = do
  args <- objectField "args" notification
  cardsObject <- expectObject "showTableau cards" =<< field "cards" args
  entries <- traverse tableauEntry (objectValues cardsObject)
  pure state
    { startSeen = True
    , tableauCards = foldl addEntry (tableauCards state) entries
    }
  where
    tableauEntry cardValue = do
      pid <- cardPlayerId cardValue
      name <- cardName cardTypes cardValue
      pure (pid, name)
    addEntry table (pid, name) =
      Map.alter (Just . appendUnique name . maybe [] id) pid table

handleCardCost :: PaymentState -> Object -> Either Text PaymentState
handleCardCost state notification = do
  args <- objectField "args" notification
  cardValue <- field "card" args
  cardInstanceId <- cardId cardValue
  info <- cardCostInfoFrom args
  pure state { cardCosts = Map.insert cardInstanceId info (cardCosts state) }

cardCostInfoFrom :: Object -> Either Text CardCostInfo
cardCostInfoFrom args = do
  militaryForce <- optionalBoolDefault True "cardcost military_force" "military_force" args
  useContact <- optionalBoolDefault False "cardcost use_contact_specialist" "use_contact_specialist" args
  alternatives <- immediateAlternativesHavePayOption args
  pure CardCostInfo
    { cardCostMilitaryForce = militaryForce
    , cardCostUseContactSpecialist = useContact
    , cardCostHasPayMilitaryOption = not militaryForce || alternatives
    }

immediateAlternativesHavePayOption :: Object -> Either Text Bool
immediateAlternativesHavePayOption args =
  case optionalField "immediate_alternatives" args of
    Nothing -> pure False
    Just (Array values) -> any id <$> traverse alternativeHasPayOption (Vector.toList values)
    Just _ -> Left "cardcost immediate_alternatives is not an array"
  where
    alternativeHasPayOption value = do
      obj <- expectObject "cardcost immediate alternative" value
      case optionalField "kind" obj of
        Nothing -> pure False
        Just kindValue -> (== "pay") <$> textValue "cardcost immediate alternative kind" kindValue

optionalBoolDefault :: Bool -> Text -> Text -> Object -> Either Text Bool
optionalBoolDefault defaultValue label name obj =
  case optionalField name obj of
    Nothing -> pure defaultValue
    Just (Bool b) -> pure b
    Just _ -> Left (label <> " is not boolean")

handleDiscard :: PaymentState -> Object -> Either Text PaymentState
handleDiscard state notification =
  if cursorPhase (phaseCursor state) `elem` [Explore, Discard, Produce]
    then pure state
    else do
      args <- objectField "args" notification
      cardIds <- discardCardIds args
      case discardOwnerMaybe (cardIndex state) cardIds of
        Nothing -> pure state
        Just ownerResult -> do
          owner <- ownerResult
          cards <- traverse (lookupKnownCardName (cardIndex state)) cardIds
          let existing =
                if Map.member owner (pendingMercenaries state)
                  then Map.lookup owner (pendingDiscards state)
                  else Nothing
              pending =
                case existing of
                  Nothing -> PendingDiscard cardIds cards
                  Just old ->
                    PendingDiscard
                      (pendingDiscardIds old <> cardIds)
                      (pendingDiscardCards old <> cards)
          pure state { pendingDiscards = Map.insert owner pending (pendingDiscards state) }

handleDiscardFromTableau :: Map Text CardTypeInfo -> PaymentState -> Object -> Either Text PaymentState
handleDiscardFromTableau cardInfosByName state notification = do
  args <- objectField "args" notification
  case optionalField "card" args of
    Nothing -> pure state
    Just cardValue -> do
      cardInstanceId <- intValue "discardfromtableau card" cardValue
      if cardInstanceId `Set.member` discardedTableauCards state
        then pure state
        else do
          name <- lookupKnownCardName (cardIndex state) cardInstanceId
          owner <- lookupCardOwner cardInstanceId state
          info <- lookupCardInfo cardInfosByName name
          let stateWithoutCard = state
                { discardedTableauCards = Set.insert cardInstanceId (discardedTableauCards state)
                , tableauCards = Map.adjust (filter (/= name)) owner (tableauCards state)
                }
          if not (startSeen stateWithoutCard)
            then pure stateWithoutCard
            else if cardTypeType info == "world"
              && (maybe False bgaStateIsTerraformingEngineers (currentBgaState state) || not (cardTypeIsSettlePaymentDiscardSource info))
            then
              pure stateWithoutCard
                { pendingUpgrades = Map.insert owner name (pendingUpgrades stateWithoutCard)
                }
            else
              pure stateWithoutCard
                { pendingTableauPayments =
                    Map.alter (Just . (<> [name]) . maybe [] id) owner (pendingTableauPayments stateWithoutCard)
                }

handleMercenaryUsed :: PaymentState -> Object -> Either Text PaymentState
handleMercenaryUsed state notification = do
  args <- objectField "args" notification
  sourceId <- intValue "mercenary_used card" =<< field "card" args
  sourceName <- lookupKnownCardName (cardIndex state) sourceId
  owner <- lookupCardOwner sourceId state
  pure state
    { pendingMercenaries =
        Map.alter (Just . appendUnique sourceName . maybe [] id) owner (pendingMercenaries state)
    }

handlePlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  Int ->
  PaymentState ->
  Object ->
  Either Text PaymentState
handlePlayCard players cardInfosByName cardTypes eventIx state notification = do
  args <- objectField "args" notification
  cardValue <- field "card" args
  cardInstanceId <- cardId cardValue
  case optionalField "money" args of
    Just _ ->
      if cardInstanceId `Set.member` playedCards state
        then pure state
        else handlePaidPlayCard players cardInfosByName cardTypes eventIx state args cardValue cardInstanceId
    Nothing -> handlePaymentSpecialPlayCard players cardInfosByName state notification args cardValue cardInstanceId

handlePaidPlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  Map Int Text ->
  Int ->
  PaymentState ->
  Object ->
  Value ->
  Int ->
  Either Text PaymentState
handlePaidPlayCard players cardInfosByName cardTypes eventIx state args cardValue cardInstanceId = do
  pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
  _ <- lookupPlayer players pid
  card <- cardName cardTypes cardValue
  cardInfo <- lookupCardInfo cardInfosByName card
  if cardTypeType cardInfo == "world" && Map.member pid (pendingUpgrades state)
    then
      pure state
        { playedCards = Set.insert cardInstanceId (playedCards state)
        , tableauCards = Map.alter (Just . appendUnique card . maybe [] id) pid (tableauCards state)
        , pendingUpgrades = Map.delete pid (pendingUpgrades state)
        , pendingMercenaries = Map.delete pid (pendingMercenaries state)
        , pendingDiscards = Map.delete pid (pendingDiscards state)
        , pendingTableauPayments = Map.delete pid (pendingTableauPayments state)
        , pendingZeroPaymentWorlds = Map.delete pid (pendingZeroPaymentWorlds state)
        }
    else do
      moneyIds <- traverse (intValue "playcard money") =<< arrayField "money" args
      money <- traverse (lookupKnownCardName (cardIndex state)) moneyIds
      let merc = if cardTypeIsMilitary cardInfo then Map.findWithDefault [] pid (pendingMercenaries state) else []
          pending = if null merc then Nothing else Map.lookup pid (pendingDiscards state)
          mercMoney = maybe [] pendingDiscardCards pending
          allMoney = mercMoney <> money
          cc = Map.lookup cardInstanceId (cardCosts state)
          paySource =
            if null merc && cardTypeIsMilitary cardInfo && (not (null allMoney) || maybe False (not . cardCostMilitaryForce) cc)
              then payMilitarySource pid cardInfo (maybe False cardCostUseContactSpecialist cc) state cardInfosByName
              else Nothing
          tableauPay = unique (Map.findWithDefault [] pid (pendingTableauPayments state))
          specials = unique (merc <> maybe [] (: []) paySource <> tableauPay)
          mode = if null allMoney then Optional else Required
          stateCleared = state
            { playedCards = Set.insert cardInstanceId (playedCards state)
            , tableauCards = Map.alter (Just . appendUnique card . maybe [] id) pid (tableauCards state)
            , pendingMercenaries = Map.delete pid (pendingMercenaries state)
            , pendingDiscards = if null merc then pendingDiscards state else Map.delete pid (pendingDiscards state)
            , pendingTableauPayments = Map.delete pid (pendingTableauPayments state)
            , pendingZeroPaymentWorlds = Map.delete pid (pendingZeroPaymentWorlds state)
            }
      stateWithLine <- addPaymentLine players eventIx pid mode allMoney specials stateCleared
      if null allMoney && null specials && cardTypeIsMilitary cardInfo && maybe False cardCostHasPayMilitaryOption cc
        then pure stateWithLine { pendingZeroPaymentWorlds = Map.insert pid card (pendingZeroPaymentWorlds stateWithLine) }
        else pure stateWithLine

handlePaymentSpecialPlayCard ::
  [Player] ->
  Map Text CardTypeInfo ->
  PaymentState ->
  Object ->
  Object ->
  Value ->
  Int ->
  Either Text PaymentState
handlePaymentSpecialPlayCard players cardInfosByName state notification _args _cardValue cardInstanceId = do
  let logText = maybe "" valueText (optionalField "log" notification)
      hasPaymentSpecialLog = "using a power to pay for Military worlds" `Text.isInfixOf` logText
  if cardInstanceId `Set.member` playedCards state && not hasPaymentSpecialLog
    then pure state
    else do
      args <- objectField "args" notification
      pid <- PlayerId <$> (intValue "playcard player" =<< field "player" args)
      _ <- lookupPlayer players pid
      card <- lookupKnownCardName (cardIndex state) cardInstanceId
      if hasPaymentSpecialLog
        then do
          targetInfo <- lookupCardInfo cardInfosByName card
          case payMilitarySource pid targetInfo False state cardInfosByName of
            Nothing -> Left ("PFM play of " <> card <> " has no payment source")
            Just source -> do
              withSpecial <- appendPaymentSpecial pid source state
              pure withSpecial
                { pendingZeroPaymentWorlds = Map.delete pid (pendingZeroPaymentWorlds withSpecial)
                , playedCards = Set.insert cardInstanceId (playedCards withSpecial)
                }
        else pure state { playedCards = Set.insert cardInstanceId (playedCards state) }

handleSettleCostConsume :: [Player] -> Map Text CardTypeInfo -> PaymentState -> Object -> Either Text PaymentState
handleSettleCostConsume players cardInfosByName state notification = do
  args <- objectField "args" notification
  case optionalField "world_id" args of
    Nothing -> pure state
    Just worldIdValue -> do
      powerId <- intValue "consume world_id" worldIdValue
      powerCard <- lookupKnownCardName (cardIndex state) powerId
      powerInfo <- lookupCardInfo cardInfosByName powerCard
      if cursorPhase (phaseCursor state) == Settle && cardTypeHasGoodForSettleCost powerInfo
        then do
          pid <- consumePlayer players powerId args state
          appendPaymentSpecial pid powerCard state
        else pure state

handleUpdatePrestige :: Map Text CardTypeInfo -> PaymentState -> Object -> Either Text PaymentState
handleUpdatePrestige cardInfosByName state notification = do
  args <- objectField "args" notification
  pid <- PlayerId <$> (intValue "updatePrestige player_id" =<< field "player_id" args)
  nbr <- intValue "updatePrestige nbr" =<< field "nbr" args
  case optionalField "card_name" args of
    Nothing -> pure state
    Just sourceValue -> do
      source <- canonicalCardName <$> textValue "updatePrestige card_name" sourceValue
      sourceInfo <- lookupCardInfo cardInfosByName source
      let withPrestige =
            if nbr < 0 && cardTypeHasPrestigeMilitary sourceInfo
              then appendPaymentSpecial pid source state
              else pure state
      withPrestigeState <- withPrestige
      if cardTypeHasDiplomatBonus sourceInfo
        then
          case Map.lookup pid (pendingZeroPaymentWorlds withPrestigeState) of
            Nothing -> pure withPrestigeState
            Just target -> do
              targetInfo <- lookupCardInfo cardInfosByName target
              case payMilitarySource pid targetInfo False withPrestigeState cardInfosByName of
                Nothing -> Left ("diplomatbonus payment for " <> target <> " has no PFM source")
                Just paymentSource -> do
                  withSpecial <- appendPaymentSpecial pid paymentSource withPrestigeState
                  pure withSpecial
                    { pendingZeroPaymentWorlds = Map.delete pid (pendingZeroPaymentWorlds withSpecial)
                    }
        else pure withPrestigeState

consumePlayer :: [Player] -> Int -> Object -> PaymentState -> Either Text PlayerId
consumePlayer players powerId args state =
  case optionalField "player_id" args of
    Just pidValue -> do
      pid <- PlayerId <$> intValue "consume player_id" pidValue
      _ <- lookupPlayer players pid
      pure pid
    Nothing -> lookupCardOwner powerId state

addPaymentLine ::
  [Player] ->
  Int ->
  PlayerId ->
  ChoiceMode ->
  [Text] ->
  [Text] ->
  PaymentState ->
  Either Text PaymentState
addPaymentLine players eventIx pid mode cards specials state = do
  _ <- lookupPlayer players pid
  order <- cursorChoiceOrder (phaseCursor state) eventIx
  let line = PaymentLine pid order mode cards specials
  pure state { paymentLines = paymentLines state <> [line] }

appendPaymentSpecial :: PlayerId -> Text -> PaymentState -> Either Text PaymentState
appendPaymentSpecial pid special state =
  case go [] (paymentLines state) of
    Nothing -> Left ("payment special " <> special <> " has no payment line")
    Just updated -> pure state { paymentLines = updated }
  where
    go _ [] = Nothing
    go seen (line : rest)
      | paymentLinePlayer line == pid && not (anyLaterForPid rest) =
          Just (seen <> [line { paymentLineSpecials = appendUnique special (paymentLineSpecials line) }] <> rest)
      | otherwise = go (seen <> [line]) rest
    anyLaterForPid = any ((== pid) . paymentLinePlayer)

payMilitarySource :: PlayerId -> CardTypeInfo -> Bool -> PaymentState -> Map Text CardTypeInfo -> Maybe Text
payMilitarySource pid target preferContact state cardInfosByName =
  payMilitaryCandidateName <$> foldl chooseCandidate Nothing candidates
  where
    sources = Map.findWithDefault [] pid (tableauCards state)
    candidates = concatMap sourceCandidates sources

    sourceCandidates source =
      case Map.lookup source cardInfosByName of
        Nothing -> []
        Just info ->
          case bestMatchingPayMilitaryPower target info of
            Nothing -> []
            Just power ->
              [ PayMilitaryCandidate
                  { payMilitaryCandidateName = source
                  , payMilitaryCandidateReduction = payMilitaryReduction power
                  , payMilitaryCandidatePreferContact = preferContact && source == "Contact Specialist"
                  }
              ]

chooseCandidate :: Maybe PayMilitaryCandidate -> PayMilitaryCandidate -> Maybe PayMilitaryCandidate
chooseCandidate Nothing candidate = Just candidate
chooseCandidate (Just current) candidate
  | payMilitaryCandidateReduction candidate > payMilitaryCandidateReduction current = Just candidate
  | payMilitaryCandidateReduction candidate == payMilitaryCandidateReduction current
      && payMilitaryCandidatePreferContact candidate
      && not (payMilitaryCandidatePreferContact current) =
      Just candidate
  | otherwise = Just current

bestMatchingPayMilitaryPower :: CardTypeInfo -> CardTypeInfo -> Maybe PayMilitaryPower
bestMatchingPayMilitaryPower target source =
  foldl choosePower Nothing (filter (payMilitaryPowerMatches target) (cardTypePayMilitaryPowers source))

choosePower :: Maybe PayMilitaryPower -> PayMilitaryPower -> Maybe PayMilitaryPower
choosePower Nothing power = Just power
choosePower (Just current) power
  | payMilitaryReduction power > payMilitaryReduction current = Just power
  | otherwise = Just current

payMilitaryPowerMatches :: CardTypeInfo -> PayMilitaryPower -> Bool
payMilitaryPowerMatches target power =
  case payMilitaryCondition power of
    PayMilitaryAlien -> targetAlienGood
    PayMilitaryRebel -> not targetAlienGood && "rebel" `Set.member` targetCategories
    PayMilitaryChromosome -> not targetAlienGood && "chromosome" `Set.member` targetCategories
    PayMilitaryNonAlien -> not targetAlienGood
  where
    targetAlienGood = cardTypeGoodKind target == Just 4
    targetCategories = cardTypeCategories target

paymentScript :: [Player] -> PaymentState -> Either Text KeldonScript
paymentScript players state =
  foldM addLine emptyScript (paymentLines state)
  where
    addLine script line = do
      player <- lookupPlayer players (paymentLinePlayer line)
      let seat = playerSeat player
          lineScript =
            choiceScriptAt
              (paymentLineOrder line)
              seat
              [Choice (paymentLineMode line) seat (ChoosePayment (paymentLineCards line) (paymentLineSpecials line))]
      pure (script `appendScript` lineScript)

cardPlayerId :: Value -> Either Text PlayerId
cardPlayerId value = do
  cardObject <- expectObject "card" value
  PlayerId <$> (intValue "card location_arg" =<< field "location_arg" cardObject)

lookupCardOwner :: Int -> PaymentState -> Either Text PlayerId
lookupCardOwner cardInstanceId state =
  case Map.lookup cardInstanceId (knownCardOwners (cardIndex state)) of
    Just owner -> pure owner
    Nothing -> Left ("card owner unknown for " <> showText cardInstanceId)

lookupCardInfo :: Map Text CardTypeInfo -> Text -> Either Text CardTypeInfo
lookupCardInfo cardInfosByName name =
  case Map.lookup name cardInfosByName of
    Just info -> pure info
    Nothing -> Left ("unknown card type info for " <> name)

discardOwnerMaybe :: CardIndex -> [Int] -> Maybe (Either Text PlayerId)
discardOwnerMaybe cardIndex cardIds =
  case fmap (`Map.lookup` knownCardOwners cardIndex) cardIds of
    [] -> Just (Left "discard has no cards")
    owners
      | all (== Nothing) owners -> Nothing
      | any (== Nothing) owners ->
          Just (Left ("discard has partly unknown owners: " <> Text.intercalate ", " (fmap renderOwner owners)))
      | otherwise ->
          case sequence owners of
            Nothing -> Just (Left "discard owner lookup unexpectedly failed")
            Just (owner : rest)
              | all (== owner) rest -> Just (Right owner)
              | otherwise ->
                  Just
                    ( Left
                        ( "discard has mixed owners: "
                            <> Text.intercalate ", " (fmap (showText . unPlayerId) (owner : rest))
                        )
                    )
            Just [] -> Just (Left "discard has no cards")
  where
    renderOwner Nothing = "<unknown>"
    renderOwner (Just owner) = showText (unPlayerId owner)

lookupPlayer :: [Player] -> PlayerId -> Either Text Player
lookupPlayer players pid =
  case filter ((== pid) . playerId) players of
    [player] -> pure player
    [] -> Left ("unknown player " <> showText (unPlayerId pid))
    _ -> Left ("duplicate player " <> showText (unPlayerId pid))

appendUnique :: Eq a => a -> [a] -> [a]
appendUnique value values =
  if value `elem` values
    then values
    else values <> [value]

unique :: Eq a => [a] -> [a]
unique = foldl (\seen value -> appendUnique value seen) []

showText :: Show a => a -> Text
showText = Text.pack . show
