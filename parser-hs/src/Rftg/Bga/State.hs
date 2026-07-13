{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Rftg.Bga.State
  ( BgaActionStep (..)
  , BgaConsumeStep (..)
  , BgaDevelopStep (..)
  , BgaDiscardStep (..)
  , BgaExploreStep (..)
  , BgaGameOverStep (..)
  , BgaPhase (..)
  , BgaProduceStep (..)
  , BgaSearchStep (..)
  , BgaSettleState (..)
  , BgaSetupStep (..)
  , BgaState (..)
  , bgaPhaseOrder
  , bgaSettleSourceName
  , bgaStateClearsSettleSource
  , bgaStateFromId
  , bgaStateHasPhase
  , bgaStateId
  , bgaStateIsNewActionRound
  , bgaStateIsDevelopMain
  , bgaStateIsExploreStart
  , bgaStateIsFinalGameOver
  , bgaStateIsSearch
  , bgaStateIsSearchDone
  , bgaStateIsSearchStart
  , bgaStateIsSelectLastAction
  , bgaStateIsSettleMain
  , bgaStateIsTerraformingEngineers
  , bgaStateLeavesPhase
  , bgaStatePhase
  , bgaStatePhaseOrder
  , optionalBgaStateField
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as Text

import Rftg.Bga.Json
  ( Object
  , intValue
  , optionalField
  )
import Rftg.Bga.Types (StateId (..))

data BgaPhase
  = BgaAction
  | BgaExplore
  | BgaDevelop
  | BgaSettle
  | BgaConsume
  | BgaProduce
  | BgaDiscard
  | BgaGameOver
  deriving stock (Eq, Show)

data BgaSetupStep
  = BgaInitialDiscard
  | BgaInitialDiscardHomeWorld
  | BgaShowTableau
  | BgaSetupFinished
  | BgaExploreConsume
  | BgaInitialDiscardAncientRace
  | BgaInitialDiscardScavenger
  deriving stock (Eq, Show)

data BgaActionStep
  = BgaPhaseChoice
  | BgaPhaseChoiceSignal
  | BgaPhaseChoiceCrystal
  deriving stock (Eq, Show)

data BgaExploreStep
  = BgaExploreMain
  | BgaPostExploreProcess
  deriving stock (Eq, Show)

data BgaDevelopStep
  = BgaDevelopMain
  | BgaDevelopProcess
  | BgaDevelopDiscard
  | BgaDevelopBonus
  | BgaAfterDevelopCheck
  deriving stock (Eq, Show)

data BgaSettleState
  = BgaSettleMain
  | BgaSettleDiscardToPutGood
  | BgaSettleTakeoverCheck
  | BgaSettleTakeoverPrevent
  | BgaSettleTakeoverAttackerBoost
  | BgaSettleTakeoverNextBoost
  | BgaSettleTakeoverDefenderBoost
  | BgaSettleTakeoverResolution
  | BgaSettleProcess
  | BgaSettleDiscard
  | BgaSettleImprovedLogistics
  | BgaSettleRebelSneakAttack
  | BgaSettleImperiumSupplyConvoy
  | BgaSettleTerraformingProject
  | BgaSettleTerraformingEngineers
  deriving stock (Eq, Show)

data BgaConsumeStep
  = BgaConsumeSell
  | BgaConsumeMain
  | BgaConsumeProcess
  deriving stock (Eq, Show)

data BgaProduceStep
  = BgaProductionIntro
  | BgaProductionWindfall
  | BgaPostProductionProcess
  | BgaProductionProcess
  deriving stock (Eq, Show)

data BgaDiscardStep
  = BgaEndRoundDiscard
  | BgaEndRound
  deriving stock (Eq, Show)

data BgaGameOverStep
  = BgaEndScore
  | BgaGameEnd
  | BgaDraftNewRound
  deriving stock (Eq, Show)

data BgaSearchStep
  = BgaSearchActionCheck
  | BgaSearchAction
  | BgaSearchActionChoose
  deriving stock (Eq, Show)

data BgaState
  = BgaSetupState BgaSetupStep
  | BgaActionState BgaActionStep
  | BgaExploreState BgaExploreStep
  | BgaDevelopState BgaDevelopStep
  | BgaSettleState BgaSettleState
  | BgaConsumeState BgaConsumeStep
  | BgaProduceState BgaProduceStep
  | BgaDiscardState BgaDiscardStep
  | BgaGameOverState BgaGameOverStep
  | BgaSearchState BgaSearchStep
  deriving stock (Eq, Show)

bgaStateFromId :: StateId -> Either Text BgaState
bgaStateFromId (StateId raw) =
  case raw of
    2 -> pure (BgaSetupState BgaInitialDiscard)
    3 -> pure (BgaSetupState BgaInitialDiscardHomeWorld)
    4 -> pure (BgaSetupState BgaShowTableau)
    5 -> pure (BgaSetupState BgaSetupFinished)
    10 -> pure (BgaActionState BgaPhaseChoice)
    11 -> pure (BgaActionState BgaPhaseChoiceSignal)
    12 -> pure (BgaActionState BgaPhaseChoiceCrystal)
    19 -> pure (BgaSetupState BgaExploreConsume)
    20 -> pure (BgaExploreState BgaExploreMain)
    21 -> pure (BgaExploreState BgaPostExploreProcess)
    30 -> pure (BgaDevelopState BgaDevelopMain)
    31 -> pure (BgaDevelopState BgaDevelopProcess)
    40 -> pure (BgaSettleState BgaSettleMain)
    41 -> pure (BgaSettleState BgaSettleDiscardToPutGood)
    42 -> pure (BgaSettleState BgaSettleImprovedLogistics)
    43 -> pure (BgaSettleState BgaSettleTakeoverCheck)
    44 -> pure (BgaSettleState BgaSettleTakeoverAttackerBoost)
    45 -> pure (BgaSettleState BgaSettleTakeoverDefenderBoost)
    46 -> pure (BgaSettleState BgaSettleTakeoverResolution)
    48 -> pure (BgaSettleState BgaSettleTakeoverNextBoost)
    49 -> pure (BgaSettleState BgaSettleTakeoverPrevent)
    50 -> pure (BgaConsumeState BgaConsumeSell)
    51 -> pure (BgaConsumeState BgaConsumeMain)
    52 -> pure (BgaConsumeState BgaConsumeProcess)
    60 -> pure (BgaProduceState BgaProductionIntro)
    61 -> pure (BgaProduceState BgaProductionWindfall)
    62 -> pure (BgaProduceState BgaPostProductionProcess)
    69 -> pure (BgaProduceState BgaProductionProcess)
    70 -> pure (BgaDiscardState BgaEndRoundDiscard)
    71 -> pure (BgaDiscardState BgaEndRound)
    98 -> pure (BgaGameOverState BgaEndScore)
    99 -> pure (BgaGameOverState BgaGameEnd)
    100 -> pure (BgaGameOverState BgaDraftNewRound)
    200 -> pure (BgaSearchState BgaSearchActionCheck)
    201 -> pure (BgaSearchState BgaSearchAction)
    202 -> pure (BgaSearchState BgaSearchActionChoose)
    230 -> pure (BgaDevelopState BgaDevelopDiscard)
    231 -> pure (BgaDevelopState BgaDevelopBonus)
    241 -> pure (BgaSettleState BgaSettleProcess)
    242 -> pure (BgaSettleState BgaSettleRebelSneakAttack)
    311 -> pure (BgaDevelopState BgaAfterDevelopCheck)
    341 -> pure (BgaSettleState BgaSettleDiscard)
    342 -> pure (BgaSettleState BgaSettleImperiumSupplyConvoy)
    442 -> pure (BgaSettleState BgaSettleTerraformingProject)
    500 -> pure (BgaSetupState BgaInitialDiscardAncientRace)
    501 -> pure (BgaSetupState BgaInitialDiscardScavenger)
    542 -> pure (BgaSettleState BgaSettleTerraformingEngineers)
    _ -> Left ("unknown BGA game state id " <> showText raw)

bgaStateId :: BgaState -> StateId
bgaStateId state =
  case state of
    BgaSetupState BgaInitialDiscard -> StateId 2
    BgaSetupState BgaInitialDiscardHomeWorld -> StateId 3
    BgaSetupState BgaShowTableau -> StateId 4
    BgaSetupState BgaSetupFinished -> StateId 5
    BgaSetupState BgaExploreConsume -> StateId 19
    BgaSetupState BgaInitialDiscardAncientRace -> StateId 500
    BgaSetupState BgaInitialDiscardScavenger -> StateId 501
    BgaActionState BgaPhaseChoice -> StateId 10
    BgaActionState BgaPhaseChoiceSignal -> StateId 11
    BgaActionState BgaPhaseChoiceCrystal -> StateId 12
    BgaExploreState BgaExploreMain -> StateId 20
    BgaExploreState BgaPostExploreProcess -> StateId 21
    BgaDevelopState BgaDevelopMain -> StateId 30
    BgaDevelopState BgaDevelopProcess -> StateId 31
    BgaDevelopState BgaDevelopDiscard -> StateId 230
    BgaDevelopState BgaDevelopBonus -> StateId 231
    BgaDevelopState BgaAfterDevelopCheck -> StateId 311
    BgaSettleState BgaSettleMain -> StateId 40
    BgaSettleState BgaSettleDiscardToPutGood -> StateId 41
    BgaSettleState BgaSettleImprovedLogistics -> StateId 42
    BgaSettleState BgaSettleTakeoverCheck -> StateId 43
    BgaSettleState BgaSettleTakeoverPrevent -> StateId 49
    BgaSettleState BgaSettleTakeoverAttackerBoost -> StateId 44
    BgaSettleState BgaSettleTakeoverNextBoost -> StateId 48
    BgaSettleState BgaSettleTakeoverDefenderBoost -> StateId 45
    BgaSettleState BgaSettleTakeoverResolution -> StateId 46
    BgaSettleState BgaSettleProcess -> StateId 241
    BgaSettleState BgaSettleRebelSneakAttack -> StateId 242
    BgaSettleState BgaSettleDiscard -> StateId 341
    BgaSettleState BgaSettleImperiumSupplyConvoy -> StateId 342
    BgaSettleState BgaSettleTerraformingProject -> StateId 442
    BgaSettleState BgaSettleTerraformingEngineers -> StateId 542
    BgaConsumeState BgaConsumeSell -> StateId 50
    BgaConsumeState BgaConsumeMain -> StateId 51
    BgaConsumeState BgaConsumeProcess -> StateId 52
    BgaProduceState BgaProductionIntro -> StateId 60
    BgaProduceState BgaProductionWindfall -> StateId 61
    BgaProduceState BgaPostProductionProcess -> StateId 62
    BgaProduceState BgaProductionProcess -> StateId 69
    BgaDiscardState BgaEndRoundDiscard -> StateId 70
    BgaDiscardState BgaEndRound -> StateId 71
    BgaGameOverState BgaEndScore -> StateId 98
    BgaGameOverState BgaGameEnd -> StateId 99
    BgaGameOverState BgaDraftNewRound -> StateId 100
    BgaSearchState BgaSearchActionCheck -> StateId 200
    BgaSearchState BgaSearchAction -> StateId 201
    BgaSearchState BgaSearchActionChoose -> StateId 202

optionalBgaStateField :: Text -> Object -> Either Text (Maybe BgaState)
optionalBgaStateField label obj =
  case optionalField "id" obj of
    Nothing -> pure Nothing
    Just value -> Just <$> bgaStateValue label value

bgaStateValue :: Text -> Value -> Either Text BgaState
bgaStateValue label value = do
  raw <- intValue label value
  bgaStateFromId (StateId raw)

bgaStatePhase :: BgaState -> Maybe BgaPhase
bgaStatePhase state =
  case state of
    BgaSetupState _ -> Nothing
    BgaActionState _ -> Just BgaAction
    BgaExploreState _ -> Just BgaExplore
    BgaDevelopState _ -> Just BgaDevelop
    BgaSettleState _ -> Just BgaSettle
    BgaConsumeState _ -> Just BgaConsume
    BgaProduceState _ -> Just BgaProduce
    BgaDiscardState _ -> Just BgaDiscard
    BgaGameOverState _ -> Just BgaGameOver
    BgaSearchState _ -> Nothing

bgaStateHasPhase :: BgaPhase -> BgaState -> Bool
bgaStateHasPhase phase state =
  bgaStatePhase state == Just phase

bgaStateLeavesPhase :: BgaPhase -> BgaState -> Bool
bgaStateLeavesPhase phase state =
  case bgaStatePhase state of
    Just statePhase -> statePhase /= phase
    Nothing ->
      case state of
        BgaSetupState _ -> True
        BgaSearchState _ -> False
        _ -> False

bgaPhaseOrder :: BgaPhase -> Int
bgaPhaseOrder phase =
  case phase of
    BgaAction -> 0
    BgaExplore -> 1
    BgaDevelop -> 2
    BgaSettle -> 3
    BgaConsume -> 4
    BgaProduce -> 5
    BgaDiscard -> 6
    BgaGameOver -> 9

bgaStatePhaseOrder :: BgaState -> Maybe Int
bgaStatePhaseOrder state =
  bgaPhaseOrder <$> bgaStatePhase state

bgaStateIsNewActionRound :: BgaState -> Bool
bgaStateIsNewActionRound (BgaActionState BgaPhaseChoice) = True
bgaStateIsNewActionRound _ = False

bgaStateIsSelectLastAction :: BgaState -> Bool
bgaStateIsSelectLastAction (BgaActionState BgaPhaseChoiceCrystal) = True
bgaStateIsSelectLastAction _ = False

bgaStateIsExploreStart :: BgaState -> Bool
bgaStateIsExploreStart (BgaExploreState BgaExploreMain) = True
bgaStateIsExploreStart _ = False

bgaStateIsDevelopMain :: BgaState -> Bool
bgaStateIsDevelopMain (BgaDevelopState BgaDevelopMain) = True
bgaStateIsDevelopMain _ = False

bgaStateIsFinalGameOver :: BgaState -> Bool
bgaStateIsFinalGameOver (BgaGameOverState BgaDraftNewRound) = True
bgaStateIsFinalGameOver _ = False

bgaStateIsSearchStart :: BgaState -> Bool
bgaStateIsSearchStart (BgaSearchState BgaSearchAction) = True
bgaStateIsSearchStart _ = False

bgaStateIsSearchDone :: BgaState -> Bool
bgaStateIsSearchDone (BgaSearchState BgaSearchActionChoose) = True
bgaStateIsSearchDone _ = False

bgaStateIsSearch :: BgaState -> Bool
bgaStateIsSearch (BgaSearchState _) = True
bgaStateIsSearch _ = False

bgaStateIsSettleMain :: BgaState -> Bool
bgaStateIsSettleMain (BgaSettleState BgaSettleMain) = True
bgaStateIsSettleMain _ = False

bgaStateIsTerraformingEngineers :: BgaState -> Bool
bgaStateIsTerraformingEngineers (BgaSettleState BgaSettleTerraformingEngineers) = True
bgaStateIsTerraformingEngineers _ = False

bgaSettleSourceName :: BgaState -> Maybe Text
bgaSettleSourceName state =
  case state of
    BgaSettleState BgaSettleImprovedLogistics -> Just "Improved Logistics"
    BgaSettleState BgaSettleRebelSneakAttack -> Just "Rebel Sneak Attack"
    BgaSettleState BgaSettleImperiumSupplyConvoy -> Just "Imperium Supply Convoy"
    BgaSettleState BgaSettleTerraformingProject -> Just "Terraforming Project"
    _ -> Nothing

bgaStateClearsSettleSource :: BgaState -> Bool
bgaStateClearsSettleSource state =
  case state of
    BgaActionState _ -> True
    BgaExploreState _ -> True
    BgaDevelopState _ -> True
    BgaSettleState BgaSettleMain -> True
    BgaConsumeState _ -> True
    BgaProduceState _ -> True
    BgaDiscardState _ -> True
    BgaGameOverState _ -> True
    _ -> False

showText :: Show a => a -> Text
showText = Text.pack . show
