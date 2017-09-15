-- Copyright (c) 2017, Travis Bemann
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- o Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- o Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- o Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Robots.Genetic.HunterKiller.Types
  
  (RobotWorld (..),
   RobotCont (..),
   RobotMutate (..),
   RobotParams (..),
   RobotConstEntry (..),
   RobotState (..),
   Robot (..),
   Shot (..),
   RobotValue (..),
   RobotIntrinsicFunc,
   RobotExpr (..),
   RobotContext (..),
   RobotAction (..),
   RobotEvent (..),
   RobotInput (..),
   RobotCycleState (..),
   RobotConfigEntry (..),
   RobotConfigValue (..))
  
where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Control.Monad.State.Strict as State
import qualified System.Random as Random

-- | Robot continuity type
data RobotCont m =
  RobotCont { robotContParams :: !RobotParams,
              robotContRandom :: !Random.StdGen,
              robotContPrograms :: !(Seq.Seq RobotExpr),
              robotContSavedWorlds :: !(Seq.Seq RobotWorld),
              robotContEventHandler :: !(RobotEvent -> m RobotInput) }

-- | Robot world type
data RobotWorld =
  RobotWorld { robotWorldParams :: !RobotParams,
               robotWorldRobots :: !(Seq.Seq Robot),
               robotWorldShots :: !(Seq.Seq Shot),
               robotWorldCycles :: !Int,
               robotWorldKills :: !Int,
               robotWorldNextRobotIndex :: !Int,
               robotWorldRandom :: !Random.StdGen }

-- | Robot mutation state type
data RobotMutate =
  RobotMutate { robotMutateParams :: !RobotParams,
                robotMutateRandom :: !Random.StdGen }

-- | Robot parameters
data RobotParams =
  RobotParams { robotParamsLocationFriction :: !Double,
                robotParamsRotationFriction :: !Double,
                robotParamsFireFactor :: !Double,
                robotParamsThrustFactor :: !Double,
                robotParamsTurnFactor :: !Double,
                robotParamsShotSpeed :: !Double,
                robotParamsShotEnergyDecay :: !Double,
                robotParamsShotMinFireEnergy :: !Double,
                robotParamsShotMinEnergy :: !Double,
                robotParamsRobotRadius :: !Double,
                robotParamsMaxCycles :: !Int,
                robotParamsMaxKills :: !Int,
                robotParamsMaxDepth :: !Int,
                robotParamsMaxInstrCount :: !Int,
                robotParamsMinKills :: !Int,
                robotParamsMaxCodeDepth :: !Int,
                robotParamsSavedWorldCount :: !Int,
                robotParamsViewAngle :: !Double,
                robotParamsViewDistance :: !Double,
                robotParamsViewSortAngleFactor :: !Double,
                robotParamsViewSortDistanceFactor :: !Double,
                robotParamsGeneralEnergyGain :: !Double,
                robotParamsWeaponEnergyGain :: !Double,
                robotParamsHealthGain :: !Double,
                robotParamsShotHarmFactor :: !Double,
                robotParamsMinInitialGeneralEnergy :: !Double,
                robotParamsMaxInitialGeneralEnergy :: !Double,
                robotParamsMinInitialWeaponEnergy :: !Double,
                robotParamsMaxInitialWeaponEnergy :: !Double,
                robotParamsMinInitialHealth :: !Double,
                robotParamsMaxInitialHealth :: !Double,
                robotParamsMinInitialLocationDeltaAbs :: !Double,
                robotParamsMaxInitialLocationDeltaAbs :: !Double,
                robotParamsMinInitialRotationDeltaAbs :: !Double,
                robotParamsMaxInitialRotationDeltaAbs :: !Double,
                robotParamsKillScore :: !Double,
                robotParamsHitScoreFactor :: !Double,
                robotParamsDieScore :: !Double,
                robotParamsDamagedScoreFactor :: !Double,
                robotParamsThrustScoreFactor :: !Double,
                robotParamsTurnScoreFactor :: !Double,
                robotParamsMutationChance :: !Double,
                robotParamsMutationReplaceLeafChance :: !Double,
                robotParamsMutationReplaceNodeChance :: !Double,
                robotParamsMutationInsertCondChance :: !Double,
                robotParamsMutationInsertCondAsTrueChance :: !Double,
                robotParamsMutationInsertBindChance :: !Double,
                robotParamsRandomBoolWeight :: !Double,
                robotParamsRandomIntWeight :: !Double,
                robotParamsRandomFloatWeight :: !Double,
                robotParamsRandomVectorWeight :: !Double,
                robotParamsRandomIntrinsicWeight :: !Double,
                robotParamsRandomIntRange :: !(Int, Int),
                robotParamsRandomFloatRange :: !(Double, Double),
                robotParamsRandomVectorMaxLength :: !Int,
                robotParamsRandomValueMaxDepth :: !Int,
                robotParamsRandomBindMaxCount :: !Int,
                robotParamsRandomFuncMaxCount :: !Int,
                robotParamsRandomApplyMaxCount :: !Int,
                robotParamsRandomSimpleConstWeight :: !Double,
                robotParamsRandomSimpleSpecialConstWeight :: !Double,
                robotParamsRandomBindWeight :: !Double,
                robotParamsRandomFuncWeight :: !Double,
                robotParamsRandomApplyWeight :: !Double,
                robotParamsRandomCondWeight :: !Double,
                robotParamsRandomApplySpecialWeight :: !Double,
                robotParamsRandomMaxDepth :: !Int,
                robotParamsReproduction :: !(Seq.Seq Int),
                robotParamsSpecialConsts :: !(Seq.Seq RobotValue),
                robotParamsSpecialValueCount :: !Int }

-- | Intrinsic entry type
data RobotConstEntry = RobotConstEntry !RobotValue !Text.Text

-- | Robot state
data RobotState =
  RobotState { robotStateParams :: !RobotParams,
               robotStateDepth :: !Int,
               robotStateInstrCount :: !Int }

-- | Robot type
data Robot =
  Robot { robotIndex :: !Int,
          robotExpr :: !RobotExpr,
          robotData :: !RobotValue,
          robotLocation :: !(Double, Double),
          robotLocationDelta :: !(Double, Double),
          robotRotation :: !Double,
          robotRotationDelta :: !Double,
          robotGeneralEnergy :: !Double,
          robotWeaponEnergy :: !Double,
          robotHealth :: !Double,
          robotScore :: !Double }

-- | Shot type
data Shot =
  Shot { shotLocation :: !(Double, Double),
         shotLocationDelta :: !(Double, Double),
         shotEnergy :: !Double,
         shotRobotIndex :: !Int }

-- | Robot value type
data RobotValue = RobotNull
                | RobotBool !Bool
                | RobotInt !Integer
                | RobotFloat !Double
                | RobotVector !(Seq.Seq RobotValue)
                | RobotClosure !RobotContext !Int !RobotExpr
                | RobotIntrinsic !RobotIntrinsicFunc
                | RobotOutput !RobotValue !RobotAction

-- | Robot intrinsic function
type RobotIntrinsicFunc =
  Seq.Seq RobotValue -> State.State RobotState RobotValue

-- | Robot code type
data RobotExpr = RobotLoad !Int
               | RobotConst !RobotValue
               | RobotSpecialConst !Int
               | RobotBind !(Seq.Seq RobotExpr) !RobotExpr
               | RobotFunc !Int !RobotExpr
               | RobotApply !(Seq.Seq RobotExpr) !RobotExpr
               | RobotCond !RobotExpr !RobotExpr !RobotExpr

-- | Robot context type
newtype RobotContext = RobotContext (Seq.Seq RobotValue)

-- | Robot action
data RobotAction =
  RobotAction { robotActionFirePower :: !Double,
                robotActionThrustPower :: !Double,
                robotActionTurnPower :: !Double }

-- | Robot event
data RobotEvent = RobotNewRound !RobotWorld
                | RobotWorldCycle !RobotWorld
                | RobotRoundDone !RobotWorld

-- | Robot input
data RobotInput = RobotContinue
                | RobotExit

-- | Robot cycle state
data RobotCycleState = RobotNextCycle
                     | RobotEndRound

-- | Robot config entry
data RobotConfigEntry = RobotConfigEntry !Text.Text !RobotConfigValue

-- | Robot config value
data RobotConfigValue = RobotConfigNum !Double
                      | RobotConfigVector !(Seq.Seq RobotConfigValue)
