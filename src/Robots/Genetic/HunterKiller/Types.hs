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
   RobotMutate (..),
   RobotParams (..),
   RobotConstEntry (..),
   RobotState (..),
   Robot (..),
   RobotVM (..),
   RobotValue (..),
   RobotIntrinsicFunc,
   RobotExpr (..),
   RobotContext (..),
   RobotAction (..),
   RobotEvent (..),
   RobotInput (..),
   RobotCycleState (..))
  
where

import Data.Sequence as Seq
import Control.Monad.State.Strict as State
import System.Random as Random

-- | Robot continuity type
data RobotCont m =
  RobotCont { robotContParams :: RobotParams,
              robotContRandom :: Random.StdGen,
              robotContPrograms :: Seq.Seq RobotExpr,
              robotCondEventHandler :: RobotEvent -> m RobotInput }
  deriving (Eq)

-- | Robot world type
data RobotWorld =
  RobotWorld { robotWorldParams :: RobotParams,
               robotWorldRobots :: Seq.Seq Robot,
               robotWorldShots :: Seq.Seq Shot,
               robotWorldCycles :: Int,
               robotWorldKills :: Int,
               robotWorldNextRobotIndex :: Int,
               robotWorldRandom :: Random.StdGen }
  deriving (Eq)

-- | Robot mutation state type
data RobotMutate =
  RobotMutate { robotMutateParams :: RobotParams,
                robotMutateRandom :: Random.StdGen }
  deriving (Eq)

-- | Robot parameters
data RobotParams =
  RobotParams { robotParamsLocationFriction :: Double,
                robotParamsRotationFriction :: Double,
                robotParamsFireFactor :: Doublle,
                robotParamsThrustFactor :: Double,
                robotParamsTurnFactor :: Double,
                robotParamsShotSpeed :: Double,
                robotParamsShotEnergyDecay :: Double,
                robotParamsShotMinEnergy :: Double,
                robotParamsRobotRadius :: Double,
                robotParamsMaxCycles :: Int,
                robotParamsMaxKills :: Int,
                robotParamsMaxDepth :: Int,
                robotParamsMaxInstrCount :: Int,
                robotParamsViewAngle :: Double,
                robotParamsViewDistance :: Double,
                robotParamsViewSortAngleFactor :: Double,
                robotParamsViewSortDistanceFactor :: Double,
                robotParamsGeneralEnergyGain :: Double,
                robotParamsWeaponEnergyGain :: Double,
                robotParamsHealthGain :: Double,
                robotParamsShotHarmFactor :: Double,
                robotParamsMinInitialGeneralEnergy :: Double,
                robotParamsMaxInitialGeneralEnergy :: Double,
                robotParamsMinInitialWeaponEnergy :: Double,
                robotParamsMaxInitialWeaponEnergy :: Dobule,
                robotParamsMinInitialHealth :: Double,
                robotParamsMaxInitialHealth :: Double,
                robotParamsMinInitialLocationDeltaAbs :: Double,
                robotParamsMaxInitialLocationDeltaAbs :: Double,
                robotParamsMinInitialRotationDeltaAbs :: Double,
                robotParamsMaxInitialRotationDeltaAbs :: Double,
                robotParamsMutationChance :: Double,
                robotParamsMutationReplaceLeafChance :: Double,
                robotParamsMutationReplaceNodeChance :: Double,
                robotParamsRandomBoolWeight :: Double,
                robotParamsRandomIntWeight :: Double,
                robotParamsRandomFloatWeight :: Double,
                robotParamsRandomVectorWeight :: Double,
                robotParamsRandomInstrinsicWeight :: Double,
                robotParamsRandomIntRange :: (Int, Int),
                robotParamsRandomFloatRange :: (Double, Double),
                robotParamsRandomVectorMaxLength :: Int,
                robotParamsRandomBindMaxCount :: Int,
                robotParamsRandomFuncMaxCount :: Int,
                robotParamsRandomApplyMaxCount :: Int,
                robotParamsRandomSimpleConstWeight :: Double,
                robotParamsRandomSimpleSpecialConstWeight :: Double,
                robotParamsRandomBindWeight :: Double,
                robotParamsRandomFuncWeight :: Double,
                robotParamsRandomApplyWeight :: Double,
                robotParamsRandomCondWeight :: Double,
                robotParamsRandomApplySpecialWeight :: Double,
                robotParamsRandomMaxDepth :: Int,
                robotParamsReproduction :: Seq.Seq Int,
                robotParamsSpecialConsts :: Seq.Seq RobotValue,
                robotParamsSpecialValueCount :: Int }
  deriving (Eq)

-- | Intrinsic entry type
data RobotConstEntry = RobotConstEntry RobotValue Text.Text
                     deriving (Eq)

-- | Robot state
data RobotState =
  RobotState { robotStateParams :: RobotParams,
               robotStateDepth :: Int,
               robotStateInstrCount :: Int }
  deriving (Eq)

-- | Robot type
data Robot =
  Robot { robotIndex :: Int,
          robotExpr :: RobotExpr,
          robotData :: RobotValue,
          robotLocation :: (Double, Double),
          robotLocationDelta :: (Double, Double),
          robotRotation :: Double,
          robotRotationDelta :: Double,
          robotGeneralEnergy :: Double,
          robotWeaponEnergy :: Double,
          robotHealth :: Double,
          robotScore :: Int }
  deriving (Eq)

-- | Shot type
data Shot =
  Shot { shotLocation :: (Double, Double),
         shotLocationDelta :: (Double, Double),
         shotEnergy :: Double,
         shotRobotIndex :: Int }
  deriving (Eq)

-- | Robot value type
data RobotValue = RobotNull
                | RobotBool Bool
                | RobotInt Integer
                | RobotFloat Double
                | RobotVector (Seq.Seq RobotValue)
                | RobotClosure RobotContext Int RobotExpr
                | RobotIntrinsic RobotIntrinsicFunc
                | RobotOutput RobotValue RobotAction
                deriving (Eq)

-- | Robot intrinsic function
type RobotIntrinsicFunc =
  Seq.Seq RobotValue -> State.State RobotState RobotValue

-- | Robot code type
data RobotExpr = RobotLoad Int
               | RobotConst RobotValue
               | RobotSpecialConst Int
               | RobotBind (Seq.Seq RobotExpr) RobotExpr
               | RobotFunc Int RobotExpr
               | RobotApply (Seq.Seq RobotExpr) RobotExpr
               | RobotCond RobotExpr RobotExpr RobotExpr
               deriving (Eq)

-- | Robot context type
newtype RobotContext = RobotContext (Seq.Seq Value)
  deriving (Eq)

-- | Robot action
data RobotAction =
  RobotAction { robotActionFirePower :: Double,
                robotActionThrustPower :: Double,
                robotActionTurnPower :: Double }
  deriving (Eq)

-- | Robot event
data RobotEvent = RobotNewRound RobotWorld
                | RobotWorldCycle RobotWorld
                | RobotRoundDone RobotWorld
                deriving (Eq)

-- | Robot input
data RobotInput = RobotContinue
                | RobotExit
                deriving (Eq)

-- | Robot cycle state
data RobotCycleState = RobotNextCycle
                     | RobotEndRound
                     deriving (Eq)
