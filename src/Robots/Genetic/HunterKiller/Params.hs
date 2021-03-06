-- Copyright (c) 2017-2018, Travis Bemann
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

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Robots.Genetic.HunterKiller.Params

  (loadParams)

where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.Utility
import Robots.Genetic.HunterKiller.Intrinsics
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Combinator as AttoComb
import Data.Functor ((<$>))
import Data.Foldable (foldl')
import Control.Applicative ((<*>),
                            (*>),
                            (<*),
                            (<|>))
import Data.Char (isSpace)
import Text.Printf as Printf

-- | Default parameters
defaultParams :: RobotParams
defaultParams =
  RobotParams { robotParamsAutoSavePath = "default.world",
                robotParamsBackupSavePath = "backup.world",
                robotParamsMaxCyclesPerSecond = 100.0,
                robotParamsAutoSaveMostKills = True,
                robotParamsAutoSaveHighestScore = True,
                robotParamsAutoSaveHighestScoreIndividual = True,
                robotParamsAlwaysMoreKills = False,
                robotParamsAlwaysHigherScore = False,
                robotParamsOversizeRadius = 2.0,
                robotParamsAimRadius = 4.0,
                robotParamsLabelRadius = 5.0,
                robotParamsLabelAngle = pi / 4.0,
                robotParamsScoreRadius = 5.0,
                robotParamsScoreAngle = -(pi / 4.0),
                robotParamsKillsRadius = 5.0,
                robotParamsKillsAngle = pi * 0.75,
                robotParamsBaseHitTransferFactor = 0.0025,
                robotParamsEnergyHitTransferFactor = 0.005,
                robotParamsBaseRecoil = 0.0,
                robotParamsEnergyRecoilFactor = 0.01,
                robotParamsHitVelocityFactor = 0.01,
                robotParamsHitBaseRadius = 0.005,
                robotParamsHitFullRadius = 0.015,
                robotParamsHitDisplayCycles = 10,
                robotParamsLocationFriction = 0.03,
                robotParamsRotationFriction = 0.005,
                robotParamsFireFactor = 2.0,
                robotParamsThrustFactor = 0.005,
                robotParamsTurnFactor = pi / 5.0,
                robotParamsShotSpeed = 0.15,
                robotParamsShotEnergyDecay = 0.35,
                robotParamsShotMinFireEnergy = 0.5,
                robotParamsShotMinEnergy = 0.5,
                robotParamsRobotRadius = 0.005,
                robotParamsMaxCycles = 7500,
                robotParamsMaxKills = 200,
                robotParamsMaxDepth = 1000,
                robotParamsMinKills = 50,
                robotParamsMaxCodeDepth = 25,
                robotParamsSavedWorldCount = 10,
                robotParamsMaxRewind = 10000,
                robotParamsMaxInstrCount = 1000,
                robotParamsViewAngle = pi / 3.0,
                robotParamsViewDistance = 0.45,
                robotParamsViewSortAngleFactor = 0.4,
                robotParamsViewSortDistanceFactor = 0.6,
                robotParamsGeneralEnergyGain = 0.1,
                robotParamsWeaponEnergyGain = 0.0025,
                robotParamsHealthGain = 0.002,
                robotParamsShotHarmFactor = 1.0,
                robotParamsMinInitialGeneralEnergy = 0.5,
                robotParamsMaxInitialGeneralEnergy = 1.0,
                robotParamsMinInitialWeaponEnergy = 0.0,
                robotParamsMaxInitialWeaponEnergy = 0.2,
                robotParamsMinInitialHealth = 0.8,
                robotParamsMaxInitialHealth = 1.0,
                robotParamsMinInitialLocationDeltaAbs = 0.0,
                robotParamsMaxInitialLocationDeltaAbs = 0.005,
                robotParamsMinInitialRotationDeltaAbs = 0.0,
                robotParamsMaxInitialRotationDeltaAbs = pi / 50.0,
                robotParamsNoThrustPenaltyCycles = 100,
                robotParamsNoTurnPenaltyCycles = 100,
                robotParamsNoThrustPenaltyMinimum = 0.25,
                robotParamsNoTurnPenaltyMinimum = 0.5,
                robotParamsNoThrustPenaltyMaximum = 2.0,
                robotParamsNoTurnPenaltyMaximum = 2.0,
                robotParamsNoThrustPenaltyDecay = mkPolynomial [0.0, 0.95],
                robotParamsNoTurnPenaltyDecay = mkPolynomial [0.0, 0.95],
                robotParamsKillScore = 10.0,
                robotParamsHitScoreFactor = 10.0,
                robotParamsDieScore = -4.0,
                robotParamsDamagedScoreFactor = -3.0,
                robotParamsThrustScoreFactor = 0.01,
                robotParamsTurnScoreFactor = 0.01,
                robotParamsNoThrustPenaltyScore = -1.0,
                robotParamsNoTurnPenaltyScore = -1.5,
                robotParamsMutationChance =
                  mkPolynomial [0.025, 0.005],
                robotParamsMutationReplaceLeafChance =
                  mkPolynomial [0.05, 0.01],
                robotParamsMutationReplaceNodeChance =
                  mkPolynomial [0.025, 0.015],
                robotParamsMutationRandomLeafChance = mkPolynomial [0.1],
                robotParamsMutationModifyBoolChance = mkPolynomial [0.25],
                robotParamsMutationModifyIntFactor = mkPolynomial [2.0],
                robotParamsMutationModifyFloatFactor = mkPolynomial [0.5],
                robotParamsMutationModifyVectorFactor = mkPolynomial [2.0],
                robotParamsMutationInsertCondChance = mkPolynomial [0.05],
                robotParamsMutationInsertCondAsTrueChance = mkPolynomial [0.75],
                robotParamsMutationInsertBindChance = mkPolynomial [0.05],
                robotParamsMutationRemoveCondChance =
                  mkPolynomial [0.05, 0.01],
                robotParamsMutationRemoveCondAsTrueChance = mkPolynomial [0.75],
                robotParamsMutationFlipCondChance = mkPolynomial [0.5],
                robotParamsMutationRemoveApplyChance =
                  mkPolynomial [0.05, 0.01],
                robotParamsMutationFlipApplyChance = mkPolynomial [0.5],
                robotParamsMutationFlipBindChance = mkPolynomial [0.5],
                robotParamsRandomBoolWeight = 0.2,
                robotParamsRandomIntWeight = 0.2,
                robotParamsRandomFloatWeight = 0.4,
                robotParamsRandomVectorWeight = 0.2,
                robotParamsRandomIntrinsicWeight = 0.3,
                robotParamsRandomIntRange = (-10, 10),
                robotParamsRandomFloatRange = (-2.0, 2.0),
                robotParamsRandomVectorMaxLength = 5,
                robotParamsRandomValueMaxDepth = 2,
                robotParamsRandomBindMaxCount = 3,
                robotParamsRandomFuncMaxCount = 3,
                robotParamsRandomApplyMaxCount = 3,
                robotParamsRandomSimpleConstWeight = 0.15,
                robotParamsRandomSimpleSpecialConstWeight = 0.2,
                robotParamsRandomBindWeight = 0.15,
                robotParamsRandomFuncWeight = 0.1,
                robotParamsRandomApplyWeight = 0.2,
                robotParamsRandomCondWeight = 0.2,
                robotParamsRandomApplySpecialWeight = 0.75,
                robotParamsRandomMaxDepth = 3,
                robotParamsReproduction = [3, 2],
                robotParamsMutatedReproduction = [3, 2],
                robotParamsSpecialConsts = specialConsts,
                robotParamsSpecialValueCount = specialValueCount }

-- | Load parameters.
loadParams :: Text.Text -> Either Text.Text RobotParams
loadParams text =
  case Atto.parseOnly (parseConfig <* Atto.endOfInput) $ prepText text of
    Right params -> foldl' loadParam (Right defaultParams) params
    Left errorText -> Left $ Text.pack errorText

-- | Load a parameter.
loadParam :: Either Text.Text RobotParams -> RobotConfigEntry ->
             Either Text.Text RobotParams
loadParam (Right params) entry@(RobotConfigEntry name _) =
  if name ==  "maxCyclesPerSecond"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsMaxCyclesPerSecond = value }
  else if name == "autoSaveMostKills"
  then parseBool entry $
       \value -> params { robotParamsAutoSaveMostKills = value }
  else if name == "autoSaveHighestScore"
  then parseBool entry $
       \value -> params { robotParamsAutoSaveHighestScore = value }
  else if name == "autoSaveHighestScoreIndividual"
  then parseBool entry $
       \value -> params { robotParamsAutoSaveHighestScoreIndividual = value }
  else if name == "alwaysMoreKills"
  then parseBool entry $
       \value -> params { robotParamsAlwaysMoreKills = value }
  else if name == "alwaysHigherScore"
  then parseBool entry $
       \value -> params { robotParamsAlwaysHigherScore = value }
  else if name == "oversizeRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsOversizeRadius = value }
  else if name == "aimRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsAimRadius = value }
  else if name == "labelRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsLabelRadius = value }
  else if name == "labelAngle"
  then parseFloat entry $
       \value -> params { robotParamsLabelAngle = value }
  else if name == "scoreRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsScoreRadius = value }
  else if name == "scoreAngle"
  then parseFloat entry $
       \value -> params { robotParamsScoreAngle = value }
  else if name == "killsRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsKillsRadius = value }
  else if name == "killsAngle"
  then parseFloat entry $
       \value -> params { robotParamsKillsAngle = value }
  else if name == "baseHitTransferFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsBaseHitTransferFactor = value }
  else if name == "energyHitTransferFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsEnergyHitTransferFactor = value }
  else if name == "baseRecoil"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsBaseRecoil = value }
  else if name == "energyRecoilFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsEnergyRecoilFactor = value }
  else if name == "hitVelocityFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsHitVelocityFactor = value }
  else if name == "hitBaseRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsHitBaseRadius = value }
  else if name == "hitFullRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsHitFullRadius = value }
  else if name == "hitDisplayCycles"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsHitDisplayCycles = value }
  else if name == "locationFriction"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsLocationFriction = value }
  else if name == "rotationFriction"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsRotationFriction = value }
  else if name == "fireFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsFireFactor = value }
  else if name == "thrustFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsThrustFactor = value }
  else if name == "turnFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsTurnFactor = value }
  else if name == "shotSpeed"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsShotSpeed = value }
  else if name == "shotEnergyDecay"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsShotEnergyDecay = value }
  else if name == "shotMinFireEnergy"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsShotMinFireEnergy = value }
  else if name == "shotMinEnergy"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsShotMinEnergy = value }
  else if name == "robotRadius"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsRobotRadius = value }
  else if name == "maxCycles"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMaxCycles = value }
  else if name == "maxKills"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMaxKills = value }
  else if name == "maxDepth"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMaxDepth = value }
  else if name == "maxInstrCount"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMaxInstrCount = value }
  else if name == "minKills"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMinKills = value }
  else if name == "maxCodeDepth"
  then parseLoBoundInt 1 entry $
       \value -> params { robotParamsMaxCodeDepth = value }
  else if name == "savedWorldCount"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsSavedWorldCount = value }
  else if name == "maxRewind"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsMaxRewind = value }
  else if name == "viewAngle"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsViewAngle = value }
  else if name == "viewDistance"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsViewDistance = value }
  else if name == "viewSortAngleFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsViewSortAngleFactor = value }
  else if name == "viewSortDistanceFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsViewSortDistanceFactor = value }
  else if name == "generalEnergyGain"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsGeneralEnergyGain = value }
  else if name == "weaponEnergyGain"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsWeaponEnergyGain = value }
  else if name == "healthGain"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsHealthGain = value }
  else if name == "shotHarmFactor"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsShotHarmFactor = value }
  else if name == "minInitialGeneralEnergy"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMinInitialGeneralEnergy = value }
  else if name == "maxInitialGeneralEnergy"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMaxInitialGeneralEnergy = value }
  else if name == "minInitialWeaponEnergy"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMinInitialWeaponEnergy = value }
  else if name == "maxInitialWeaponEnergy"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMaxInitialWeaponEnergy = value }
  else if name == "minInitialHealth"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMinInitialHealth = value }
  else if name == "maxInitialHealth"
  then parseBoundFloat (0.0, 1.0) entry $
       \value -> params { robotParamsMaxInitialHealth = value }
  else if name == "minInitialLocationDeltaAbs"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsMinInitialLocationDeltaAbs = value }
  else if name == "maxInitialLocationDeltaAbs"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsMaxInitialLocationDeltaAbs = value }
  else if name == "minInitialRotationDeltaAbs"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsMinInitialRotationDeltaAbs = value }
  else if name == "maxInitialRotationDeltaAbs"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsMaxInitialRotationDeltaAbs = value }
  else if name == "noThrustPenaltyCycles"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsNoThrustPenaltyCycles = value }
  else if name == "noTurnPenaltyCycles"
  then parseLoBoundInt 0 entry $
       \value -> params { robotParamsNoTurnPenaltyCycles = value }
  else if name == "noThrustPenaltyMinimum"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsNoThrustPenaltyMinimum = value }
  else if name == "noTurnPenaltyMinimum"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsNoTurnPenaltyMinimum = value }
  else if name == "noThrustPenaltyMaximum"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsNoThrustPenaltyMaximum = value }
  else if name == "noTurnPenaltyMaximum"
  then parseLoBoundFloat 0.0 entry $
       \value -> params { robotParamsNoTurnPenaltyMaximum = value }
  else if name == "noThrustPenaltyDecay"
  then parsePolynomial entry $
       \value -> params { robotParamsNoThrustPenaltyDecay = value }
  else if name == "noTurnPenaltyDecay"
  then parsePolynomial entry $
       \value -> params { robotParamsNoTurnPenaltyDecay = value }
  else if name == "killScore"
  then parseFloat entry $
       \value -> params { robotParamsKillScore = value }
  else if name == "hitScoreFactor"
  then parseFloat entry $
       \value -> params { robotParamsHitScoreFactor = value }
  else if name == "dieScore"
  then parseFloat entry $
       \value -> params { robotParamsDieScore = value }
  else if name == "damagedScoreFactor"
  then parseFloat entry $
       \value -> params { robotParamsDamagedScoreFactor = value }
  else if name == "thrustScoreFactor"
  then parseFloat entry $
       \value -> params { robotParamsThrustScoreFactor = value }
  else if name == "turnScoreFactor"
  then parseFloat entry $
       \value -> params { robotParamsTurnScoreFactor = value }
  else if name == "noThrustPenaltyScore"
  then parseFloat entry $
       \value -> params { robotParamsNoThrustPenaltyScore = value }
  else if name == "noTurnPenaltyScore"
  then parseFloat entry $
       \value -> params { robotParamsNoTurnPenaltyScore = value }
  else if name == "mutationChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationChance = value }
  else if name == "mutationReplaceLeafChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationReplaceLeafChance = value }
  else if name == "mutationReplaceNodeChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationReplaceNodeChance = value }
  else if name == "mutationRandomLeafChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationRandomLeafChance = value }
  else if name == "mutationModifyBoolChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationModifyBoolChance = value }
  else if name == "mutationModifyIntFactor"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationReplaceLeafChance = value }
  else if name == "mutationModifyFloatFactor"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationReplaceLeafChance = value }
  else if name == "mutationModifyVectorFactor"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationReplaceLeafChance = value }
  else if name == "mutationInsertCondChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationInsertCondChance = value }
  else if name == "mutationInsertCondAsTrueChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationInsertCondAsTrueChance = value }
  else if name == "mutationInsertBindChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationInsertBindChance = value }
  else if name == "mutationRemoveCondChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationRemoveCondChance = value }
  else if name == "mutationRemoveCondAsTrueChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationRemoveCondAsTrueChance = value }
  else if name == "mutationFlipCondChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationFlipCondChance = value }
  else if name == "mutationRemoveApplyChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationRemoveApplyChance = value }
  else if name == "mutationFlipApplyChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationFlipApplyChance = value }
  else if name == "mutationFlipBindChance"
  then parsePolynomial entry $
       \value -> params { robotParamsMutationFlipBindChance = value }
  else if name == "randomBoolWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomBoolWeight = value }
  else if name == "randomIntWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomIntWeight = value }
  else if name == "randomFloatWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomFloatWeight = value }
  else if name == "randomVectorWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomVectorWeight = value }
  else if name == "randomIntrinsicWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomIntrinsicWeight = value }
  else if name == "randomMinInt"
  then parseInt entry
       (\value -> let (_, randomMaxInt) = robotParamsRandomIntRange params
                  in params { robotParamsRandomIntRange =
                               (value, randomMaxInt) })
  else if name == "randomMaxInt"
  then parseInt entry
       (\value -> let (randomMinInt, _) = robotParamsRandomIntRange params
                  in params { robotParamsRandomIntRange =
                                (randomMinInt, value) })
  else if name == "randomMinFloat"
  then parseFloat entry
       (\value -> let (_, randomMaxFloat) = robotParamsRandomFloatRange params
                  in params { robotParamsRandomFloatRange =
                                (value, randomMaxFloat) })
  else if name == "randomMaxFloat"
  then parseFloat entry
       (\value -> let (randomMinFloat, _) = robotParamsRandomFloatRange params
                  in params { robotParamsRandomFloatRange =
                                (randomMinFloat, value) })
  else if name == "randomVectorMaxLength"
  then parseInt entry $
       \value -> params { robotParamsRandomVectorMaxLength = value }
  else if name == "randomValueMaxDepth"
  then parseInt entry $
       \value -> params { robotParamsRandomValueMaxDepth = value }
  else if name == "randomBindMaxCount"
  then parseInt entry $
       \value -> params { robotParamsRandomBindMaxCount = value }
  else if name == "randomFuncMaxCount"
  then parseInt entry $
       \value -> params { robotParamsRandomFuncMaxCount = value }
  else if name == "randomApplyMaxCount"
  then parseInt entry $
       \value -> params { robotParamsRandomApplyMaxCount = value }
  else if name == "randomSimpleConstWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomSimpleConstWeight = value }
  else if name == "randomSimpleSpecialConstWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomSimpleSpecialConstWeight = value }
  else if name == "randomBindWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomBindWeight = value }
  else if name == "randomFuncWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomFuncWeight = value }
  else if name == "randomApplyWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomApplyWeight = value }
  else if name == "randomCondWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomCondWeight = value }
  else if name == "randomApplySpecialWeight"
  then parseFloat entry $
       \value -> params { robotParamsRandomApplySpecialWeight = value }
  else if name == "randomMaxDepth"
  then parseInt entry $
       \value -> params { robotParamsRandomMaxDepth = value }
  else if name == "reproduction"
  then parseReproduction entry $
       \value -> params { robotParamsReproduction = value }
  else if name == "mutatedReproduction"
  then parseReproduction entry $
       \value -> params { robotParamsMutatedReproduction = value }
  else Left . Text.pack $
       Printf.printf "%s: unknown configuration parameter" name
loadParam (Left errorMessage) _ = Left errorMessage

-- | Parse a boolean value.
parseBool :: RobotConfigEntry -> (Bool -> RobotParams) ->
             Either Text.Text RobotParams
parseBool (RobotConfigEntry _ (RobotConfigBool value)) func =
  Right $ func value
parseBool (RobotConfigEntry name _) _ =
  Left . Text.pack $ Printf.printf "%s: not a boolean" name

-- | Parse a floating-point value.
parseFloat :: RobotConfigEntry -> (Double -> RobotParams) ->
              Either Text.Text RobotParams
parseFloat (RobotConfigEntry _ (RobotConfigNum value)) func =
  Right $ func value
parseFloat (RobotConfigEntry name _) _ =
  Left . Text.pack $ Printf.printf "%s: not a number" name

-- | Parse a low-bounded floating-point value
parseLoBoundFloat :: Double -> RobotConfigEntry -> (Double -> RobotParams) ->
                     Either Text.Text RobotParams
parseLoBoundFloat lo (RobotConfigEntry name (RobotConfigNum value)) func =
  if value >= lo
  then Right $ func value
  else Left . Text.pack $
       Printf.printf "%s: not a number greater than or equal to %f" name lo
parseLoBoundFloat lo (RobotConfigEntry name _) _ =
  Left . Text.pack $
  Printf.printf "%s: not a number greater than or equal to %f" name lo

-- | Parse a bounded floating-point value
parseBoundFloat :: (Double, Double) -> RobotConfigEntry ->
                   (Double -> RobotParams) -> Either Text.Text RobotParams
parseBoundFloat (lo, hi) (RobotConfigEntry name (RobotConfigNum value)) func =
  if (value >= lo) && (value <= hi)
  then Right $ func value
  else Left . Text.pack $
       Printf.printf "%s: not a number between %f and %f" name lo hi
parseBoundFloat (lo, hi) (RobotConfigEntry name _) _ =
  Left . Text.pack $
  Printf.printf "%s: not a number between %f and %f" name lo hi

-- | Parse an integral value
parseInt :: RobotConfigEntry -> (Int -> RobotParams) ->
            Either Text.Text RobotParams
parseInt (RobotConfigEntry name (RobotConfigNum value)) func =
  let intValue = floor value
  in if fromIntegral intValue == value
     then Right $ func intValue
     else Left . Text.pack $ Printf.printf "%s: not an integer" name
parseInt (RobotConfigEntry name _) _ =
  Left . Text.pack $ Printf.printf "%s: not an integer name" name

-- | Parse a low-bounded integral value
parseLoBoundInt :: Int -> RobotConfigEntry -> (Int -> RobotParams) ->
                   Either Text.Text RobotParams
parseLoBoundInt lo (RobotConfigEntry name (RobotConfigNum value)) func =
  let intValue = floor value
  in if fromIntegral intValue == value
     then if intValue >= lo
          then Right $ func intValue
          else Left . Text.pack $
               Printf.printf "%s: not an integer greater than or equal to %d"
               name lo
     else Left . Text.pack $
          Printf.printf "%s: not an integer greater than or equal to %d" name lo
parseLoBoundInt lo (RobotConfigEntry name _) _ =
  Left . Text.pack $
  Printf.printf "%s: not an integer greater than or equal to %d" name lo

-- | Parse a bounded integral value
parseBoundInt :: (Int, Int) -> RobotConfigEntry -> (Int -> RobotParams) ->
                 Either Text.Text RobotParams
parseBoundInt (lo, hi) (RobotConfigEntry name (RobotConfigNum value)) func =
  let intValue = floor value
  in if fromIntegral intValue == value
     then if (intValue >= lo) && (intValue <= hi)
          then Right $ func intValue
          else Left . Text.pack $
               Printf.printf "%s: not an integer betweeen %d and %d" name lo hi
     else Left . Text.pack $
          Printf.printf "%s: not an integer between %d and %d" name lo hi
parseBoundInt (lo, hi) (RobotConfigEntry name _) _ =
  Left . Text.pack $
  Printf.printf "%s: not an integer between %d and %d" name lo hi

-- | Parse a reproduction value
parseReproduction :: RobotConfigEntry -> (Seq.Seq Int -> RobotParams) ->
                     Either Text.Text RobotParams
parseReproduction (RobotConfigEntry name (RobotConfigVector values)) func =
  if Seq.length values >= 1
  then
    case Seq.findIndexL isRobotConfigNumSmallerThanZero values of
      Just _ -> Left . Text.pack $ Printf.printf
                "%s: reproduction value smaller than zero" name
      Nothing ->
        case Seq.findIndexL isRobotConfigValueNotInteger
             values of
          Just _ -> Left . Text.pack $ Printf.printf
                    "%s: reproduction value not an integer" name
          Nothing ->
            let values' = fmap convertRobotConfigValueIntoInt values
            in Right $ func values'
  else Left . Text.pack $ Printf.printf "%s: does not have at least one entry"
                          name
  where isRobotConfigNumSmallerThanZero (RobotConfigNum value) = value < 1.0
        isRobotConfigNumSmallerThanZero _ = False
        isRobotConfigValueNotInteger (RobotConfigNum value) =
          (realToFrac (floor value)) /= value
        isRobotConfigValueNotInteger _ = True
        convertRobotConfigValueIntoInt (RobotConfigNum value) = floor value
        convertRobotConfigValueIntoInt _ = 0
parseReproduction (RobotConfigEntry name _) _ =
  Left .  Text.pack $ Printf.printf "%s: not a vector" name

-- | Parse a polynomial
parsePolynomial :: RobotConfigEntry -> (Polynomial -> RobotParams) ->
                   Either Text.Text RobotParams
parsePolynomial (RobotConfigEntry name (RobotConfigVector values)) func =
  if Seq.length values >= 1
  then
    case Seq.findIndexL isRobotConfigValueNotNumber values of
      Just _ -> Left . Text.pack $ Printf.printf
                "%s: coefficient not a number" name
      Nothing -> Right . func . Polynomial $
                 fmap convertRobotConfigValueIntoDouble values
  else Left . Text.pack $
       Printf.printf "%s: does not have at least one coefficient" name
  where isRobotConfigValueNotNumber (RobotConfigNum _) = True
        isRobotConfigValueNotNumber _ = False
        convertRobotConfigValueIntoDouble (RobotConfigNum value) = value
        convertRobotConfigValueIntoDouble _ = 0.0
parsePolynomial (RobotConfigEntry name (RobotConfigNum value)) func =
  Right . func $ mkPolynomial [value]
parsePolynomial (RobotConfigEntry name (RobotConfigBool _)) _ =
  Left . Text.pack $ Printf.printf "%s: not a polynomial" name

-- | Parse configuration.
parseConfig :: Atto.Parser (Seq.Seq RobotConfigEntry)
parseConfig =
  Atto.option Seq.empty
   ((<|) <$> parseConfigEntry
         <*> (Seq.fromList <$> (Atto.many'
                                (Atto.skipSpace *>
                                 parseConfigEntry)))) <*
  Atto.skipSpace
  
-- | Parse a configuration entry.
parseConfigEntry :: Atto.Parser RobotConfigEntry
parseConfigEntry =
  RobotConfigEntry <$> (Atto.skipSpace *>
                        Atto.takeWhile1 (\x -> not (isSpace x) && (x /= '=')) <*
                        Atto.skipSpace <*
                        "=")
                   <*> parseConfigValue

-- | Parse a configuration value.
parseConfigValue :: Atto.Parser RobotConfigValue
parseConfigValue =
  Atto.skipSpace *>
  Atto.choice [RobotConfigNum <$> Atto.double,
               RobotConfigBool <$> parseConfigBool,
               RobotConfigVector <$> parseConfigVector]

-- | Parse a configuration boolean.
parseConfigBool :: Atto.Parser Bool
parseConfigBool =
  Atto.skipSpace *>
  Atto.choice [const True <$> Atto.string "true",
               const False <$> Atto.string "false"] <*
  Atto.choice [const undefined <$> Atto.skipSpace,
               const undefined <$> Atto.endOfLine,
               const undefined <$> Atto.endOfInput,
               const undefined <$> AttoComb.lookAhead (Atto.char ',')]

-- | Parse a configuration vector.
parseConfigVector :: Atto.Parser (Seq.Seq RobotConfigValue)
parseConfigVector =
  Atto.skipSpace *>
  "[" *>
  Atto.option Seq.empty
   (((<|) <$> parseConfigValue
          <*> (Seq.fromList <$> (Atto.many'
                                 (Atto.skipSpace *>
                                  "," *>
                                  parseConfigValue))))) <*
  Atto.skipSpace <*
  "]" <*
  Atto.skipSpace
