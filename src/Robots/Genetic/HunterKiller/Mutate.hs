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

module Robots.Genetic.HunterKiller.Mutate

  (mutate)

where

import Robots.Genetic.HunterKiller.Types
import Data.Sequence as Seq
import Data.Sequence ((><))
import Control.Monad.State.Strict as State
import System.Random as Random
import Data.Functor ((<$>))
import Control.Monad (mapM)

-- | Mutate an expression.
mutate :: Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutate contextDepth expr = do
  mutationChance <- robotParamsMutationChance <$> robotMutateParams <$> get
  mutateValue <- random
  expr <- mutateSub contextDepth expr
  if mutateValue <= mutationChance
    then mutateDirect contextDepth expr
    else expr

-- | Get a random value in [0, 1)
random :: Random.Random a => State.State RobotMutate a
random = do
  (value, gen) <- Random.random <$> robotMutateRandom <$> get
  state $ \mutateState -> mutateState { robotMutateRandom = gen }
  return value

-- | Get a random value in [a, b]
randomR :: Random.Random a => (a, a) -> State.State RandomMutate a
randomR range = do
  (value, gen) <- Random.randomR range <$> robotMutateRandom <$> get
  state $ \mutateState -> mutateState { robotMutateRandom = gen }
  return value

-- | Mutate a subexpression.
mutateSub :: Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateSub _ (RobotLoad _)@expr = return expr
mutateSub _ (RobotConst _)@expr = return expr
mutateSub _ (RobotSpecialConst _)@expr = return expr
mutateSub contextDepth (RobotBind boundExprs expr) = do
  boundExprs <- mapM $ \boundExpr ->
    mutate (contextDepth + Seq.length boundExprs) boundExpr
  expr <- mutate (contextDepth + Seq.length boundExprs)
  return $ RobotBind robotExprs expr
mutateSub argCount expr = do
  RobotFunc argCount <$> mutate (contextDepth + argCount) expr
mutateSub contextDepth (RobotApply argExprs funcExpr) = do
  argExprs <- mapM $ \argExpr -> mutate contextDepth argExpr
  funcExpr <- mutate contextDepth funcExpr
  return $ RobotApply argExrs funcExpr
mutateSub contextDepth (RobotCond condExpr trueExpr falseExpr) =
  condExpr <- mutate contextDepth condExpr
  trueExpr <- mutate contextDepth trueExpr
  falseExpr <- mutate contextDepth falseExpr
  return $ RobotCond condExpr trueExpr falseExpr

-- | Directly mutate an expression.
mutateDirect :: Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateDirect contextDepth expr = do
  case expr of
    RobotLoad _ | RobotConst _ | RobotSpecialContext _ ->
      mutateLeaf contextDepth expr
    _ -> mutateNode contextDepth expr

-- | Directly mutate a leaf expression.
mutateLeaf :: Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateLeaf contextDepth expr = do
  randomValue <- random
  replaceLeafChance <-
    robotParamsMutationReplaceLeafChance <$> robotMutateParams <$> get
  if randomValue <= replaceLeafChance
    then randomExpr contextDepth
    else
      case expr of
        RobotLoad _ -> RobotLoad <$> randomR (0, contextDepth - 1)
        RobotSpecialConst _ -> randomSpecialConst
        _ -> RobotConst <$> randomValue

-- | Directly mutate a node expression.
mutateNode :: Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateNode contextDepth expr = do
  randomValue <- random
  replaceNodeChance <-
    robotParamsMutationReplaceNodeChance <$> robotMutateParams <$> get
  if randomValue <= replaceNodeChance
    then randomExpr contextDepth
    else
      case expr of
        RobotBind boundExprs expr -> mutateBind contextDepth boundExprs expr
        RobotFunc argCount expr -> mutateFunc contextDepth argCount expr
        RobotApply argExprs expr -> mutateApply contextDepth argExprs expr
        _ -> expr

-- | Directly mutate a bind expression.
mutateBind :: Int -> Seq.Seq RobotExpr -> RobotExpr ->
              State.State RobotMutate RobotExpr
mutateBind contextDepth boundExprs expr = do
  bindMaxCount <- robotParamsRandomBindMaxCount <$> robotMutateParams <$> get
  bindCount <- randomR (0, bindMaxCount)
  let oldBindCount = Seq.length boundExprs
  boundExprs' <- mapM (adjustLoads oldBindCount (bindCount - oldBindCount))
                 (Seq.take bindCount boundExprs)
  expr' <- adjustLoads oldBindCount (bindCount - oldBindCount) expr
  if bindCount <= oldBindCount
    then return $ RobotBind boundExprs' expr'
    else do newBoundExprs <- replicateM (bindCount - oldBindCount)
                             (randomExpr (contextDepth + bindCount))
            return $ RobotBind (boundExprs' >< newBoundExprs) expr'

-- | Directly mutate a func expression.
mutateFunc :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateFunc contextDepth argCount expr = do
  argMaxCount <- robotParamsRandomFuncMaxCount <$> robotMutateParams <$> get
  newArgCount <- randomR (0, argMaxCount)
  RobotFunc newArgCount <$> adjustLoads argCount (newArgCount - argCount) expr

-- | Directly mutate an apply expression.
mutateApply :: Int -> Seq.Seq RobotExpr -> RobotExpr ->
               State.State RobotMutate RobotExpr
mutateApply contextDepth argExprs funcExpr = do
  applyMaxCount <- robotParamsRandomApplyMaxCount <$> robotMutatesParams <$> get
  applyCount <- randomR (0, applyMaxCount)
  let oldApplyCount = Seq.length argExprs
      argExprs' = Seq.take applyCount argExprs
  if applyCount <= oldApplyCount
    then return $ RobotApply argExprs' funcExpr
    else do newArgExprs <- replicateM (applyCount - oldApplyCount)
                           (randomExpr contextDepth)
            return $ RobotApply (argExprs' >< newArgExprs) expr

-- | Adjust load expressions.
adjustLoads :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
adjustLoads threshold delta (RobotLoad index)@expr = do
  if index < threshold
  then return expr
  else if (index >= threshold) && (delta < 0) && (index < threshold - delta)
    RobotLoad <$> randomR (0, contextDepth)
  else return . RobotLoad $ index + delta
adjustLoads _ _ (RobotConst _)@expr = return expr
adjustLoads _ _ (RobotSpecialConst _)@expr = return expr
adjustLoads threshold delta (RobotBind boundExprs expr) = do
  newBoundExprs <- mapM (\boundExpr ->
                          adjustLoads threshold delta boundExpr) boundExprs
  RobotBind newBoundExprs <$> adjustLoads threshold delta expr
adjustLoads contextDepth (RobotFunc argCount expr) = do
  RobotFunc argCount <$> adjustLoads threshold delta expr
adjustLoads contextDepth (RobotApply argExprs expr) = do
  newArgExprs <- mapM (adjustLoads threshold delta) argExprs
  RobotBind newArgExrs <$> adjustLoad threshold delta expr
adjustLoads contextDepth (RobotCond condExpr trueExpr falseExpr) = do
  newCondExpr <- adjustLoads threshold delta condExpr
  newTrueExpr <- adjustLoads threshold delta trueExpr
  newFalseExpr <- adjustLoads threshold delta falseExpr
  return $ RobotCond newCondExpr newTrueExpr newFalseExpr

-- | Generate a random expression.
randomExpr :: Int -> State.State RobotMutate RobotExpr
randomExpr contextDepth = randomExpr' contextDepth 0

-- | Actually generate a random expression.
randomExpr' :: Int -> Int -> State.State RobotMutate RobotExpr
randomExpr' contextDepth randomDepth = do
  maxDepth <- robotParamsRandomMaxDepth <$> robotMutateParams <$> get
  if randomDepth >= maxDepth
    then randomSimple contextDepth
    else do
      params <- robotMutateParams <$> get
      let bindWeight = robotParamsRandomBindWeight params
          funcWeight = robotParamsRandomFuncWeight params
          applyWeight = robotParamsRandomApplyWeight params
          condWeight = robotParamsRandomCondWeight params
      randomValue <- random
      if randomValue < bindWeight
        then randomBind contextDepth randomDepth
        else if randomValue < bindWeight + funcWeight
        then randomFunc contextDepth randomDepth
        else if randomValue < bindWeight + funcWeight + applyWeight
        then randomApply contextDepth randomDepth
        else if randomValue < bindWeight + funcWeight + applyWeight + condWeight
        then randomCond contextDepth randomDepth
        else randomSimple contextDepth

-- | Generate a random bind expression.
randomBind :: Int -> Int -> State.State RobotMutate RobotExpr
randomBind contextDepth randomDepth = do
  bindMaxCount <- robotParamsRandomBindMaxCount <$> robotMutateParams <$> get
  bindCount <- randomR (0, bindMaxCount)
  boundExprs <- Seq.replicateM bindCount
                (randomExpr' (contextDepth + bindCount) (randomDepth + 1))
  expr <- randomExpr' (contextDepth + bindCount) (randomDepth + 1)
  return $ RobotBind boundExprs expr
  
-- | Generate a random func expression.
randomFunc :: Int -> Int -> State.State RobotMutate RobotExpr
randomFunc contextDepth randomDepth = do
  argMaxCount <- robotParamsRandomFuncMaxCount <$> robotMutateParams <$> get
  argCount <- randomR (0, argMaxCount)
  expr <- randomExpr' (contextDepth + argCount) (randomDepth + 1)
  return $ RobotFunc argCount expr

-- | Generate a random apply expression.
randomApply :: Int -> Int -> State.State RobotMutate RobotExpr
randomApply contextDepth randomDepth = do
  params <- robotMutateParams <$> get
  let argMaxCount = robotParamsRandomFuncMaxCount params
      applySpecialWeight = robotParamsRandomApplySpecialWeight params
  argCount <- randomR (0, argMaxCount)
  argExprs <- Seq.replicateM argCount
              (randomExpr' contextDepth (randomDepth + 1))
  applySpecialValue <- random
  if applySpecialValue < applySpecialWeight
    then RobotApply argExprs <$> randomIntrinsic
    else do RobotApply argExprs <$> randomExpr' contextDepth (randomDepth + 1)

-- | Generate a random cond expression.
randomCond :: Int -> Int -> State.State RobotMutate RobotExpr
randomCond contextDepth randomDepth = do
  condExpr <- randomExpr' contextDepth (randomDepth + 1)
  trueExpr <- randomExpr' contextDepth (randomDepth + 1)
  falseExpr <- randomExpr' contextDepth (randomDepth + 1)
  return $ RobotCond condExpr trueExpr falseExpr

-- | Generate a simple expression.
randomSimple :: Int -> State.State RobotMutate RobotExpr
randomSimple contextDepth = do
  params <- robotMutateParams <$> get
  exprValue <- random
  let constWeight = robotParamsRandomSimpleConstWeight params
      specialConstWeight = robotParamsRandomSimpleSpecialConstWeight
  if exprValue < constWeight
    then RobotConst <$> randomValue
    else if exprValue < constWeight + specialConstValue
    then randomSpecialConst
    else RandomLoad <$> randomR (0, contextDepth - 1)
  
-- | Generate a random value.
randomValue :: State.State RobotMutate RobotValue
randomValue = do
  typeValue <- random
  params <- randomMutateParams <$> get
  let boolWeight = robotParamsRandomBoolWeight params
      intWeight = robotParamsRandomIntWeight params
      floatWeight = robotParamsRandomFloatWeight params
      vectorWeight = robotParamsRandomVectorWeight arams
  if typeValue < boolWeight
    then RobotBool <$> randomR (False, True)
    else if typeValue < boolWeight + intWeight
    then RobotInt <$> randomR $ robotParamsRandomIntRange params
    else if typeValue < boolWeight + intWeight + floatWeight
    then RobotFloat <$> randomR $ randomParamsRandomFloatRange params
    else if typeValue < boolWeight + intWeight + floatWeight + vectorWeight
    then do
      let maxLength <- robotParamsRandomVectorMaxLength params
      lengthValue <- randomR (0, maxLength)
      RobotVector <$> Seq.replicateM lengthValue randomValue
    else return RobotNull

-- | Generate a random special constant.
randomSpecialConst :: State.State RobotMutate RobotValue
randomSpecialConst = do
  randomValue <- random
  let weight = robotParamsRandomIntrinsicWeight params
  if randomValue < weight
    then randomIntrinsic      
    else RobotSpecialConst <$> randomR (0, robotParamsSpecialValueCount params)

-- | Generate a random intrinsic.
randomIntrinsic :: State.State RobotMutate RobotValue
randomIntrinsic = do
  params <- randomMutateParams <$> get
  let constsLength = Seq.length $ robotParamsSpecialConsts params
  RobotSpecialConst <$> randomR (robotParamsSpecialValueCount params,
                                  constsLength - 1)
