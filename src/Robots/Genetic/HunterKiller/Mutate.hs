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

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Robots.Genetic.HunterKiller.Mutate

  (mutate)

where

import Robots.Genetic.HunterKiller.Types
import qualified Data.Sequence as Seq
import Data.Sequence ((><))
import qualified Control.Monad.State.Strict as State
import qualified System.Random as Random
import Data.Functor ((<$>))
import Control.Monad (mapM)
import Data.Foldable (foldl')

-- | Mutate an expression.
mutate :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutate contextDepth totalDepth expr = do
  mutationChance <- robotParamsMutationChance <$> robotMutateParams <$> State.get
  mutateValue <- random
  expr <- mutateSub contextDepth totalDepth expr
  if mutateValue <= mutationChance
    then mutateDirect contextDepth totalDepth expr
    else return expr

-- | Get a random value in [0, 1)
random :: Random.Random a => State.State RobotMutate a
random = do
  (value, gen) <- Random.random <$> robotMutateRandom <$> State.get
  State.modify $ \mutateState -> mutateState { robotMutateRandom = gen }
  return value

-- | Get a random value in [a, b]
randomR :: Random.Random a => (a, a) -> State.State RobotMutate a
randomR range = do
  (value, gen) <- Random.randomR range <$> robotMutateRandom <$> State.get
  State.modify $ \mutateState -> mutateState { robotMutateRandom = gen }
  return value

-- | Mutate a subexpression.
mutateSub :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateSub _ _ expr@(RobotLoad _) = return expr
mutateSub _ _ expr@(RobotConst _) = return expr
mutateSub _ _ expr@(RobotSpecialConst _) = return expr
mutateSub contextDepth totalDepth (RobotBind boundExprs expr) = do
  boundExprs <- mapM (\boundExpr ->
    mutate (contextDepth + Seq.length boundExprs) (totalDepth + 1) boundExpr)
    boundExprs
  expr <- mutate (contextDepth + Seq.length boundExprs) (totalDepth + 1) expr
  return $ RobotBind boundExprs expr
mutateSub contextDepth totalDepth (RobotFunc argCount expr) = do
  RobotFunc argCount <$> mutate (contextDepth + argCount) (totalDepth + 1) expr
mutateSub contextDepth totalDepth (RobotApply argExprs funcExpr) = do
  argExprs <- mapM (\argExpr -> mutate contextDepth (totalDepth + 1) argExpr)
    argExprs
  funcExpr <- mutate contextDepth (totalDepth + 1) funcExpr
  return $ RobotApply argExprs funcExpr
mutateSub contextDepth totalDepth (RobotCond condExpr trueExpr falseExpr) = do
  condExpr <- mutate contextDepth (totalDepth + 1) condExpr
  trueExpr <- mutate contextDepth (totalDepth + 1) trueExpr
  falseExpr <- mutate contextDepth (totalDepth + 1) falseExpr
  return $ RobotCond condExpr trueExpr falseExpr

-- | Directly mutate an expression.
mutateDirect :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateDirect contextDepth totalDepth expr = do
  probability <- random
  insertCondChance <-
    robotParamsMutationInsertCondChance <$> robotMutateParams <$> State.get
  if probability <= insertCondChance
    then insertCond contextDepth totalDepth expr
    else do
      probability <- random
      insertBindChance <-
        robotParamsMutationInsertBindChance <$> robotMutateParams <$> State.get
      if probability <= insertBindChance
        then insertBind contextDepth totalDepth expr
        else case expr of
               RobotLoad _ -> mutateLeaf contextDepth totalDepth expr
               RobotConst _ -> mutateLeaf contextDepth totalDepth expr
               RobotSpecialConst _ -> mutateLeaf contextDepth totalDepth expr
               _ -> mutateNode contextDepth totalDepth expr

-- | Directly mutate a leaf expression.
mutateLeaf :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateLeaf contextDepth totalDepth  expr = do
  probability <- random
  replaceLeafChance <-
    robotParamsMutationReplaceLeafChance <$> robotMutateParams <$> State.get
  if probability <= replaceLeafChance
    then randomExpr contextDepth totalDepth
    else
      case expr of
        RobotLoad _ -> RobotLoad <$> randomR (0, contextDepth - 1)
        RobotSpecialConst _ -> randomSpecialConst
        _ -> RobotConst <$> randomValue (totalDepth + 1)

-- | Directly mutate a node expression.
mutateNode :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
mutateNode contextDepth totalDepth expr = do
  probability <- random
  replaceNodeChance <-
    robotParamsMutationReplaceNodeChance <$> robotMutateParams <$> State.get
  if probability <= replaceNodeChance
    then randomExpr contextDepth totalDepth
    else
      case expr of
        RobotBind boundExprs expr ->
          mutateBind contextDepth totalDepth boundExprs expr
        RobotFunc argCount expr ->
          mutateFunc contextDepth totalDepth argCount expr
        RobotApply argExprs expr ->
          mutateApply contextDepth totalDepth argExprs expr
        RobotCond condExpr trueExpr falseExpr ->
          mutateCond contextDepth totalDepth condExpr trueExpr falseExpr

-- | Directly mutate a bind expression.
mutateBind :: Int -> Int -> Seq.Seq RobotExpr -> RobotExpr ->
              State.State RobotMutate RobotExpr
mutateBind contextDepth totalDepth boundExprs expr = do
  flipBindChance <- robotParamsMutationFlipBindChance <$>
                    robotMutateParams <$> State.get
  probability <- random
  let bindCount = Seq.length boundExprs
  if probability <= flipBindChance
    then
      if bindCount > 0
      then do
        index0 <- randomR (0, bindCount - 1)
        index1 <- randomR (0, bindCount - 1)
        case Seq.lookup index0 boundExprs of
          Just boundExpr0 ->
            case Seq.lookup index1 boundExprs of
              Just boundExpr1 ->
                let boundExprs' = Seq.update index0 boundExpr1 boundExprs
                    boundExprs'' = Seq.update index1 boundExpr0 boundExprs'
                in return $ RobotBind boundExprs'' expr
              Nothing -> error "impossible"
          Nothing -> error "impossible"
      else return $ RobotBind boundExprs expr
    else do
      bindMaxCount <- robotParamsRandomBindMaxCount <$> robotMutateParams <$> State.get
      newBindCount <- randomR (0, bindMaxCount)
      boundExprs' <- mapM (adjustLoads bindCount (newBindCount - bindCount))
                     (Seq.take newBindCount boundExprs)
      expr' <- adjustLoads bindCount (newBindCount - bindCount) expr
      if newBindCount <= bindCount
        then return $ RobotBind boundExprs' expr'
        else do newBoundExprs <- Seq.replicateM (newBindCount - bindCount)
                                 (randomExpr (contextDepth + newBindCount)
                                  (totalDepth + 1))
                return $ RobotBind (boundExprs' >< newBoundExprs) expr'

-- | Directly mutate a func expression.
mutateFunc :: Int -> Int -> Int -> RobotExpr ->
              State.State RobotMutate RobotExpr
mutateFunc contextDepth totalDepth argCount expr = do
  argMaxCount <- robotParamsRandomFuncMaxCount <$> robotMutateParams <$> State.get
  newArgCount <- randomR (0, argMaxCount)
  RobotFunc newArgCount <$> adjustLoads argCount (newArgCount - argCount) expr

-- | Directly mutate an apply expression.
mutateApply :: Int -> Int -> Seq.Seq RobotExpr -> RobotExpr ->
               State.State RobotMutate RobotExpr
mutateApply contextDepth totalDepth argExprs funcExpr = do
  removeApplyChance <- robotParamsMutationRemoveApplyChance <$>
                       robotMutateParams <$> State.get
  flipApplyChance <- robotParamsMutationFlipApplyChance <$>
                     robotMutateParams <$> State.get
  probability <- random
  let applyCount = Seq.length argExprs
  if probability <= removeApplyChance
    then
      if applyCount > 0
      then do
        selectedArgIndex <- randomR (0, applyCount - 1)
        case Seq.lookup selectedArgIndex argExprs of
          Just argExpr -> return argExpr
          Nothing -> error "impossible"
      else return $ RobotConst RobotNull
    else if probability <= removeApplyChance + flipApplyChance
    then
      if applyCount > 0
      then do
        index0 <- randomR (0, applyCount - 1)
        index1 <- randomR (0, applyCount - 1)
        case Seq.lookup index0 argExprs of
          Just argExpr0 ->
            case Seq.lookup index1 argExprs of
              Just argExpr1 ->
                let argExprs' = Seq.update index0 argExpr1 argExprs
                    argExprs'' = Seq.update index1 argExpr0 argExprs'
                in return $ RobotApply argExprs'' funcExpr
              Nothing -> error "impossible"
          Nothing -> error "impossible"
      else return $ RobotApply argExprs funcExpr
    else do
      applyMaxCount <- robotParamsRandomApplyMaxCount <$> robotMutateParams <$> State.get
      newApplyCount <- randomR (0, applyMaxCount)
      let argExprs' = Seq.take newApplyCount argExprs
      if newApplyCount <= applyCount
        then return $ RobotApply argExprs' funcExpr
        else do newArgExprs <- Seq.replicateM (newApplyCount - applyCount)
                               (randomExpr contextDepth (totalDepth + 1))
                return $ RobotApply (argExprs' >< newArgExprs) funcExpr

-- | Directly mutate a cond expression.
mutateCond :: Int -> Int -> RobotExpr -> RobotExpr -> RobotExpr ->
              State.State RobotMutate RobotExpr
mutateCond contextDepth totalDepth condExpr trueExpr falseExpr = do
  removeCondChance <- robotParamsMutationRemoveCondChance <$>
                      robotMutateParams <$> State.get
  removeCondAsTrueChance <- robotParamsMutationRemoveCondAsTrueChance <$>
                            robotMutateParams <$> State.get
  flipCondChance <- robotParamsMutationFlipCondChance <$>
                    robotMutateParams <$> State.get
  probability <- random
  if probability <= removeCondChance
    then do
      probability <- random
      if probability <= removeCondAsTrueChance
        then return trueExpr
        else return falseExpr
    else if probability <= removeCondChance + flipCondChance
    then return $ RobotCond condExpr falseExpr trueExpr
    else return $ RobotCond condExpr trueExpr falseExpr

-- | Adjust load expressions.
adjustLoads :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
adjustLoads threshold delta expr@(RobotLoad index) = do
  if index < threshold
  then return expr
  else if (index >= threshold) && (delta < 0) && (index < threshold - delta)
  then RobotLoad <$> randomR (0, threshold - 1)
  else return . RobotLoad $ index + delta
adjustLoads _ _ expr@(RobotConst _) = return expr
adjustLoads _ _ expr@(RobotSpecialConst _) = return expr
adjustLoads threshold delta (RobotBind boundExprs expr) = do
  newBoundExprs <- mapM (\boundExpr ->
                          adjustLoads threshold delta boundExpr) boundExprs
  RobotBind newBoundExprs <$> adjustLoads threshold delta expr
adjustLoads threshold delta (RobotFunc argCount expr) = do
  RobotFunc argCount <$> adjustLoads threshold delta expr
adjustLoads threshold delta (RobotApply argExprs expr) = do
  newArgExprs <- mapM (adjustLoads threshold delta) argExprs
  RobotBind newArgExprs <$> adjustLoads threshold delta expr
adjustLoads threshold delta (RobotCond condExpr trueExpr falseExpr) = do
  newCondExpr <- adjustLoads threshold delta condExpr
  newTrueExpr <- adjustLoads threshold delta trueExpr
  newFalseExpr <- adjustLoads threshold delta falseExpr
  return $ RobotCond newCondExpr newTrueExpr newFalseExpr

-- | Generate a random expression.
randomExpr :: Int -> Int -> State.State RobotMutate RobotExpr
randomExpr contextDepth totalDepth =
  randomExpr' contextDepth 0 totalDepth

-- | Actually generate a random expression.
randomExpr' :: Int -> Int -> Int -> State.State RobotMutate RobotExpr
randomExpr' contextDepth randomDepth totalDepth = do
  maxDepth <- robotParamsRandomMaxDepth <$> robotMutateParams <$> State.get
  maxCodeDepth <- robotParamsMaxCodeDepth <$> robotMutateParams <$> State.get
  if randomDepth >= maxDepth
    then randomSimple contextDepth totalDepth
    else do
      params <- robotMutateParams <$> State.get
      let bindWeight = robotParamsRandomBindWeight params
          funcWeight = robotParamsRandomFuncWeight params
          applyWeight = robotParamsRandomApplyWeight params
          condWeight = robotParamsRandomCondWeight params
      if totalDepth < maxCodeDepth - 2
        then do
          probability <- random
          if probability < bindWeight
            then randomBind contextDepth (randomDepth + 1) totalDepth
            else if probability < bindWeight + funcWeight
            then randomFunc contextDepth (randomDepth + 1) totalDepth
            else if probability < bindWeight + funcWeight + applyWeight
            then randomApply contextDepth (randomDepth + 1) totalDepth
            else if probability <
                    bindWeight + funcWeight + applyWeight + condWeight
            then randomCond contextDepth (randomDepth + 1) totalDepth
            else randomSimple contextDepth totalDepth
        else randomSimple contextDepth totalDepth

-- | Generate a random bind expression.
randomBind :: Int -> Int -> Int -> State.State RobotMutate RobotExpr
randomBind contextDepth randomDepth totalDepth = do
  bindMaxCount <-
    robotParamsRandomBindMaxCount <$> robotMutateParams <$> State.get
  bindCount <- randomR (0, bindMaxCount)
  boundExprs <- Seq.replicateM bindCount
                (randomExpr' (contextDepth + bindCount) (randomDepth + 1)
                 (totalDepth + 1))
  expr <- randomExpr' (contextDepth + bindCount) (randomDepth + 1)
          (totalDepth + 1)
  return $ RobotBind boundExprs expr
  
-- | Generate a random func expression.
randomFunc :: Int -> Int -> Int -> State.State RobotMutate RobotExpr
randomFunc contextDepth randomDepth totalDepth = do
  argMaxCount <- robotParamsRandomFuncMaxCount <$> robotMutateParams <$> State.get
  argCount <- randomR (0, argMaxCount)
  expr <- randomExpr' (contextDepth + argCount) (randomDepth + 1)
          (totalDepth + 1)
  return $ RobotFunc argCount expr

-- | Generate a random apply expression.
randomApply :: Int -> Int -> Int -> State.State RobotMutate RobotExpr
randomApply contextDepth randomDepth totalDepth = do
  params <- robotMutateParams <$> State.get
  let argMaxCount = robotParamsRandomFuncMaxCount params
      applySpecialWeight = robotParamsRandomApplySpecialWeight params
  argCount <- randomR (0, argMaxCount)
  argExprs <- Seq.replicateM argCount
              (randomExpr' contextDepth (randomDepth + 1) (totalDepth + 1))
  applySpecialValue <- random
  if applySpecialValue <= applySpecialWeight
    then RobotApply argExprs <$> randomIntrinsic
    else RobotApply argExprs <$>
         randomExpr' contextDepth (randomDepth + 1) (totalDepth + 1)

-- | Generate a random cond expression.
randomCond :: Int -> Int -> Int -> State.State RobotMutate RobotExpr
randomCond contextDepth randomDepth totalDepth = do
  condExpr <- randomExpr' contextDepth (randomDepth + 1) (totalDepth + 1)
  trueExpr <- randomExpr' contextDepth (randomDepth + 1) (totalDepth + 1)
  falseExpr <- randomExpr' contextDepth (randomDepth + 1) (totalDepth + 1)
  return $ RobotCond condExpr trueExpr falseExpr

-- | Generate a simple expression.
randomSimple :: Int -> Int -> State.State RobotMutate RobotExpr
randomSimple contextDepth totalDepth = do
  params <- robotMutateParams <$> State.get
  exprValue <- random
  let constWeight = robotParamsRandomSimpleConstWeight params
      specialConstWeight = robotParamsRandomSimpleSpecialConstWeight params
      maxCodeDepth = robotParamsMaxCodeDepth params
  if totalDepth < maxCodeDepth - 1
    then do if exprValue < constWeight
              then RobotConst <$> randomValue (totalDepth + 1)
              else if exprValue < constWeight + specialConstWeight
              then randomSpecialConst
              else RobotLoad <$> randomR (0, contextDepth - 1)
    else if exprValue < specialConstWeight
    then randomSpecialConst
    else RobotLoad <$> randomR (0, contextDepth - 1)
  
-- | Generate a random value.
randomValue :: Int -> State.State RobotMutate RobotValue
randomValue totalDepth = do
  maxCodeDepth <-
    robotParamsMaxCodeDepth . robotMutateParams <$> State.get
  valueMaxDepth <-
    robotParamsRandomValueMaxDepth . robotMutateParams <$> State.get
  if (maxCodeDepth - totalDepth) > valueMaxDepth
    then randomValue' $ valueMaxDepth - 1
    else randomValue' $ (maxCodeDepth - totalDepth) - 1

-- | Actually generate a random value.
randomValue' :: Int -> State.State RobotMutate RobotValue
randomValue' counter = do
  typeValue <- random
  params <- robotMutateParams <$> State.get
  let boolWeight = robotParamsRandomBoolWeight params
      intWeight = robotParamsRandomIntWeight params
      floatWeight = robotParamsRandomFloatWeight params
      vectorWeight = robotParamsRandomVectorWeight params
  if typeValue < boolWeight
    then RobotBool <$> randomR (False, True)
    else if typeValue < boolWeight + intWeight
    then let (minInt, maxInt) = robotParamsRandomIntRange params
         in RobotInt <$> (randomR (fromIntegral minInt, fromIntegral maxInt))
    else if typeValue < boolWeight + intWeight + floatWeight
    then RobotFloat <$> (randomR $ robotParamsRandomFloatRange params)
    else if (typeValue < boolWeight + intWeight + floatWeight + vectorWeight) &&
            (counter > 0)
    then do
      let maxLength = robotParamsRandomVectorMaxLength params
      lengthValue <- randomR (0, maxLength)
      RobotVector <$> Seq.replicateM lengthValue (randomValue' (counter - 1))
    else return RobotNull

-- | Generate a random special constant.
randomSpecialConst :: State.State RobotMutate RobotExpr
randomSpecialConst = do
  probability <- random
  params <- robotMutateParams <$> State.get
  let weight = robotParamsRandomIntrinsicWeight params
  if probability < weight
    then randomIntrinsic      
    else RobotSpecialConst <$> randomR (0, robotParamsSpecialValueCount params)

-- | Generate a random intrinsic.
randomIntrinsic :: State.State RobotMutate RobotExpr
randomIntrinsic = do
  params <- robotMutateParams <$> State.get
  let constsLength = Seq.length $ robotParamsSpecialConsts params
  RobotSpecialConst <$> randomR (robotParamsSpecialValueCount params,
                                  constsLength - 1)

-- | Insert a cond instruction.
insertCond :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
insertCond contextDepth totalDepth expr = do
  maxCodeDepth <- robotParamsMaxCodeDepth <$> robotMutateParams <$> State.get
  if totalDepth + codeDepth expr < maxCodeDepth
    then do
      probability <- random
      insertCondAsTrueChance <-
        robotParamsMutationInsertCondAsTrueChance <$> robotMutateParams <$>
        State.get
      condExpr <- randomExpr' contextDepth 1 (totalDepth + 1)
      expr <- mutate contextDepth (totalDepth + 1) expr
      otherExpr <- randomExpr' contextDepth 1 (totalDepth + 1)
      if probability <= insertCondAsTrueChance
        then return $ RobotCond condExpr expr otherExpr
        else return $ RobotCond condExpr otherExpr expr
    else return expr

-- | Insert a bind instruction.
insertBind :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
insertBind contextDepth totalDepth expr = do
  maxCodeDepth <- robotParamsMaxCodeDepth <$> robotMutateParams <$> State.get
  if totalDepth + codeDepth expr < maxCodeDepth
    then do
      bindMaxCount <-
        robotParamsRandomBindMaxCount <$> robotMutateParams <$> State.get
      bindCount <- randomR (0, bindMaxCount)
      boundExprs <- Seq.replicateM bindCount
                    (randomExpr' (contextDepth + bindCount) 1 (totalDepth + 1))
      expr <- mutate contextDepth (totalDepth + 1) expr
      expr <- insertBindings contextDepth bindCount expr
      return $ RobotBind boundExprs expr
    else return expr

-- | Insert bindings into existing code.
insertBindings :: Int -> Int -> RobotExpr -> State.State RobotMutate RobotExpr
insertBindings contextDepth bindCount expr@(RobotLoad index) =
  if index < contextDepth
  then return expr
  else return $ RobotLoad (contextDepth + bindCount)
insertBindings _ _ expr@(RobotConst _) = return expr
insertBindings _ _ expr@(RobotSpecialConst _) = return expr
insertBindings contextDepth bindCount (RobotBind boundExprs expr) = do
  boundExprs <- mapM (insertBindings contextDepth bindCount) boundExprs
  expr <- insertBindings contextDepth bindCount expr
  return $ RobotBind boundExprs expr
insertBindings contextDepth bindCount (RobotFunc argCount expr) = do
  expr <- insertBindings contextDepth bindCount expr
  return $ RobotFunc argCount expr
insertBindings contextDepth bindCount (RobotApply argExprs funcExpr) = do
  argExprs <- mapM (insertBindings contextDepth bindCount) argExprs
  funcExpr <- insertBindings contextDepth bindCount funcExpr
  return $ RobotApply argExprs funcExpr
insertBindings contextDepth bindCount (RobotCond condExpr trueExpr
                                       falseExpr) = do
  condExpr <- insertBindings contextDepth bindCount condExpr
  trueExpr <- insertBindings contextDepth bindCount trueExpr
  falseExpr <- insertBindings contextDepth bindCount falseExpr
  return $ RobotCond condExpr trueExpr falseExpr

-- | Get code depth.
codeDepth :: RobotExpr -> Int
codeDepth (RobotLoad _) = 1
codeDepth (RobotConst value) = 1 + valueDepth value
codeDepth (RobotSpecialConst _) = 1
codeDepth (RobotBind boundExprs expr) =
  let boundExprsDepth =
        foldl' (\depth expr -> max depth (codeDepth expr)) 0 boundExprs
  in 1 + max boundExprsDepth (codeDepth expr)
codeDepth (RobotFunc _ expr) = 1 + codeDepth expr
codeDepth (RobotApply argExprs funcExpr) =
  let argExprsDepth =
        foldl' (\depth expr -> max depth (codeDepth expr)) 0 argExprs
  in 1 + max argExprsDepth (codeDepth funcExpr)
codeDepth (RobotCond condExpr trueExpr falseExpr) =
  1 + max (codeDepth condExpr) (max (codeDepth trueExpr) (codeDepth falseExpr))

-- | Get value depth.
valueDepth :: RobotValue -> Int
valueDepth RobotNull = 1
valueDepth (RobotBool _) = 1
valueDepth (RobotInt _) = 1
valueDepth (RobotFloat _) = 1
valueDepth (RobotVector values) =
  1 + foldl' (\depth value -> max depth (valueDepth value)) 0 values
valueDepth (RobotClosure _ _ _) = 1
valueDepth (RobotIntrinsic _) = 1
valueDepth (RobotOutput value _) = 1 + valueDepth value
