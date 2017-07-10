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

module Robots.Genetic.HunterKiller.VM

  (execute,
   updateStateInto,
   updateStateTail,
   updateStateFinish,
   castToBool,
   castToInt,
   castToInteger,
   caseToDouble)

import Robots.Genetic.HunterKiller.Types
import Control.Monad.State.Strict as State
import Data.Sequence as Seq
import Data.Sequence ((><),
                      (|>))
import Control.Monad (mapM,
                     (=<<))

-- | Execute an instruction.
execute :: RobotContext -> RobotExpr -> State.State RobotState RobotValue
execute (RobotContext context) expr = do
  depth <- robotStateDepth <$> State.get
  instrCount <- robotStateInstrCount <$> State.get
  maxDepth <- robotParamsMaxDepth . robotStateParams <$> State.get
  maxInstrCount <- robotParamsMaxInstrCount . robotStateParams <$> State.get
  if (depth <= maxDepth) && (instrCount <= maxInstrCount)
    then
      case expr of
        RobotLoad index ->
          let value = case Seq.lookup index context of
                        Just value -> value
                        Nothing -> RobotNull
          in do updateStateFinish
                return value
        RobotConst value -> (value, updateStateFinish state)
        RobotSpecialConst index ->
          let value = case Seq.lookup index $ robotParamsSpecialConsts params of
                        Just value -> value
                        Nothing -> RobotNull
          in do updateStateFinish
                return value
        RobotBind boundExprs expr -> do
          let preBoundContext = context >< Seq.replicate (Seq.length boundExprs)
                                (Seq.singleton RobotNull)
          args <- executeArgExprs (RobotContext (context >< Seq.replicate (Seq.singleton RobotNull)) boundExprs
          updateStateTail
          execute (RobotContext (context >< args)) expr
        RobotFunc argCount expr -> do
          updateStateFinish
          return $ RobotClosure (RobotContext context) argCount expr
        RobotApply argExprs funcExpr -> do
          updateStateInto
          func <- execute (RobotContext context) (updateStateInto state)
                  funcExpr
          args <- executeArgExprs (RobotContext context) argExprs
          applyTail state args func
        RobotCond condExpr trueExpr falseExpr -> do
          updateStateInto
          condValue <- execute (RobotContext context) condExpr
          updateStateTail
          if castToBool condValue
            then execute (RobotContext context) trueExpr
            else execute (RobotContext context) falseExpr
    else do updateStateFinish
            return RobotNull

-- | Execute argument expressions.
executeArgExprs :: RobotContext -> Seq.Seq RobotExpr ->
                   State.State RobotState (Seq.Seq RobotValue)
executeArgExprs context exprs =
  mapM (\expr -> do
           instrCount <- robotStateInstrCount <$> State.get
           maxInstrCount <- robotParamsMaxInstrCount . robotStateParams <$> State.get
           if instrCount <= maxInstrCount
             then do updateStateInto
                     execute context expr
             else RobotNull)
       exprs

-- | Apply function.
apply :: Seq.Seq RobotValue -> RobotValue -> State.State RobotState RobotValue
apply args (RobotClosure context argCount expr) =
  let args' = Seq.take argCount args
  in let args = args' >< (Seq.replicate (argCount - (Seq.length args'))
                          (Seq.singleton RobotNull))
  in do updateStateInto
        execute (context >< args) expr
apply args (RobotIntrinsic func) = do
  updateStateInto
  func args
apply args _ = do
  updateStateFinish state
  return RobotNull

-- | Apply function.
applyTail :: Seq.Seq RobotValue -> RobotValue ->
             State.State RobotState RobotValue
applyTail args (RobotClosure context argCount expr) =
  let args' = Seq.take argCount args
  in let args = args' >< (Seq.replicate (argCount - (Seq.length args'))
                          (Seq.singleton RobotNull))
  in do updateStateTail
        execute (context >< args) expr
applyTail args (RobotIntrinsic func) = do
  updateStateTail
  func args
applyTail args _ = do
  updateStateFinish state
  return RobotNull

-- | Update state for next deeper instruction.
updateStateInto :: State.State RobotState ()
updateStateInto = do
  State.modify $ \state ->
    state { robotStateDepth = (robotStateDepth state) + 1,
            robotStateInstrCount = (robotStateInstrCount state) + 1 }

-- | Update state for next tail instruction.
updateStateTail :: State.State RobotState ()
updateStateTail = do
  State.modify $ \state ->
    state { robotStateInstrCount = (robotStateInstrCount state) + 1 }

-- | Update state for finishing instruction.
updateStateFinish :: State.State RobotState ()
updateStateFinish = do
  State.modify $ \state ->
    state { robotStateDepth = (robotStateDepth state) - 1 }

-- | Cast a RobotValue to a Bool.
castToBool :: RobotValue -> Bool
castToBool RobotNull = False
castToBool (RobotBool value) = value
castToBool (RobotInt value) = value /= 0
castToBool (RobotFloat value) = value /= 0.0
castToBool (RobotVector value) = (Seq.length value) /= 0
castToBool (RobotClosure _ _ _) = True
castToBool (RobotIntrinsic _) = True
castToBool (RobotOutput value _) = castToBool value

-- | Cast a RobotValue to an Int.
castToInt :: RobotValue -> Int
castToInt RobotNull = 0
castToInt (RobotBool False) = 0
castToInt (RobotBool True) = 1
castToInt (RobotInt value) = fromIntegral value
castToInt (RobotFloat value) = floor value
castToInt (RobotVector value) = Seq.length value
castToInt (RobotClosure _ _ _) = 1
castToInt (RobotIntrinsic _) = 1
castToInt (RobotOutput value _) = castToInt value

-- | Cast a RobotValue to an Integer.
castToInteger :: RobotValue -> Integer
castToInteger RobotNull = 0
castToInteger (RobotBool False) = 0
castToInteger (RobotBool True) = 1
castToInteger (RobotInt value) = value
castToInteger (RobotFloat value) = floor value
castToInteger (RobotVector value) = fromIntegral $ Seq.length value
castToInteger (RobotClosure _ _ _) = 1
castToInteger (RobotIntrinsic _) = 1
castToInteger (RobotOutput value _) = castToInteger value

-- | Cast a RobotValue to a Double.
castToDouble :: RobotValue -> Double
castToDouble RobotNull = 0.0
castToDouble (RobotBool False) = 0.0
castToDouble (RobotBool True) = 1.0
castToDouble (RobotInt value) = fromIntegral value
castToDouble (RobotFloat value) = value
castToDouble (RobotVector value) = fromIntegral $ Seq.length value
castToDouble (RobotClosure _ _ _) = 1.0
castToDouble (RobotIntrinsic _) = 1.0
castToDouble (RobotOutput value _) = castToDouble value
