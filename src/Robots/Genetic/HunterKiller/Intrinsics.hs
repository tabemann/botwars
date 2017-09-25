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

module Robots.Genetic.HunterKiller.Intrinsics

  (specialConsts,
   specialValueCount,
   specialConstEntries)

where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.VM
import qualified Control.Monad.State.Strict as State
import qualified Data.Sequence as Seq
import Data.Sequence ((><),
                      (<|),
                      (|>),
                      ViewL(..),
                      ViewR(..))
import Control.Monad (mapM)
import Data.Foldable (foldlM,
                      foldrM)
import Data.Bits (xor)
import qualified Data.Text as Text
import Data.Functor (fmap)

-- | Constants
specialConsts :: Seq.Seq RobotValue
specialConsts = fmap (\(RobotConstEntry value _) -> value) specialConstEntries

-- | Special value count
specialValueCount = Seq.length specialValues

-- | Special constants
specialConstEntries = specialValues >< intrinsics

-- | Special values
specialValues :: Seq.Seq RobotConstEntry
specialValues = [RobotConstEntry (RobotInt 0) "0",
                 RobotConstEntry (RobotInt 1) "1",
                 RobotConstEntry (RobotBool False) "false",
                 RobotConstEntry (RobotBool True) "true",
                 RobotConstEntry (RobotInt (-1)) "-1",
                 RobotConstEntry (RobotInt 2) "2",
                 RobotConstEntry (RobotInt (-2)) "-2",
                 RobotConstEntry (RobotFloat 0.5) "0.5",
                 RobotConstEntry (RobotFloat (-0.5)) "-0.5",
                 RobotConstEntry RobotNull "null",
                 RobotConstEntry (RobotVector Seq.empty) "empty",
                 RobotConstEntry (RobotFloat pi) "pi",
                 RobotConstEntry (RobotFloat (exp 1)) "e",
                 RobotConstEntry (RobotFloat (pi * 2.0)) "pi2"]

-- | Intrinsics
intrinsics :: Seq.Seq RobotConstEntry
intrinsics = [RobotConstEntry (RobotIntrinsic intrinsicEquals) "equals",
              RobotConstEntry (RobotIntrinsic intrinsicNotEquals) "notEquals",
              RobotConstEntry (RobotIntrinsic intrinsicGet) "get",
              RobotConstEntry (RobotIntrinsic intrinsicSet) "set",
              RobotConstEntry (RobotIntrinsic intrinsicLength) "length",
              RobotConstEntry (RobotIntrinsic intrinsicCons) "cons",
              RobotConstEntry (RobotIntrinsic intrinsicSnoc) "snoc",
              RobotConstEntry (RobotIntrinsic intrinsicLHead) "lHead",
              RobotConstEntry (RobotIntrinsic intrinsicLTail) "lTail",
              RobotConstEntry (RobotIntrinsic intrinsicRHead) "rHead",
              RobotConstEntry (RobotIntrinsic intrinsicRTail) "rTail",
              RobotConstEntry (RobotIntrinsic intrinsicMap) "map",
              RobotConstEntry (RobotIntrinsic intrinsicFoldl) "foldl",
              RobotConstEntry (RobotIntrinsic intrinsicFoldr) "foldr",
              RobotConstEntry (RobotIntrinsic intrinsicNot) "not",
              RobotConstEntry (RobotIntrinsic intrinsicAnd) "and",
              RobotConstEntry (RobotIntrinsic intrinsicOr) "or",
              RobotConstEntry (RobotIntrinsic intrinsicXor) "xor",
              RobotConstEntry (RobotIntrinsic intrinsicGT) "gt",
              RobotConstEntry (RobotIntrinsic intrinsicLT) "lt",
              RobotConstEntry (RobotIntrinsic intrinsicGTE) "gte",
              RobotConstEntry (RobotIntrinsic intrinsicLTE) "lte",
              RobotConstEntry (RobotIntrinsic intrinsicFire) "fire",
              RobotConstEntry (RobotIntrinsic intrinsicThrust) "thrust",
              RobotConstEntry (RobotIntrinsic intrinsicTurn) "turn",
              RobotConstEntry (RobotIntrinsic intrinsicAdd) "add",
              RobotConstEntry (RobotIntrinsic intrinsicSub) "sub",
              RobotConstEntry (RobotIntrinsic intrinsicMul) "mul",
              RobotConstEntry (RobotIntrinsic intrinsicDiv) "div",
              RobotConstEntry (RobotIntrinsic intrinsicMod) "mod",
              RobotConstEntry (RobotIntrinsic intrinsicPow) "pow",
              RobotConstEntry (RobotIntrinsic intrinsicSqrt) "sqrt",
              RobotConstEntry (RobotIntrinsic intrinsicAbs) "abs",
              RobotConstEntry (RobotIntrinsic intrinsicExp) "exp",
              RobotConstEntry (RobotIntrinsic intrinsicLog) "log",
              RobotConstEntry (RobotIntrinsic intrinsicSin) "sin",
              RobotConstEntry (RobotIntrinsic intrinsicCos) "cos",
              RobotConstEntry (RobotIntrinsic intrinsicTan) "tan",
              RobotConstEntry (RobotIntrinsic intrinsicAsin) "asin",
              RobotConstEntry (RobotIntrinsic intrinsicAcos) "acos",
              RobotConstEntry (RobotIntrinsic intrinsicAtan) "atan",
              RobotConstEntry (RobotIntrinsic intrinsicSinh) "sinh",
              RobotConstEntry (RobotIntrinsic intrinsicCosh) "cosh",
              RobotConstEntry (RobotIntrinsic intrinsicTanh) "tanh",
              RobotConstEntry (RobotIntrinsic intrinsicAsinh) "asinh",
              RobotConstEntry (RobotIntrinsic intrinsicAcosh) "acosh",
              RobotConstEntry (RobotIntrinsic intrinsicAtanh) "atanh"]

-- | The equals intrinsic
intrinsicEquals :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicEquals args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just x, Just y) -> return . RobotBool $ valueEqual x y
    _ -> return . RobotBool $ False

-- | The not equals intrinsic
intrinsicNotEquals :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicNotEquals args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just x, Just y) -> return . RobotBool $ valueNotEqual x y
    _ -> return . RobotBool $ True

-- | Get whether two values are equal
valueEqual :: RobotValue -> RobotValue -> Bool
valueEqual RobotNull RobotNull = True
valueEqual (RobotBool x) (RobotBool y) = x == y
valueEqual (RobotInt x) (RobotInt y) = x == y
valueEqual (RobotFloat x) (RobotFloat y) = x == y
valueEqual (RobotInt x) (RobotFloat y) = fromIntegral x == y
valueEqual (RobotFloat x) (RobotInt y) = x == fromIntegral y
valueEqual (RobotVector x) (RobotVector y) =
  if Seq.length x == Seq.length y
  then
    case Seq.elemIndexL False
         (fmap (\(x, y) -> valueEqual x y) (Seq.zip x y)) of
      Just _ -> False
      Nothing -> True
  else False
valueEqual _ _ = False

-- | Get whether two values are not equal
valueNotEqual :: RobotValue -> RobotValue -> Bool
valueNotEqual RobotNull RobotNull = False
valueNotEqual (RobotBool x) (RobotBool y) = x /= y
valueNotEqual (RobotInt x) (RobotInt y) = x /= y
valueNotEqual (RobotFloat x) (RobotFloat y) = x /= y
valueNotEqual (RobotInt x) (RobotFloat y) = fromIntegral x /= y
valueNotEqual (RobotFloat x) (RobotInt y) = x /= fromIntegral y
valueNotEqual (RobotVector x) (RobotVector y) =
  if Seq.length x == Seq.length y
  then
    case Seq.elemIndexL True
         (fmap (\(x, y) -> valueNotEqual x y) (Seq.zip x y)) of
      Just _ -> True
      Nothing -> False
  else True
valueNotEqual _ _ = True

-- | The vector get intrinsic
intrinsicGet :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicGet args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just index, Just (RobotVector vector)) ->
      case Seq.lookup (castToInt index) vector of
        Just value -> return value
        Nothing -> return RobotNull
    _ -> return RobotNull

-- | The vector set intrinsic
intrinsicSet :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSet args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args, Seq.lookup 2 args) of
    (Just index, Just value, Just (RobotVector vector)) ->
      let index' = castToInt index
      in let vector' = if index' < Seq.length vector
                       then vector
                       else vector ><
                            (Seq.replicate ((index' + 1) - (Seq.length vector))
                             RobotNull)
      in return . RobotVector $ Seq.update index' value vector'
    _ -> return RobotNull

-- | The vector cons intrinsic
intrinsicCons :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicCons args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just value, Just (RobotVector vector)) ->
      return . RobotVector $ value <| vector
    (Just value0, Just value1) ->
      return $ RobotVector [value0, value1]
    (Just value, Nothing) ->
      return . RobotVector $ Seq.singleton value
    _ -> return . RobotVector $ Seq.empty

-- | The vector snoc intrinsic
intrinsicSnoc :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSnoc args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 2 args) of
    (Just (RobotVector vector), Just value) ->
      return . RobotVector $ vector |> value
    (Just value0, Just value1) ->
      return $ RobotVector [value0, value1]
    (Just (RobotVector vector), Nothing) ->
      return . RobotVector $ vector
    (Just value, Nothing) ->
      return . RobotVector . Seq.singleton $ value
    _ -> return . RobotVector $ Seq.empty

-- | The vector length intrinsic
intrinsicLength :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLength args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotVector value) ->
      return . RobotInt . fromIntegral $ Seq.length value
    _ -> return . RobotInt $ 0

-- | The vector left head intrinsic
intrinsicLHead :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLHead args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotVector value) ->
      case Seq.viewl value of
        headValue :< _ -> return headValue
        _ -> return RobotNull
    _ -> return RobotNull

-- | The vector left tail intrinsic
intrinsicLTail :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLTail args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotVector value) ->
      case Seq.viewl value of
        _ :< vector -> return . RobotVector $ vector
        _ -> return . RobotVector $ Seq.empty
    _ -> return . RobotVector $ Seq.empty

-- | The vector right head intrinsic
intrinsicRHead :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicRHead args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotVector value) ->
      case Seq.viewr value of
        _ :> headValue -> return headValue
        _ -> return RobotNull
    _ -> return RobotNull

-- | The vector right tail intrinsic
intrinsicRTail :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicRTail args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotVector value) ->
      case Seq.viewr value of
        vector :> _ -> return . RobotVector $ vector
        _ -> return . RobotVector $ Seq.empty
    _ -> return . RobotVector $ Seq.empty

-- | The vector map intrinsic
intrinsicMap :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicMap args = do
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just func, Just (RobotVector vector)) -> do
      vector' <- mapM (\value -> apply (Seq.singleton value) func) vector
      updateStateFinish
      return . RobotVector $ vector'
    _ -> do updateStateFinish
            return . RobotVector $ Seq.empty

-- | The vector foldl intrinsic
intrinsicFoldl :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicFoldl args =
  case (Seq.lookup 0 args, Seq.lookup 1 args, Seq.lookup 2 args) of
    (Just func, Just start, Just (RobotVector vector)) -> do
      value <- foldlM (\current value ->
                         apply [current, value] func)
               start vector
      updateStateFinish
      return value
    _ -> do updateStateFinish
            return RobotNull

-- | The vector foldr intrinsic
intrinsicFoldr :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicFoldr args =
  case (Seq.lookup 0 args, Seq.lookup 1 args, Seq.lookup 2 args) of
    (Just func, Just start, Just (RobotVector vector)) -> do
      value <- foldrM (\value current ->
                         apply [value, current] func)
               start vector
      updateStateFinish
      return value
    _ -> do updateStateFinish
            return RobotNull

-- | The not intrinsic
intrinsicNot :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicNot args = do
  updateStateFinish
  return . RobotBool $
    case Seq.lookup 0 args of
      Just value -> not . castToBool $ value
      _ -> False

-- | The and intrinsic
intrinsicAnd :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAnd args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value0, Just value1) -> castToBool value0 && castToBool value1
      _ -> False

-- | The or intrinsic
intrinsicOr :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicOr args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value0, Just value1) -> castToBool value0 || castToBool value1
      _ -> True

-- | The xor intrinsic
intrinsicXor :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicXor args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value0, Just value1) -> castToBool value0 `xor` castToBool value1
      _ -> True

-- | The greater than intrinsic
intrinsicGT :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicGT args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just (RobotFloat value0), Just value1) -> value0 > castToDouble value1
      (Just value0, Just (RobotFloat value1)) -> castToDouble value0 > value1
      (Just value0, Just value1) -> castToInteger value0 > castToInteger value1
      _ -> False

-- | The less than intrinsic
intrinsicLT :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLT args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just (RobotFloat value0), Just value1) -> value0 < castToDouble value1
      (Just value0, Just (RobotFloat value1)) -> castToDouble value0 < value1
      (Just value0, Just value1) -> castToInteger value0 < castToInteger value1
      _ -> False

-- | The greater than or equal to intrinsic
intrinsicGTE :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicGTE args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just (RobotFloat value0), Just value1) -> value0 >= castToDouble value1
      (Just value0, Just (RobotFloat value1)) -> castToDouble value0 >= value1
      (Just value0, Just value1) -> castToInteger value0 >= castToInteger value1
      _ -> False

-- | The less than or equal to intrinsic
intrinsicLTE :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLTE args = do
  updateStateFinish
  return . RobotBool $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just (RobotFloat value0), Just value1) -> value0 <= castToDouble value1
      (Just value0, Just (RobotFloat value1)) -> castToDouble value0 <= value1
      (Just value0, Just value1) -> castToInteger value0 <= castToInteger value1
      _ -> False

-- | The fire intrinsic
intrinsicFire :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicFire args = do
  updateStateFinish
  return $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value, Just power) ->
        case value of
          RobotOutput value action ->
            RobotOutput value
              (action { robotActionFirePower =
                        robotActionFirePower action + castToDouble power })
          value ->
            RobotOutput value
              (RobotAction { robotActionFirePower = castToDouble power,
                             robotActionThrustPower = 0.0,
                             robotActionTurnPower = 0.0 })
      (Just value, Nothing) -> value
      _ -> RobotNull

-- | The thrust intrinsic
intrinsicThrust :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicThrust args = do
  updateStateFinish
  return $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value, Just power) ->
        case value of
          RobotOutput value action ->
            RobotOutput value
              (action { robotActionThrustPower =
                        robotActionThrustPower action + castToDouble power })
          value ->
            RobotOutput value
              (RobotAction { robotActionFirePower = 0.0,
                             robotActionThrustPower = castToDouble power,
                             robotActionTurnPower = 0.0 })
      (Just value, Nothing) -> value
      _ -> RobotNull

-- | The turn intrinsic
intrinsicTurn :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicTurn args = do
  updateStateFinish
  return $
    case (Seq.lookup 0 args, Seq.lookup 1 args) of
      (Just value, Just power) ->
        case value of
          RobotOutput value action ->
            RobotOutput value
              (action { robotActionTurnPower =
                        robotActionTurnPower action + castToDouble power })
          value ->
            RobotOutput value
              (RobotAction { robotActionFirePower = 0.0,
                             robotActionThrustPower = 0.0,
                             robotActionTurnPower = castToDouble power })
      (Just value, Nothing) -> value
      _ -> RobotNull

-- | The add intrinsic
intrinsicAdd :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAdd args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just (RobotFloat value0), Just value1) ->
      return . RobotFloat $ value0 + castToDouble value1
    (Just value0, Just (RobotFloat value1)) ->
      return . RobotFloat $ castToDouble value0 + value1
    (Just value0, Just value1) ->
      return . RobotInt $ castToInteger value0 + castToInteger value1
    _ -> return . RobotInt $ 0

-- | The subtract intrinsic
intrinsicSub :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSub args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just (RobotFloat value0), Just value1) ->
      return . RobotFloat $ value0 - castToDouble value1
    (Just value0, Just (RobotFloat value1)) ->
      return . RobotFloat $ castToDouble value0 - value1
    (Just value0, Just value1) ->
      return . RobotInt $ castToInteger value0 - castToInteger value1
    _ -> return . RobotInt $ 0

-- | The multiply intrinsic
intrinsicMul :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicMul args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just (RobotFloat value0), Just value1) ->
      return . RobotFloat $ value0 * castToDouble value1
    (Just value0, Just (RobotFloat value1)) ->
      return . RobotFloat $ castToDouble value0 * value1
    (Just value0, Just value1) ->
      return . RobotInt $ castToInteger value0 * castToInteger value1
    _ -> return . RobotInt $ 1

-- | The divide intrinsic
intrinsicDiv :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicDiv args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just (RobotFloat value0), Just value1) ->
      if castToDouble value1 /= 0.0
      then return . RobotFloat $ value0 / castToDouble value1
      else return RobotNull
    (Just value0, Just (RobotFloat value1)) ->
      if value1 /= 0.0
      then return . RobotFloat $ castToDouble value0 / value1
      else return RobotNull
    (Just value0, Just value1) ->
      let value0' = castToInteger value0
          value1' = castToInteger value1
      in if value1' /= 0
         then if (value0' `mod` value1') == 0
              then return . RobotInt $ value0' `div` value1'
              else return . RobotFloat $ castToDouble value0 / castToDouble value1
         else return RobotNull
    _ -> return . RobotInt $ 1

-- | The modulus intrinsic
intrinsicMod :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicMod args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just (RobotFloat value0), Just value1) ->
      let value1' = castToDouble value1
      in if value1' /= 0.0
         then let quotient = value0 / value1'
              in return . RobotFloat $
                 value0 - ((fromIntegral $ floor quotient) * value1')
         else return RobotNull
    (Just value0, Just (RobotFloat value1)) ->
      if value1 /= 0.0
      then let value0' = castToDouble value0
           in let quotient = value0' / value1
              in return . RobotFloat $
                 value0' - ((fromIntegral $ floor quotient) * value1)
      else return RobotNull
    (Just value0, Just value1) ->
      if castToInteger value1 /= 0
      then return . RobotInt $ castToInteger value0 `mod` castToInteger value1
      else return RobotNull
    _ -> return . RobotInt $ 0

-- | The power intrinsic
intrinsicPow :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicPow args = do
  updateStateFinish
  case (Seq.lookup 0 args, Seq.lookup 1 args) of
    (Just value0, Just (RobotFloat value1)) ->
      return . RobotFloat $ castToDouble value0 ** value1
    (Just (RobotFloat value0), Just value1) ->
      return . RobotFloat $ value0 ** castToDouble value1
    (Just (RobotInt value0), Just (RobotInt value1)) ->
      if value1 >= 0
      then return . RobotInt $ value0 ^ value1
      else return . RobotFloat $ fromIntegral value0 ** fromIntegral value1
    (Just value0, Just value1) ->
      let value0' = castToInteger value0
          value1' = castToInteger value1
      in if value1' >= 0
         then return . RobotInt $ value0' ^ value1'
         else return . RobotFloat $ fromIntegral value0' ** fromIntegral value1'
    _ -> return . RobotInt $ 1

-- | The sqrt intrinsic
intrinsicSqrt :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSqrt args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . sqrt . castToDouble $ value
    _ -> return . RobotFloat $ 0.0

-- | The abs intrinsic
intrinsicAbs :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAbs args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just (RobotFloat value) -> return . RobotFloat . abs $ value
    Just value -> return . RobotInt . abs . castToInteger $ value
    _ -> return . RobotInt $ 0

-- | The exp intrinsic
intrinsicExp :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicExp args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . exp . castToDouble $ value
    _ -> return . RobotFloat $ 1.0

-- | Infinity
inf :: Double
inf = 1.0 / 0.0

-- | The log intrinsic
intrinsicLog :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicLog args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . log . castToDouble $ value
    _ -> return . RobotFloat $ -inf

-- | The sin intrinsic
intrinsicSin :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSin args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . sin . castToDouble $ value
    _ -> return . RobotFloat . sin $ 0.0

-- | The cos intrinsic
intrinsicCos :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicCos args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . cos . castToDouble $ value
    _ -> return . RobotFloat . cos $ 0.0

-- | The tan intrinsic
intrinsicTan :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicTan args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . tan . castToDouble $ value
    _ -> return . RobotFloat . tan $ 0.0

-- | The asin intrinsic
intrinsicAsin :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAsin args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . asin . castToDouble $ value
    _ -> return . RobotFloat . asin $ 0.0

-- | The acos intrinsic
intrinsicAcos :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAcos args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . acos . castToDouble $ value
    _ -> return . RobotFloat . acos $ 0.0

-- | The atan intrinsic
intrinsicAtan :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAtan args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . atan . castToDouble $ value
    _ -> return . RobotFloat . atan $ 0.0

-- | The sinh intrinsic
intrinsicSinh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicSinh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . sinh . castToDouble $ value
    _ -> return . RobotFloat . sinh $ 0.0

-- | The cosh intrinsic
intrinsicCosh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicCosh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . cosh . castToDouble $ value
    _ -> return . RobotFloat . cosh $ 0.0

-- | The tanh intrinsic
intrinsicTanh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicTanh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . tanh . castToDouble $ value
    _ -> return . RobotFloat . tanh $ 0.0

-- | The asinh intrinsic
intrinsicAsinh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAsinh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . asinh . castToDouble $ value
    _ -> return . RobotFloat . asinh $ 0.0

-- | The acosh intrinsic
intrinsicAcosh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAcosh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . acosh . castToDouble $ value
    _ -> return . RobotFloat . acosh $ 0.0

-- | The atanh intrinsic
intrinsicAtanh :: Seq.Seq RobotValue -> State.State RobotState RobotValue
intrinsicAtanh args = do
  updateStateFinish
  case Seq.lookup 0 args of
    Just value -> return . RobotFloat . atanh . castToDouble $ value
    _ -> return . RobotFloat . atanh $ 0.0
