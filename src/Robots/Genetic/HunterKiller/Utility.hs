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

module Robots.Genetic.HunterKiller.Utility

  (prepText,
   collectJust,
   sign,
   applyDeltaVectorFriction,
   addVector,
   subVector,
   mulVector,
   wrap,
   wrapDim,
   absVector,
   vectorAngle,
   normalizeAngle,
   modDouble)
  
where

import Robots.Genetic.HunterKiller.Types
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Text as Text
import Data.Functor (fmap)
import Data.Foldable (Foldable,
                      foldl')
import Data.Char (isControl)

-- | Prep text.
prepText :: Text.Text -> Text.Text
prepText = removeComments . convertCR

-- | Convert CR in text.
convertCR :: Text.Text -> Text.Text
convertCR = Text.replace "\r" "\n" . Text.replace "\r\n" "\n"

-- | Remove comments from text.
removeComments :: Text.Text -> Text.Text
removeComments =
  Text.intercalate "\n" . fmap (fst . Text.breakOn "#") . Text.splitOn "\n"

-- | Collect Just entries from a sequence into a new sequence
collectJust :: Foldable t => t (Maybe a) -> Seq.Seq a
collectJust items = foldl' (\entries entry ->
                               case entry of
                                 Just entry -> entries |> entry
                                 Nothing -> entries)
                    Seq.empty items

-- | Get the sign of a number.
sign :: Double -> Double
sign n = if n > 0.0 then 1.0 else if n < 0.0 then -1.0 else 0.0

-- | Apply frication two location delta.
applyDeltaVectorFriction :: (Double, Double) -> Double -> (Double, Double)
applyDeltaVectorFriction vector friction =
  let distance = absVector vector
      angle = vectorAngle vector
      friction' = 1.0 - friction
  in case angle of
       Just angle -> (cos angle * distance * friction',
                      sin angle * distance * friction')
       Nothing -> (0.0, 0.0)

-- | Add two vectors.
addVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVector (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Subtract two vectors.
subVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
subVector (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- | Multiply a vector by a scalar value.
mulVector :: Double -> (Double, Double) -> (Double, Double)
mulVector a (x, y) = (x * a, y * a)

-- | Wrap coordinate.
wrap :: (Double, Double) -> (Double, Double)
wrap (x, y) = (wrapDim x, wrapDim y)

-- | Wrap a single dimension
wrapDim :: Double -> Double
wrapDim x =
  let x' = (modDouble (x + 0.5) 1.0) - 0.5
  in if x' /= (-0.5) then x' else 0.5

-- | Absolute value of a vector function
absVector :: (Double, Double) -> Double
absVector (x, y) = sqrt ((x ** 2.0) + (y ** 2.0))

-- | Angle of vector function function
vectorAngle :: (Double, Double) -> Maybe Double
vectorAngle (x, y) =
  if x > 0.0
  then Just . atan $ y / x
  else if (x < 0.0) && (y >= 0.0)
  then Just $ (atan $ y / x) + pi
  else if (x < 0.0) && (y < 0.0)
  then Just $ (atan $ y / x) - pi
  else if (x == 0.0) && (y > 0.0)
  then Just $ pi / 2.0
  else if (x == 0.0) && (y < 0.0)
  then Just $ -(pi / 2.0)
  else Nothing

-- | Angle normalization functon
normalizeAngle :: Double -> Double
normalizeAngle angle =
  if angle > 0.0
  then normalizeAngleCore angle
  else if angle < 0.0
  then let angle' = -(normalizeAngleCore (-angle))
       in if angle' > (-pi)
          then angle'
          else pi
  else 0.0

-- | Core of angle normalization
normalizeAngleCore :: Double -> Double
normalizeAngleCore angle =
  let limited = modDouble angle (pi * 2.0)
  in if limited <= pi
     then limited
     else (-pi) + (limited - pi)

-- | Floating point modulus
modDouble :: Double -> Double -> Double
modDouble x y = x - ((fromIntegral (floor (x / y))) * y)
