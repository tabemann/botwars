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

module Robots.Genetic.HunterKiller.Render (drawWorld) where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.Utility
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.Sequence as Seq
import Control.Monad (mapM_)
import Control.Monad.IO.Class (liftIO)

-- | Draw a world.
drawWorld :: RobotWorld -> Double -> Double -> Cairo.Render ()
drawWorld world w h = do
  mapM_ (drawRobot w h (robotWorldParams world)) (robotWorldRobots world)
  mapM_ (drawShot w h (robotWorldParams world)) (robotWorldShots world)

-- | Draw a robot.
drawRobot :: Double -> Double -> RobotParams -> Robot -> Cairo.Render ()
drawRobot w h params robot = do
  Cairo.setLineWidth 1.0
  let (centerX, centerY) = convertCoord (robotLocation robot) w h
  let radius = robotParamsRobotRadius params
  Cairo.arc centerX centerY (radius * w * robotParamsOversizeRadius params)
    0.0 (2.0 * pi)
  Cairo.setSourceRGB (1.0 - ((1.0 - robotHealth robot) / 2.0))
    (1.0 - ((1.0 - robotGeneralEnergy robot) / 2.0))
    (1.0 - ((1.0 - robotGeneralEnergy robot) / 2.0))
  Cairo.fill
  Cairo.arc centerX centerY (radius * w) 0.0 (2.0 * pi)
  Cairo.setSourceRGB (robotHealth robot) (robotGeneralEnergy robot)
    (robotWeaponEnergy robot)
  Cairo.fill
  Cairo.setSourceRGB 0.0 0.0 0.0
  Cairo.arc centerX centerY (radius * w * robotParamsOversizeRadius params)
    0.0 (2.0 * pi)
  Cairo.stroke
  let rotation = robotRotation robot
  let (endX, endY) = convertCoord (addVector (robotLocation robot)
                                   (mulVector
                                    (robotParamsAimRadius params * radius)
                                    (cos rotation, sin rotation))) w h
  Cairo.moveTo centerX centerY
  Cairo.lineTo endX endY
  Cairo.stroke

-- | Draw a shot.
drawShot :: Double -> Double -> RobotParams -> Shot -> Cairo.Render ()
drawShot w h params shot = do
  Cairo.setLineWidth 1.0
  Cairo.setSourceRGB 1.0 0.0 0.0
  let (x, y) = convertCoord (shotLocation shot) w h
  Cairo.moveTo x y
  let (endX, endY) = convertCoord (addVector (shotLocation shot)
                                   (shotLocationDelta shot))
                     w h
  Cairo.lineTo endX endY
  Cairo.stroke

-- | Convert a coordinate.
convertCoord :: (Double, Double) -> Double -> Double -> (Double, Double)
convertCoord (x, y) w h = ((x * w) + (w / 2.0), (h / 2.0) - (y * h))
