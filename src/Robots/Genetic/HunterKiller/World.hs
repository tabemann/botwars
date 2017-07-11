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

module Robots.Genetic.HunterKiller.World

  (worldCycle,
   generateRobot,
   startingContextDepth)
  
where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.VM
import Data.Sequence as Seq
import Data.Sequence ((|>),
                      (><))
import Control.Monad.State.Strict as State
import Data.Functor (fmap,
                     (<$>))
import Control.Monad (mapM,
                      mapM_,
                      (=<<))
import Data.Foldable (Foldable,
                      foldl')
import System.Random as Random

-- | Execute a world cycle.
worldCycle :: State.State RobotWorld RobotCycleState
worldCycle = do
  world <- State.get
  let params = robotWorldParams world
  cycleEndStates <-
    mapM (\robot -> (\output -> (robot, output)) <$> robotCycle robot)
      (robotWorldRobots world)
  let updatedRobotsAndShots =
        fmap (\(robot, (stateData, action)) ->
                updateRobot robot stateData action params)
        cycleEndStates
      robots = fmap fst updatedRobotsAndShots
      newShots = collectJust . fmap snd $ updatedRobotsAndShots
      shots = robotWorldShots world >< newShots
      updatedShots = collectJust $ fmap (\shot -> updateShot shot params) shots
  collideRobotsAndShots robots updatedShots
  State.modify $ \world ->
                   world { robotWorldCycles = robotWorldCycles world + 1 }
  world <- State.get
  if (robotWorldCycles world >= robotParamsMaxCycles params) ||
     (robotWorldKills world >= robotParamsMaxKills params)
    then return RobotEndRound
    else return RobotNextCycle

-- | Collide robots and shots.
collideRobotsAndShots :: Seq.Seq Robot -> Seq.Seq Shot ->
                         State.State RobotWorld ()
collideRobotsAndShots robots shots = do
  (robots, shots, kills) <-
    foldM (\(robots, shots, kills) robot -> do
              (robot, shots, robotKills) <-
                collideRobotWitShots robot shots
              return $ (robots |> robot, shots, kills >< robotKills))
    (Seq.empty, shots, Seq.empty)
  let robots' = fmap (updateRobotForKills kills) robots
  state (\world -> world { robotWorldRobots = robots',
                           robotWorldShots = shots,
                           robotWorldKills = robotWorldKills world +
                                             Seq.length kills })

-- | Collide robots with shots.
collideRobotWithShots :: Robot -> Seq.Seq Shot ->
                         State.State RobotWorld
                           (Robot, Seq.Seq Shot, Seq.Seq Int)
collideRobotWithShots robot shots = do
  params <- robotWorldParams <$> State.get
  let (shotsThatHit, shotsThatDidNotHit) =
        Seq.partition
          (\shot ->
             let relativeLocation =
                   subVector (shotLocation shot) (robotLocation robot)
                 distance = absVector relativeLocation
             in distance < robotParamsRobotRadius params)
          shots
      shotHarm =
        sum $ fmap (\shot ->
                      let relativeLocation =
                            subVector (robotLocation robot) (shotLocation shot)
                          angle = vectorAngle relativeLocation
                          factor = 
                            case angle of
                              Just angle ->
                                let deltaAngle =
                                      vectorAngle (shotLocationDelta shot)
                                in case deltaAngle of
                                  Just deltaAngle ->
                                    max 0.0 . cos $ angle - deltaAngle
                                  Nothing -> 0.0
                              Nothing -> 1.0
                      in shotEnergy shot * factor *
                         robotParamsShotHarmFactor params)
          shotsThatHit
      health = robotHealth robot - shotHarm
  if health > 0.0
    then return $ (robot { robotHealth = health }, shotsThatDidNotHit,
                   Seq.empty)
    else do
      robot <- respawnRobot robot
      return (robot, shotThatDidNotHit, fmap shotRobotIndex shotsThatHit)

-- | Respawn a robot
respawnRobot :: Robot -> State.State RobotWorld Robot
respawnRobot robot = do
  world <- State.get
  let params = robotWorldParams world
      index = robotWorldNextRobotIndex world
      program = robotExpr robot
      score = robotScore robot
      gen = robotWorldRandom world
      (robot, gen') = generateRobot index program (score - 1) gen params
  State.modify $ (\world -> world { robotWorldNextRobotIndex = index + 1,
                                    robotWorldRandom = gen' })
  return robot

-- | Generate a robot.
generateRobot :: Int -> RobotExpr -> Int -> RobotParams -> Random.StdGen ->
                 (Robot, Random.StdGen)
generateRobot index program score gen params = 
  let (generalEnergy, gen') =
        Random.randomR (robotParamsMinInitialGeneralEnergy params,
                        robotParamsMaxInitialGeneralEnergy params) gen
      (weaponEnergy, gen'') =
        Random.randomR (robotParamsMinInitialWeaponEnergy params,
                        robotParamsMaxInitialWeaponEnergy params) gen'
      (health, gen''') =
        Random.randomR (robotParamsMinInitialHealth params,
                        robotParamsMaxInitialHealth params) gen''
      (locationDeltaAbs, gen'''') =
        Random.randomR (robotParamsMinInitialLocationDeltaAbs params,
                        robotParamsMaxInitialLocationDeltaAbs params) gen'''
      (rotationDelta, gen''''') =
        Random.randomR (-(robotParamsMinInitialRotationDeltaAbs params),
                        robotParamsMaxInitialRotationDeltaAbs params) gen''''
      (locationX, gen'''''') = Random.randomR (-0.5, 0.5) gen'''''
      (locationY, gen''''''') = Random.randomR (-0.5, 0.5) gen''''''
      (rotation, gen'''''''') = Random.randomR (-pi, pi) gen'''''''
      (locationDeltaAngle, gen''''''''') = Random.randomR (-pi, pi) gen''''''''
      locationX' = wrapDim locationX
      locationY' = wrapDim locationY
      locationDeltaX = cos locationDeltaAngle * locationDeltaAbs
      locationDeltaY = sin locationDeltaAngle * locationDeltaAbs
      rotation' = normalizeAngle rotation
  in (Robot { robotIndex = index,
             robotExpr = program,
             robotData = RobotNull,
             robotLocation = (locationX', locationY'),
             robotLocationDelta = (locationDeltaX, locationDeltaY),
             robotRotation = rotation,
             robotRotationDelta = rotationDelta,
             robotGeneralEnergy = generalEnergy,
             robotWeaponEnergy = weaponEnergy,
             robotHealth = health,
             robotScore = score },
       gen''''''''')

-- | Update robots for kills.
updateRobotForKills :: Seq.Seq Int -> Robot -> Robot
updateRobotForKills kills robot =
  foldl' (\robot kill -> if kill == robotIndex robot
                         then robot { robotScore = robotScore robot + 1 }
                         else robot)
    robot kills

-- | Collect Just entries from a sequence into a new sequence
collectJust :: Foldable t => t (Maybe a) -> Seq.Seq a
collectJust items = foldl' (\entries entry ->
                               case entry of
                                 Just entry -> entries |> entry
                                 Nothing -> entries)
                    Seq.empty items

-- | Update a robot.
updateRobot :: Robot -> RobotValue -> RobotAction -> RobotParams ->
  (Robot, Maybe Shot)
updateRobot robot stateData action params =
  let generalEnergy =
        min (robotGeneralEnergy robot + robotParamsGeneralEnergyGain params) 1.0
      weaponEnergy =
        min (robotWeaponEnergy robot + robotParamsWeaponEnergyGain params) 1.0
      health =
        min (robotHealth robot + robotParamsHealthGain params) 1.0
      locationDelta =
        applyDeltaVectorFriction (robotLocationDelta robot)
          (robotParamsLocationFriction params)
      rotationDelta =
        robotRotationDelta robot * robotParamsRotationFriction params
      turnValue = robotActionTurnPower action
      turnPower = min generalEnergy (abs turnValue)
      generalEnergy' = generalEnergy - turnPower
      thrustValue = robotActionThrustPower action
      thrustPower = min generalEnergy' (abs thrustValue)
      generalEnergy'' = generalEnergy' - thrustPower
      rotationDelta' = rotationDelta + (turnPower * sign turnValue *
                                        robotParamsTurnFactor params)
      rotation = normalizeAngle $ robotRotation robot + rotationDelta'
      locationDelta' =
        addVector locationDelta
          (cos rotation * thrustPower * sign thrustValue *
           robotParamsThrustFactor params,
           sin rotation * thrustPower * sign thrustValue *
           robotParamsThrustFactor params)
      location = wrap $ addVector (robotLocation robot) locationDelta'
      firePower = min weaponEnergy . max 0.0 $ robotActionFirePower action
      weaponEnergy' = weaponEnergy - firePower
      robot' = robot { robotData = stateData,
                       robotLocation = location,
                       robotLocationDelta = locationDelta',
                       robotRotation = rotation,
                       robotRotationDelta = rotationDelta',
                       robotGeneralEnergy = generalEnergy'',
                       robotWeaponEnergy = weaponEnergy',
                       robotHealth = health },
      shot = if firePower > 0.0
             then Just $ fireShot robot' firePower params
             else Nothing
  in (robot', shot)

-- | Fire a shot.
fireShot :: Robot -> Double -> RobotParams -> Shot
fireShot robot power params =
  let shotLocationDelta =
        (cos (robotRotation robot) * robotParamsShotSpeed params,
         sin (robotRotation robot) * robotParamsShotSpeed params)
      shotLocationDelta' =
        addVector shotLocationDelta (robotLocationDelta robot)
      shotLocationOffset =
        (cos (robotRotation robot) * robotParamsRobotRadius params,
         sin (robotRotation robot) * robotParamsRobotRadius params)
      shotLocation =
        addVector (robotLocation robot) shotLocationOffset
      shotEnergy = power * robotParamsFireFactor params
  in Shot { shotLocation = shotLocation,
            shotLocationDelta = shotLocationDelta',
            shotEnergy = shotEnergy,
            shotRobotIndex = robotIndex robot }

-- | Update a shot.
updateShot :: Shot -> RobotParams -> Maybe Shot
updateShot shot params =
  let energy = shotEnergy shot * robotParamsShotEnergyDecay params
      location =
        wrap  $ addVector (shotLocation shot) (shotLocationDelta shot)
  in if energy >= robotParamsShotMinEnergy params
     then Just $ shot { shotEnergy = energy, shotLocation = location }
     else Nothing

-- | Get the sign of a number.
sign :: Double -> Double
sign n = if n > 0.0 then 1.0 else if n < 0.0 then -1.0 else 0.0

-- | Apply frication two location delta.
applyDeltaVectorFriction :: (Double, Double) -> Double -> (Double, Double)
applyDeltaVectorFriction vector friction =
  let distance = vectorAbs vector
      angle = vectorAngle vector
  in case angle of
       Just angle -> (cos angle * distance * friction,
                      sin angle * distance * friction)
       Nothing -> (0.0, 0.0)

-- | Starting context depth.
startingContextDepth :: Int
startingContextDepth = 9

-- | Execute a robot cycle.
robotCycle :: Robot -> State.State RobotWorld (RobotValue, RobotAction)
robotCycle robot = do
  world <- State.get
  let locationDeltaAbs = vectorAbs . robotLocationDelta $ robot
      locationDeltaAngle = vectorAngle . robotLocationDelta $ robot
      locationDeltaAngle' =
        case locationDeltaAngle of
          Just angle -> normalizeAngle $ angle - (robotRotation robot)
          Nothing -> 0.0
      robotObjects = fmap (\robot -> (robotLocation robot,
                                      robotLocationDelta robot))
                       (robotWorldRobots world)
      shotObjects = fmap (\shot -> (shotLocation shot,
                                    shotLocationDelta shot))
                      (robotWorldShots world)
      input = Seq.fromList [robotData robot,
                            RobotFloat locationDeltaAbs,
                            RobotFloat locationDeltaAngle',
                            RobotFloat . robotRotationDelta $ robot,
                            RobotFloat . robotGeneralEnergy $ robot,
                            RobotFloat . robotWeaponEnergy $ robot,
                            RobotFloat . robotHealth $ robot,
                            RobotVector $ detect robot robotObjects
                             (robotWorldParams world),
                            RobotVector $ detect robot shotObjects
                             (robotWorldParams world)]
  return . extractOutput $
    State.evalState (execute (RobotContext input) (robotExpr robot))
      (RobotState { robotStateParams = robotWorldParams world,
                    robotStateDepth = 0,
                    robotStateInstrCount })

-- | Detect robots.
detect :: Robot -> Seq.Seq ((Double, Double), (Double, Double)) ->
  RobotParams -> Seq.Seq RobotValue
detect robot objects params =
  let location = robotLocation robot
      objectsWithInfo =
        fmap (\(objectLocation, _) @ object ->
                let objectRelativeLocation =
                      wrap $ subVector objectLocation location
                    objectDistance = vectorAbs objectRelativeLocation
                    objectAngle = vectorAngle objectRelativeLocation
                    objectAngle' =
                      case objectAngle of
                        Just objectAngle ->
                          normalizeAngle $ objectAngle - robotRotation robot
                        Nothing -> 0.0
                in (object, objectDistance, objectAngle'))
        objects
      filteredObjects =
        Seq.filter (\(_, otherDistance, otherAngle) ->
                      otherDistance <= (robotParamsViewDistance params) &&
                      (abs otherAngle) <= (robotParamsViewAngle params))
        objectsWithInfo
      sortedObjects =
        Seq.unstableSortBy (\(_, distance0, _) (_, distance1, _) ->
                              compare distance0 distance1)
        filteredObjects
      sortedObjects' =
        Seq.sortBy (\(_, distance0, angle0) (_, distance1, angle1) ->
                       compare
                         (viewScore distance0 angle0 params)
                         (viewScore distance1 angle1 params))
        sortedObjects
  in fmap (\((objectLocation, objectLocationDelta), distance, angle) ->
             let objectNewLocation =
                   addVector objectLocation objectLocationDelta
                 objectRelativeNewLocation =
                   wrap $ subVector objectNewLocation location
                 objectNewDistance = vectorAbs objectRelativeNewLocation
                 objectNewAngle = vectorAngle objectNewDistance
                 objectNewAngle' =
                   case objectNewAngle of
                     Just objectNewAngle ->
                       normalizeAngle $ objectNewAngle - robotRotation robot
                     Nothing -> 0.0
                 objectDistanceDelta = objectNewDistance - distance
                 objectAngleDelta = normalizeAngle $ objectNewAngle - angle
             RobotVector . fromList $
               [RobotFloat distance,
                RobotFloat angle,
                RobotFloat objectDistanceDelta,
                RobotFloat objectNewDelta])
          sortedObjects'

-- | Calculate a view score based on distance and angle.
viewScore :: Double -> Double -> RobotParams
viewScore distance angle params =
  ((robotParamsViewSortDistanceFactor params) * distance) +
  ((robotParamsViewSortAngleFactor params) * (abs angle))

-- | Extract the data and actions from a robot output.
extractOutput :: RobotValue -> (RobotValue, RobotAction)
extractOutput (RobotOutput value action) = (value, action)
extractOutput value = (value, RobotAction { robotActionFirePower = 0.0,
                                            robotActionThrustPower = 0.0,
                                            robotActionTurnPower = 0.0 })

-- | Add two vectors.
addVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVector (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

-- | Subtract two vectors.
subVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
subVector (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)

-- | Wrap coordinate.
wrap :: (Double, Double) -> (Double, Double)
wrap (x, y) = (wrapDim x, wrapDim y)

-- | Wrap a single dimension
wrapDim :: Double -> Double
wrapDim x =
  let x' = (modDouble (x + 0.5) 1.0) - 0.5
  in if x' != (-0.5) then x' else 0.5

-- | Absolute value of a vector function
vectorAbs :: (Double, Double) -> Double
vectorAbs (x, y) = sqrt ((x ** 2.0) + (y ** 2.0))

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
