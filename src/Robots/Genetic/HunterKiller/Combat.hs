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

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Robots.Genetic.HunterKiller.Combat

  (initCont,
   executeCycle)
  
where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.World
import Robots.Genetic.HunterKiller.Mutate
import Control.Monad.State.Strict as State
import Data.Sequence as Seq
import Data.Sequence ((<|),
                      (|>))
import Data.Functor ((<$>),
                     fmap)
import System.Random as Random
import Control.Monad.IO.Class as MonadIO
import Control.Monad.Trans.Class (MonadTrans,
                                  lift)
import Data.Foldable (foldlM,
                      foldl')

-- | Execute robot combat.
initCont :: Seq.Seq RobotExpr -> RobotParams -> Random.StdGen -> RobotCont
initCont programs params gen = do
  RobotCont { robotContParams = params,
              robotContRandom = gen,
              robotContWorld = Nothing,
              robotContPrevWorlds = Seq.empty,
              robotContPrograms = programs,
              robotContSavedWorlds = Seq.empty }

-- | Execute a cycle.
executeCycle :: RobotCont -> (RobotEvent, RobotCont)
executeCycle cont =
  State.runState executeCycle' cont
  where
    executeCycle' = do
      world <- robotContWorld <$> State.get
      case world of
        Nothing -> do
          world <- setupWorld
          State.modify $ \cont -> cont { robotContWorld = Just world }
          executeCycle'
        Just world ->
          let (cycleState, !world') = State.runState worldCycle world
          in case cycleState of
               RobotNextCycle -> do
                 State.modify (updateCont world')
                 return $ RobotWorldCycle world'
               RobotEndRound -> do
                 State.modify (updateCont world')
                 startNextRound
                 return $ RobotRoundDone world'
    updateCont world cont =
      cont { robotContWorld = Just world,
             robotContPrevWorlds =
               let maxRewind = robotParamsMaxRewind $ robotContParams cont
               in case robotContWorld cont of
                 Just prevWorld ->
                   let prevWorlds =
                         robotContPrevWorlds cont |> cleanupWorld prevWorld
                   in if Seq.length prevWorlds > maxRewind
                      then Seq.drop (Seq.length prevWorlds - maxRewind)
                           prevWorlds
                      else prevWorlds
                 Nothing -> Seq.empty }
                 
-- | Start the next round
startNextRound :: State.State RobotCont ()
startNextRound = do
  world <- do
    world <- robotContWorld <$> State.get
    case world of
      Just world -> return $ cleanupWorld world
      Nothing -> error "impossible"
  savedWorlds <- robotContSavedWorlds <$> State.get
  minKills <- robotParamsMinKills <$> robotContParams <$> State.get
  savedWorldCount <-
    robotParamsSavedWorldCount <$> robotContParams <$> State.get
  if robotWorldKills world >= minKills
    then do
      State.modify $ \cont -> cont { robotContSavedWorlds =
                                       Seq.take savedWorldCount
                                       (world <| savedWorlds) }
      prepareNextRound world
    else
      case Seq.lookup 1 savedWorlds of
        Just savedWorld -> do
          let savedWorld' =
                savedWorld { robotWorldRandom = robotWorldRandom world }
          State.modify $ \cont -> cont { robotContSavedWorlds =
                                           Seq.drop 1 savedWorlds }
          prepareNextRound savedWorld'
        Nothing ->
          case Seq.lookup 0 savedWorlds of
            Just savedWorld -> do
              let savedWorld' =
                    savedWorld { robotWorldRandom = robotWorldRandom world }
              prepareNextRound savedWorld'
            Nothing -> prepareNextRound world
  world <- setupWorld
  State.modify $ \cont -> cont { robotContWorld = Just world }

-- | Clean up after a world.
cleanupWorld :: RobotWorld -> RobotWorld
cleanupWorld world =
  world { robotWorldRobots =
            fmap (\robot -> robot { robotData = RobotNull })
                 (robotWorldRobots world) }

-- | Prepare the next round.
prepareNextRound :: RobotWorld -> State.State RobotCont ()
prepareNextRound world = do
  params <- robotContParams <$> State.get
  let reproduction = robotParamsReproduction params
      mutatedReproduction = robotParamsMutatedReproduction params
      sortedRobots = Seq.sortBy
        (\robot0 robot1 -> compare (exprSize (robotExpr robot1))
          (exprSize (robotExpr robot0))) (robotWorldRobots world)
      sortedRobots' = Seq.sortBy (\robot0 robot1 -> compare (robotScore robot1)
                                   (robotScore robot0)) sortedRobots
      programs = fmap robotExpr sortedRobots'
      totalNew = foldl' (+) 0 reproduction + foldl' (+) 0 mutatedReproduction
      reproduction' =
        if Seq.length reproduction < Seq.length mutatedReproduction
        then reproduction >< Seq.replicate (Seq.length mutatedReproduction -
                                             Seq.length reproduction) 0
        else reproduction
      mutatedReproduction' =
        if Seq.length mutatedReproduction < Seq.length reproduction
        then mutatedReproduction ><
             Seq.replicate (Seq.length reproduction -
                            Seq.length mutatedReproduction) 0
        else mutatedReproduction
      combinedReproduction = Seq.zip reproduction' mutatedReproduction'
      takeCount = Seq.length programs -
                  (totalNew - Seq.length combinedReproduction)
      (reproduced, nonReproduced) =
        Seq.splitAt (Seq.length reproduction) (Seq.take takeCount programs)
      (programs', gen) = foldl' (reproduce params)
                           (nonReproduced, robotWorldRandom world)
                           (Seq.zip reproduced combinedReproduction)
  State.modify $ \contState ->
                   contState { robotContPrograms = programs',
                               robotContRandom = robotWorldRandom world }

-- | Get the "size" of an expression.
exprSize :: RobotExpr -> Int
exprSize (RobotLoad _) = 1
exprSize (RobotConst value) = valueSize value
exprSize (RobotSpecialConst _) = 1
exprSize (RobotBind bindExprs expr) =
  1 + foldl' (\size boundExpr -> size + exprSize boundExpr) 0 bindExprs +
  exprSize expr
exprSize (RobotFunc _ expr) = 1 + exprSize expr
exprSize (RobotApply argExprs funcExpr) =
  1 + foldl' (\size argExpr -> size + exprSize argExpr) 0 argExprs +
  exprSize funcExpr
exprSize (RobotCond condExpr trueExpr falseExpr) =
  1 + exprSize condExpr + exprSize trueExpr + exprSize falseExpr

-- | Get the "size" of a value.
valueSize :: RobotValue -> Int
valueSize RobotNull = 1
valueSize (RobotBool _) = 1
valueSize (RobotInt _) = 1
valueSize (RobotFloat _) = 1
valueSize (RobotVector values) =
  1 + foldl' (\size value -> size + valueSize value) 0 values
valueSize (RobotClosure _ _ _) = 1
valueSize (RobotIntrinsic _) = 1
valueSize (RobotOutput value _) = 1 + valueSize value

-- | Reproduce a robot
reproduce :: RobotParams -> (Seq.Seq RobotExpr, Random.StdGen) ->
             (RobotExpr, (Int, Int)) -> (Seq.Seq RobotExpr, Random.StdGen)
reproduce params (programs, gen) (program, (unmutatedCount, mutatedCount)) =
  if mutatedCount > 0
  then let (newProgram, mutateState) =
             State.runState (mutate startingContextDepth 0 program)
               (RobotMutate { robotMutateRandom = gen,
                              robotMutateParams = params })
       in reproduce params
            (programs |> newProgram, robotMutateRandom mutateState)
            (program, (unmutatedCount, mutatedCount - 1))
  else (programs >< Seq.replicate unmutatedCount program, gen)

-- | Set up a world.
setupWorld :: State.State RobotCont RobotWorld
setupWorld = do
  params <- robotContParams <$> State.get
  programs <- robotContPrograms <$> State.get
  (robots, nextIndex) <-
    foldlM (\(robots, nextIndex) program -> do
               robot <- setupRobot program nextIndex
               return (robots |> robot, nextIndex + 1)) (Seq.empty, 0) programs
  gen <- robotContRandom <$> State.get
  return $ RobotWorld { robotWorldParams = params,
                        robotWorldRobots = robots,
                        robotWorldShots = Seq.empty,
                        robotWorldCycles = 0,
                        robotWorldKills = 0,
                        robotWorldNextRobotIndex = nextIndex,
                        robotWorldRandom = gen }

-- | Set up a robot.
setupRobot :: RobotExpr -> Int -> State.State RobotCont Robot
setupRobot program index = do
  params <- robotContParams <$> State.get
  gen <- robotContRandom <$> State.get
  let (robot, gen') = generateRobot index program 0.0 gen params
  State.modify $ \contState -> contState { robotContRandom = gen' }
  return robot
  
-- | Get a random value in [0, 1)
random :: Random.Random a => State.State RobotCont a
random = do
  (value, gen) <- Random.random <$> robotContRandom <$> State.get
  state $ \contState -> ((), contState { robotContRandom = gen })
  return value

-- | Get a random value in [a, b]
randomR :: Random.Random a => (a, a) -> State.State RobotCont a
randomR range = do
  (value, gen) <- Random.randomR range <$> robotContRandom <$> State.get
  state $ \contState -> ((), contState { robotContRandom = gen })
  return value
