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

module Robots.Genetic.HunterKiller.Combat

where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.World
import Robots.Genetic.HunterKiller.Mutate
import Control.Monad.State.Strict as State
import Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Functor ((<$>))
import System.Random as Random
import Control.Monad.IO.Class as MonadIO
import Control.Monad.Trans.Class (MonadTrans,
                                  lift)
import Data.Foldable (foldlM,
                      foldl')

-- | Execute robot combat.
combat :: Monad m => (RobotEvent -> m ()) -> RobotExpr -> Int -> RobotParams -> 
          Random.StdGen -> m Random.StdGen
combat func program count params gen =
  if count >= 1
  then robotContRandom <$> State.execStateT (combat' (count - 1))
         (RobotCont { robotContParams = params,
                      robotContRandom = gen,
                      robotContPrograms = Seq.singleton program,
                      robotContEventHandler = func })
  else error "count smaller than 1"
  
-- | Actually set up and execute robot combat.
combat' :: Monad m => Int -> State.StateT (RobotCont m) m ()
combat' count =
  if count > 0
  then do
    programs <- robotContPrograms <$> State.get
    gen <- robotContRandom <$> State.get
    params <- randomContParams <$> State.get
    case Seq.lookup 0 programs of
      Just program ->
        let (newProgram, mutateState) =
              State.runState (mutate startingContextDepth program)
                (RobotMutate { robotMutateRandom = gen,
                               robotMutateParams = params })
        in do state $ \cont -> cont { randomContRandom =
                                        robotMutateRandom mutateState,
                                      randomContPrograms =
                                        randomContPrograms cont |> newProgram }
              combat' $ count - 1
      Nothing -> error "no programs available"
  else executeRounds

-- | Execute rounds of combat.
executeRounds :: Monad m => State.StateT (RobotCont m) m ()
executeRounds = do
  world <- setupWorld
  eventHandler <- robotContEventHandler <$> State.get
  input <- lift . eventHandler $ RobotNewRound world
  case input of
    RobotContinue ->
      (input, world') <- executeCycles world
      case input of
        RobotContinue -> do
          input <- lift . eventHandler $ RobotRoundDone world'
          case input of
            RobotContinue -> do
              prepareNextRound world'
              executeRounds
            RobotExit -> return ()
        RobotExit -> return ()
    RobotExit -> return ()

-- | Execute cycles of combat.
executeCycles :: Monad m => RobotWorld ->
                 State.StateT (RobotCont m) m (RobotInput, RobotWorld)
executeCycles world = do
  eventHandler <- robotContEventHandler
  input <- lift . eventHandler $ RobotWorldCycle world
  case input of
    RobotContinue ->
      let (cycleState, world') = State.runState worldCycle world
      in case cycleState of
           RobotNextCycle -> executeCycles world'
           RobotEndRound -> return (RobotContinue, world')
    RobotExit -> return (RobotExit, world)

-- | Prepare the next round.
prepareNextRound :: RobotWorld -> State.StateT (RobotCont m) m ()
prepareNextRound world = do
  params <- robotContParams <$> State.get
  let reproduction = robotParamsReproduction params
      sortedRobots = Seq.sortBy (\robot0 robot1 -> compare (robotScore robot1)
                                  (robotScore robot0)) (robotWorldRobots world)
      totalNew = foldl' (+) reproduction
      sortedRobots' = Seq.take (Seq.length sortedRobots - totalNew) sortedRobots
      programs = foldl' robotExpr sortedRobots
      (programs', gen) = foldl' (reproduce params)
                           (programs, robotWorldRandom world)
                           (Seq.zip programs reproduction)
  State.modify $ \contState ->
                   contState { robotContPrograms = programs',
                               robotContRandom = robotWorldRandom world }

-- | Reproduce a robot
reproduce :: RobotParams -> (Seq.Seq RobotExpr, Random.StdGen) ->
             (RobotExpr, Int) -> (Seq.Seq RobotExpr, Random.StdGen)
reproduce params (programs, gen) (program, count) =
  if count > 0
  then let (newProgram, mutateState) =
             State.runState (mutate startingContextDepth program)
               (RobotMutate { robotMutateRandom = gen,
                              robotMutateParams = params })
       in reproduce params
            (programs |> newProgram, robotMutateRandom mutateState)
            (program, count - 1)
  else (programs, gen)

-- | Set up a world.
setupWorld :: State.StateT (RobotCont m) m RobotWorld
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
setupRobot :: RobotExpr -> Int -> State.StateT (RobotCont m) m Robot
setupRobot program index = do
  params <- robotContParams <$> State.get
  gen <- robotContRandom <$> State.get
  let (robot, gen') = generateRobot index program 0 gen params
  State.modify $ \contState -> contState { robotContRandom = gen' }
  return robot
  
-- | Get a random value in [0, 1)
random :: Random.Random a => State.StateT (RobotCont m) m a
random = do
  (value, gen) <- Random.random <$> robotContRandom <$> State.get
  state $ \contState -> contState { robotContRandom = gen }
  return value

-- | Get a random value in [a, b]
randomR :: Random.Random a => (a, a) -> State.StateT (RobotCont m) m a
randomR range = do
  (value, gen) <- Random.randomR range <$> robotContRandom <$> State.get
  state $ \contState -> contState { robotContRandom = gen }
  return value
