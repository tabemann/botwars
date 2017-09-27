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

{-# LANGUAGE OverloadedStrings, OverloadedLabels, PatternSynonyms #-}

module Main (Main.main) where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.Utility
import Robots.Genetic.HunterKiller.Intrinsics
import Robots.Genetic.HunterKiller.Params
import Robots.Genetic.HunterKiller.Load
import Robots.Genetic.HunterKiller.Save
import Robots.Genetic.HunterKiller.Combat
import Robots.Genetic.HunterKiller.Render
import Control.Concurrent (forkIO,
                           forkOS,
                           threadDelay)
import Control.Concurrent.MVar (MVar,
                                newEmptyMVar,
                                putMVar,
                                takeMVar,
                                tryTakeMVar)
import System.Exit (exitWith,
                    exitFailure,
                    ExitCode(..))
import qualified Data.Sequence as Seq
import Data.Sequence ((><))
import qualified Data.Text as Text
import Data.Text.IO (hPutStr,
                     readFile)
import System.IO (stderr,
                  openFile,
                  hClose,
                  IOMode(..))
import System.Environment (getArgs,
                           getProgName)
import Control.Exception (catch,
                          IOException,
                          SomeException)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import GI.Cairo.Structs.Context (Context(..))
import qualified Graphics.Rendering.Cairo as Cairo
import Data.GI.Base
import Control.Monad.Trans.Reader (ReaderT(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Graphics.Rendering.Cairo.Internal (Render(..))
import Control.Monad.IO.Class (liftIO)
import Text.Printf (printf)
import Data.Functor ((<$>))
import Text.Read (readMaybe)
import Data.IORef (IORef,
                   newIORef,
                   readIORef,
                   writeIORef)
import System.Random (StdGen,
                      newStdGen)
import Data.Foldable (foldl',
                      toList)
import GI.GLib (idleAdd,
                pattern PRIORITY_DEFAULT,
                pattern PRIORITY_HIGH)
import GI.Gdk.Objects.Window
import qualified System.Clock as Clock
import Prelude hiding (hPutStr,
                       readFile)

-- | The main action.
main :: IO ()
main = do
  inputs <- getInputs
  case inputs of
    Right (exprs, params) -> setup exprs params
    Left errorText -> do
      hPutStr stderr errorText
      exitFailure

-- | Get inputs.
getInputs :: IO (Either Text.Text (Seq.Seq RobotExpr, RobotParams))
getInputs = do
  args <- getArgs
  case args of
    [worldPath, worldCopiesText, paramsPath] -> do
      paramsText <-
        catch (Right <$> readFile paramsPath)
              (\e -> return . Left . Text.pack $ show (e :: IOException))
      case paramsText of
        Right paramsText -> do
          worldText <-
            catch (Right <$> readFile worldPath)
                  (\e -> return . Left . Text.pack $ show (e :: IOException))
          case worldText of
            Right worldText ->
              let errorOrParams = loadParams paramsText in
              case errorOrParams of
                Right params ->
                  let errorOrExprs = loadWorld specialConstEntries worldText in
                  case errorOrExprs of
                    Right exprs ->
                      case readMaybe worldCopiesText of
                        Just worldCopies ->
                          if worldCopies >= 1
                          then return $ Right
                               (foldl' (><) Seq.empty $
                                 Seq.replicate worldCopies exprs,
                                 params)
                          else return $ Left
                               "number of copies must be greater than zero\n"
                        Nothing -> return $ Left "invalid number of copies\n"
                    Left errorText ->
                      return . Left . Text.pack $
                        printf "%s: unable to load world: %s\n"
                        worldPath errorText
                Left errorText ->
                  return . Left . Text.pack $
                    printf "%s: unable to load params: %s\n"
                    paramsPath errorText
            Left errorText ->
              return . Left . Text.pack $ printf "%s: %s\n" worldPath errorText
        Left errorText ->
          return . Left . Text.pack $ printf "%s: %s\n" paramsPath errorText
    _ -> do progName <- getProgName
            return . Left . Text.pack $
              printf "Usage: %s WORLD-FILE COUNT PARAMS-FILE\n" progName

-- | Set up the UI and prepare for running.
setup :: Seq.Seq RobotExpr -> RobotParams -> IO ()
setup exprs params = do
  controlMVar <- newEmptyMVar
  exitMVar <- newEmptyMVar
  Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setWindowTitle window "Botwars"
  Gtk.onWidgetDestroy window $ do
    Gtk.mainQuit
    putMVar controlMVar RobotExit
  vbox <- Gtk.boxNew Gtk.OrientationVertical 10
  Gtk.boxSetHomogeneous vbox False
  canvas <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest canvas 920 920
  worldRef <- newIORef Nothing
  Gtk.onWidgetDraw canvas $ \(Context fp) -> do
    withManagedPtr fp $ \p ->
      (`runReaderT` Cairo (castPtr p)) $ runRender $ do
      w <- liftIO $ fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
      h <- liftIO $ fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
      world <- liftIO $ readIORef worldRef
      case world of
        Just world -> drawWorld world w h
        Nothing -> return ()
      return True
  Gtk.boxPackStart vbox canvas True True 0
  buttonBox <- Gtk.buttonBoxNew Gtk.OrientationHorizontal
  Gtk.buttonBoxSetLayout buttonBox Gtk.ButtonBoxStyleCenter
  backwardButton <- Gtk.buttonNew
  stopButton <- Gtk.buttonNew
  startButton <- Gtk.buttonNew
  forwardButton <- Gtk.buttonNew
  saveButton <- Gtk.buttonNew
  Gtk.buttonSetLabel backwardButton "<<"
  Gtk.buttonSetLabel stopButton "Stop"
  Gtk.buttonSetLabel startButton "Start"
  Gtk.buttonSetLabel forwardButton ">>"
  Gtk.buttonSetLabel saveButton "Save"
  Gtk.onButtonClicked backwardButton $ putMVar controlMVar RobotBackward
  Gtk.onButtonClicked stopButton $ putMVar controlMVar RobotStop
  Gtk.onButtonClicked startButton $ putMVar controlMVar RobotStart
  Gtk.onButtonClicked forwardButton $ putMVar controlMVar RobotForward
  Gtk.onButtonClicked saveButton $ do
    fileChooser <- Gtk.fileChooserNativeNew (Just "Save As World")
      (Just window) Gtk.FileChooserActionSave Nothing
      Nothing
    result <- toEnum <$> fromIntegral <$> Gtk.nativeDialogRun fileChooser
    case result of
      Gtk.ResponseTypeAccept -> do
        filename <- Gtk.fileChooserGetFilename fileChooser
        case filename of
          Just filename -> putMVar controlMVar (RobotSave filename)
          Nothing -> return ()
      _ -> return ()
  Gtk.boxPackStart buttonBox backwardButton False False 0
  Gtk.boxPackStart buttonBox stopButton False False 0
  Gtk.boxPackStart buttonBox startButton False False 0
  Gtk.boxPackStart buttonBox forwardButton False False 0
  Gtk.boxPackStart buttonBox saveButton False False 0
  Gtk.boxPackEnd vbox buttonBox False False 0
  Gtk.containerAdd window vbox
  gen <- newStdGen
  Gtk.widgetShowAll window
  forkOS Gtk.main
  forkIO $ do
    time <- Clock.getTime Clock.Monotonic
    let play =
          RobotPlay { robotPlayCyclesPerSecond =
                      robotParamsMaxCyclesPerSecond params,
                      robotPlayRunning = False,
                      robotPlayReverse = False,
                      robotPlayIndex = 0,
                      robotPlayDoStep = RobotNoStep }
    mainLoop (initCont exprs params gen) canvas worldRef controlMVar
      exitMVar time play
  exitStatus <- takeMVar exitMVar
  exitWith exitStatus 

-- | Execute the main loop of the genetically-programmed robot fighting arena.
mainLoop :: RobotCont -> Gtk.DrawingArea -> IORef (Maybe RobotWorld) ->
            MVar RobotControl -> MVar ExitCode -> Clock.TimeSpec ->
            RobotPlay -> IO ()
mainLoop cont canvas worldRef controlMVar exitMVar nextTime play = do
  let params = robotContParams cont
  control <- tryTakeMVar controlMVar
  case control of
    Just RobotExit -> putMVar exitMVar ExitSuccess
    Just RobotStart ->
      let play' = play { robotPlayRunning = True }
      in mainLoop cont canvas worldRef controlMVar exitMVar nextTime play'
    Just RobotStop ->
      let play' = play { robotPlayRunning = False }
      in mainLoop cont canvas worldRef controlMVar exitMVar nextTime play'
    Just RobotForward ->
      let play' =
            if robotPlayRunning play
            then if robotPlayReverse play
                 then
                   let newCyclesPerSecond = robotPlayCyclesPerSecond play / 2.0
                   in if newCyclesPerSecond >=
                         robotParamsMaxCyclesPerSecond params / 32.0
                      then play { robotPlayCyclesPerSecond =
                                    newCyclesPerSecond }
                      else play { robotPlayReverse = False,
                                  robotPlayCyclesPerSecond =
                                    robotParamsMaxCyclesPerSecond params / 32.0
                                }
                 else
                   let newCyclesPerSecond = robotPlayCyclesPerSecond play * 2.0
                   in if newCyclesPerSecond <=
                         robotParamsMaxCyclesPerSecond params
                      then play { robotPlayCyclesPerSecond =
                                    newCyclesPerSecond }
                      else play { robotPlayCyclesPerSecond =
                                    robotParamsMaxCyclesPerSecond params }
            else play { robotPlayDoStep = RobotStepForward }
      in mainLoop cont canvas worldRef controlMVar exitMVar nextTime play'
    Just RobotBackward ->
      let play' =
            if robotPlayRunning play
            then if robotPlayReverse play
                 then
                   let newCyclesPerSecond = robotPlayCyclesPerSecond play * 2.0
                   in if newCyclesPerSecond <=
                         robotParamsMaxCyclesPerSecond params
                      then play { robotPlayCyclesPerSecond =
                                    newCyclesPerSecond }
                      else play { robotPlayCyclesPerSecond =
                                    robotParamsMaxCyclesPerSecond params }
                 else
                   let newCyclesPerSecond = robotPlayCyclesPerSecond play / 2.0
                   in if newCyclesPerSecond >=
                         robotParamsMaxCyclesPerSecond params / 32.0
                      then play { robotPlayCyclesPerSecond =
                                    newCyclesPerSecond }
                      else play { robotPlayReverse = True,
                                  robotPlayCyclesPerSecond =
                                    robotParamsMaxCyclesPerSecond params / 32.0
                                }
            else play { robotPlayDoStep = RobotStepBackward }
      in mainLoop cont canvas worldRef controlMVar exitMVar nextTime play'
    Just (RobotSave path) -> do
      case robotContWorld cont of
        Just world -> do
          message <- saveWorldToFile path world
          case message of
            Left errorText -> hPutStr stderr errorText
            Right () -> return ()
        Nothing -> return ()
      mainLoop cont canvas worldRef controlMVar exitMVar nextTime play
    Nothing -> do
      (cont', world, play) <- nextState cont play
      writeIORef worldRef (Just world)
      Gdk.threadsAddIdle PRIORITY_HIGH $ do
        window <- #getWindow canvas
        case window of
          Just window -> #invalidateRect window Nothing True
          Nothing -> return ()
        return False
      let robotDisplay =
            Text.concat
            (toList (fmap (\robot ->
                             Text.pack $ printf "%d " (robotIndex robot))
                      (robotWorldRobots world)))
          shotDisplay =
            Text.concat
            (toList (fmap (\shot ->
                             Text.pack $ printf "%d " (shotRobotIndex shot))
                      (robotWorldShots world)))
      putStr $ printf "Robots: %sShots: %s\n" robotDisplay shotDisplay
      time <- Clock.getTime Clock.Monotonic
      let cyclesPerSecond =
            if robotPlayRunning play
            then robotPlayCyclesPerSecond play
            else robotParamsMaxCyclesPerSecond params
          maxDelay =
            Clock.fromNanoSecs . floor $ 1000000000.0 / cyclesPerSecond
          nextTime' = nextTime + maxDelay
      if time < nextTime'
        then threadDelay . fromIntegral $
             (Clock.toNanoSecs (nextTime' - time)) `div` 1000
        else return ()
      let nextTime'' =
            if time - nextTime > (Clock.fromNanoSecs . floor $
                                  2000000000.0 / cyclesPerSecond)
            then time
            else nextTime'
      mainLoop cont' canvas worldRef controlMVar exitMVar nextTime'' play

-- | Get a new continuity, world, and play control state.
nextState :: RobotCont -> RobotPlay -> IO (RobotCont, RobotWorld, RobotPlay)
nextState cont play =
  if robotPlayRunning play
  then if not $ robotPlayReverse play
       then if robotPlayIndex play >= -1
            then do
              let (event, cont') = executeCycle cont
                  play' = play { robotPlayIndex = 0 }
              world <- case event of
                RobotWorldCycle world -> return world
                RobotRoundDone world -> do
                  saveWorldToFile "backup.world" world >> return ()
                  return world
              return (cont', world, play')
            else
              let index = robotPlayIndex play + 1
                  prevWorlds = robotContPrevWorlds cont
                  world =
                    case Seq.lookup (Seq.length prevWorlds + index)
                         prevWorlds of
                      Just world -> world
                      Nothing -> error "impossible"
                  play' = play { robotPlayIndex = index }
              in return (cont, world, play')
       else
         let prevWorlds = robotContPrevWorlds cont in
           if robotPlayIndex play >= -(Seq.length prevWorlds - 1)
           then
             let index = robotPlayIndex play - 1
                 world =
                   case Seq.lookup (Seq.length prevWorlds + index)
                        prevWorlds of
                     Just world -> world
                     Nothing -> error "impossible"
                 play' = play { robotPlayIndex = index }
             in return (cont, world, play')
           else
             let play' = play { robotPlayRunning = False,
                                robotPlayReverse = False }
                 world =
                   case Seq.lookup (Seq.length prevWorlds +
                                     robotPlayIndex play') prevWorlds of
                     Just world -> world
                     Nothing -> error "impossible"
             in return (cont, world, play')
  else
    case robotPlayDoStep play of
      RobotStepForward ->
        if robotPlayIndex play >= -1
        then do
          let (event, cont') = executeCycle cont
              play' = play { robotPlayIndex = 0,
                             robotPlayDoStep = RobotNoStep }
          world <- case event of
            RobotWorldCycle world -> return world
            RobotRoundDone world -> do
              saveWorldToFile "backup.world" world >> return ()
              return world
          return (cont', world, play')
        else
          let index = robotPlayIndex play + 1
              prevWorlds = robotContPrevWorlds cont
              world =
                case Seq.lookup (Seq.length prevWorlds + index)
                     prevWorlds of
                  Just world -> world
                  Nothing -> error "impossible"
              play' = play { robotPlayIndex = index,
                             robotPlayDoStep = RobotNoStep }
          in return (cont, world, play')
      RobotStepBackward ->
        let prevWorlds = robotContPrevWorlds cont in
          if robotPlayIndex play >= -(Seq.length prevWorlds - 1)
          then
            let index = robotPlayIndex play - 1
                world =
                  case Seq.lookup (Seq.length prevWorlds + index)
                       prevWorlds of
                    Just world -> world
                    Nothing -> error "impossible"
                play' = play { robotPlayIndex = index,
                               robotPlayDoStep = RobotNoStep }
            in return (cont, world, play')
          else
            let play' = play { robotPlayDoStep = RobotNoStep } in
            if robotPlayIndex play' < 0 then
              let world =
                    case Seq.lookup (Seq.length prevWorlds +
                                      robotPlayIndex play') prevWorlds of
                      Just world -> world
                      Nothing -> error "impossible"
              in return (cont, world, play)
            else
              case robotContWorld cont of
                Just world -> return (cont, world, play')
                Nothing -> do
                  let (event, cont') = executeCycle cont
                      play'' = play' { robotPlayIndex = 0 }
                  world <- case event of
                    RobotWorldCycle world -> return world
                    RobotRoundDone world -> do
                      saveWorldToFile "backup.world" world >> return ()
                      return world
                  return (cont', world, play'')
      RobotNoStep ->
        if robotPlayIndex play >= 0
        then
          case robotContWorld cont of
            Just world -> return (cont, world, play)
            Nothing -> do
              let (event, cont') = executeCycle cont
              world <- case event of
                RobotWorldCycle world -> return world
                RobotRoundDone world -> do
                  saveWorldToFile "backup.world" world >> return ()
                  return world
              return (cont', world, play)
        else do
          let prevWorlds = robotContPrevWorlds cont
              world =
                case Seq.lookup (Seq.length prevWorlds + robotPlayIndex play)
                     prevWorlds of
                  Just world -> world
                  Nothing -> error "impossible"
          return (cont, world, play)

-- | Save a world.
saveWorldToFile :: FilePath -> RobotWorld -> IO (Either Text.Text ())
saveWorldToFile path world = do
  let worldText =
        saveWorld specialConstEntries (fmap robotExpr (robotWorldRobots world))
  saveFile <- catch (Right <$> openFile path WriteMode)
              (\e -> return . Left . Text.pack $ show (e :: IOException))
  case saveFile of
    Right saveFile -> do
      hPutStr saveFile worldText
      hClose saveFile
      return $ Right ()
    Left errorText -> do
      return $ Left errorText
