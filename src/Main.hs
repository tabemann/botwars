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
import qualified Data.Text.IO as TextIO
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

-- | The main action.
main :: IO ()
main = do
  inputs <- getInputs
  case inputs of
    Right (exprs, params, savePath) -> setup exprs params savePath
    Left errorText -> do
      TextIO.hPutStr stderr errorText
      exitFailure

-- | Get inputs.
getInputs :: IO (Either Text.Text (Seq.Seq RobotExpr, RobotParams, FilePath))
getInputs = do
  args <- getArgs
  case args of
    [worldPath, worldCopiesText, paramsPath, savePath] -> do
      paramsText <-
        catch (Right <$> TextIO.readFile paramsPath)
              (\e -> return . Left . Text.pack $ show (e :: IOException))
      case paramsText of
        Right paramsText -> do
          worldText <-
            catch (Right <$> TextIO.readFile worldPath)
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
                                 params, savePath)
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
              printf "Usage: %s WORLD-FILE COUNT PARAMS-FILE SAVE-FILE\n" progName

-- | Set up the UI and prepare for running.
setup :: Seq.Seq RobotExpr -> RobotParams -> FilePath -> IO ()
setup exprs params savePath = do
  exitMVar <- newEmptyMVar
  reallyExitMVar <- newEmptyMVar
  Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setWindowTitle window "Botwars"
  Gtk.onWidgetDestroy window $ do
    Gtk.mainQuit
    putMVar exitMVar ExitSuccess
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
  Gtk.containerAdd window canvas
  gen <- newStdGen
  Gtk.widgetShowAll window
  forkOS Gtk.main
  forkIO $ do
    time <- Clock.getTime Clock.Monotonic
    mainLoop (initCont exprs params gen) savePath canvas worldRef exitMVar
      reallyExitMVar time
  exitStatus <- takeMVar reallyExitMVar
  exitWith exitStatus 

-- | Execute the main loop of the genetically-programmed robot fighting arena.
mainLoop :: RobotCont -> FilePath -> Gtk.DrawingArea ->
            IORef (Maybe RobotWorld) -> MVar ExitCode -> MVar ExitCode ->
            Clock.TimeSpec -> IO ()
mainLoop cont savePath canvas worldRef exitMVar reallyExitMVar nextTime = do
  let (event, cont') = executeCycle cont
      world = case event of
        RobotWorldCycle world -> world
        RobotRoundDone world -> world
  exitStatus <- tryTakeMVar exitMVar
  case exitStatus of
    Nothing -> do
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
      case event of
        RobotRoundDone _ ->
          saveWorldToFile (savePath ++ ".prev") world >> return ()
        _ -> return ()
      time <- Clock.getTime Clock.Monotonic
      let maxCyclesPerSecond =
            robotParamsMaxCyclesPerSecond $ robotWorldParams world
          maxDelay =
            Clock.fromNanoSecs . floor $ 1000000000.0 / maxCyclesPerSecond
          nextTime' = nextTime + maxDelay
      if time < nextTime'
        then threadDelay . fromIntegral $
             (Clock.toNanoSecs (nextTime' - time)) `div` 1000
        else return ()
      let nextTime'' =
            if time - nextTime > (Clock.fromNanoSecs . floor $
                                  2000000000.0 / maxCyclesPerSecond)
            then time
            else nextTime'
      mainLoop cont' savePath canvas worldRef exitMVar reallyExitMVar nextTime''
    Just exitStatus -> do
      status <- saveWorldToFile savePath world
      case status of
        Right () -> return ()
        Left errorText -> do
          TextIO.hPutStr stderr errorText
          exitFailure
      putMVar reallyExitMVar exitStatus

-- | Save a world.
saveWorldToFile :: FilePath -> RobotWorld -> IO (Either Text.Text ())
saveWorldToFile path world = do
  let worldText =
        saveWorld specialConstEntries (fmap robotExpr (robotWorldRobots world))
  saveFile <- catch (Right <$> openFile path WriteMode)
              (\e -> return . Left . Text.pack $ show (e :: IOException))
  case saveFile of
    Right saveFile -> do
      TextIO.hPutStr saveFile worldText
      hClose saveFile
      return $ Right ()
    Left errorText -> do
      return $ Left errorText
