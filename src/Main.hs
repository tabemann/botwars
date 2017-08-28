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
import Control.Concurrent.MVar (newEmptyMVar,
                                putMVar,
                                takeMVar)
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
                          IOException)
import qualified GI.Gtk as Gtk
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
import System.Random (newStdGen)
import Data.Foldable (foldl')
import GI.GLib (idleAdd,
                pattern PRIORITY_DEFAULT)

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
  exit <- newEmptyMVar

  Gtk.init Nothing

  window <- new Gtk.Window [ #title := "Botwars",
                             #borderWidth := 10 ]

  on window #destroy $ putMVar exit ExitSuccess

  canvas <- new Gtk.DrawingArea []

  #setSizeRequest canvas 512 512

  worldRef <- newIORef Nothing
  
  on canvas #draw $ \(Context fp) -> do
    withManagedPtr fp $ \p ->
      (`runReaderT` Cairo (castPtr p)) $ runRender $ do
      liftIO $ putStr "rendering\n"
      w <- liftIO $ fromIntegral <$> Gtk.widgetGetAllocatedWidth canvas
      h <- liftIO $ fromIntegral <$> Gtk.widgetGetAllocatedHeight canvas
      world <- liftIO $ readIORef worldRef
      case world of
        Just world -> drawWorld world w h
        Nothing -> return ()
      return True

  gen <- newStdGen
  
  forkIO $ do
    _ <- combat (handleRobotEvent canvas worldRef) exprs params gen
    return ()

  #showAll window

  forkOS Gtk.main

  exitStatus <- takeMVar exit

  world <- readIORef worldRef
  case world of
    Just world -> do
      let worldText = saveWorld specialConstEntries (fmap robotExpr (robotWorldRobots world))
      saveFile <- catch (Right <$> openFile savePath WriteMode)
                  (\e -> return . Left . Text.pack $ show (e :: IOException))
      case saveFile of
        Right saveFile -> do
          TextIO.hPutStr saveFile worldText
          hClose saveFile
        Left errorText -> do
          TextIO.hPutStr stderr errorText
          exitFailure
    Nothing -> return ()

  exitWith exitStatus

-- | Handle a robot event.
handleRobotEvent :: Gtk.DrawingArea -> IORef (Maybe RobotWorld) ->
                    RobotEvent -> IO RobotInput
handleRobotEvent canvas worldRef (RobotNewRound world) = do
  writeIORef worldRef (Just world)
  idleAdd PRIORITY_DEFAULT $ #queueDraw canvas >> return False
  return RobotContinue
handleRobotEvent canvas worldRef (RobotWorldCycle world) = do
  writeIORef worldRef (Just world)
  idleAdd PRIORITY_DEFAULT $ #queueDraw canvas >> return False
  threadDelay 10
  return RobotContinue
handleRobotEvent canvas worldRef (RobotRoundDone world) = do
  writeIORef worldRef (Just world)
  idleAdd PRIORITY_DEFAULT $ #queueDraw canvas >> return False
  return RobotContinue
