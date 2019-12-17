{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception (IOException, catch)
import Control.Monad (replicateM, unless, when)
import Data.List (intercalate)
import Database.HDBC.PostgreSQL (Connection)
import Database.HDBC (fromSql, quickQuery)
import Network.Socket (withSocketsDo)
import Options.Generic (getRecord)
import System.Directory (setCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (splitFileName)
import System.IO (openFile, IOMode(WriteMode))

import DirParser (parseGMap)
import DB (initdb, noteErrorOnHost, numToProc, popItem, queueItems, updateItem, withdb)
import Lock (withLock)
import NetClient (dlItem)
import RobotsTxt (parseRobots, isURLAllowed)
import Types (GAddress(..), State(..), Env(..), envFromConfig)
import Utils (msg, getFSPath)

{- | Main entry point for the program. -}
main :: IO ()
main = withSocketsDo $ do         -- Prepare things for sockets
  env <- envFromConfig =<< getRecord "gopherbot"
  setCurrentDirectory (baseDir env)
  withdb env $ \c -> do
    initdb c
    n <- numToProc c
    msg $ show n ++ " items to process"
    -- Nothing to do: prime the db
    when (n == 0) (mapM_ (\g -> updateItem env c g NotVisited "") (startingAddresses env))

  {- Fork off the childthreads.  Each one goes into a loop
     of waiting for new items to process and processing them. -}
  children <- replicateM (numThreads env) (myForkOS $ withdb env (procLoop env))
  -- This is the thread that displays status updates every so often
  _ <- forkOS (statsthread env)
  -- When the main thread exits, so does the program, so
  -- we wait for all children before exiting.
  mapM_ takeMVar children

{- | A simple wrapper around forkOS to notify the main thread when each
individual thread dies. -}
myForkOS :: IO () -> IO (MVar ())
myForkOS io = do
  mvar <- newEmptyMVar
  _ <- forkFinally (msg "started." >> io) (\_ -> putMVar mvar ())
  pure mvar

{- | Main worker loop.  We receive an item and process it.  If it's
Nothing, there is nothing else to do, so the thread shuts down.
Otherwise, call procItem, pop the next, and then call itself. -}
procLoop :: Env -> Connection -> IO ()
procLoop env c =
  popItem env c >>= \case
    Nothing -> msg "No items left, exiting"
    Just item -> do
      procItem env c item
      procLoop env c

{- | What happened when we checked the robots.txt file? -}
data RobotStatus
  = RobotsOK -- ^ Proceed
  | RobotsDeny -- ^ Do not download this file
  | RobotsError -- ^ Error occured; abort.

{- | Given a 'GAddress' (corresponding to a single item),
check to see if it's OK to download according to robots.txt.
-}
checkRobots :: Env -> Connection -> GAddress -> IO RobotStatus
checkRobots env c ga = do
  let garobots = ga {path = "robots.txt", dtype = '0'}
  fspath <- getFSPath (baseDir env) garobots
  dfe <- doesFileExist fspath
  unless dfe (procItem env c garobots) -- Download file if needed
  dfe2 <- doesFileExist fspath -- Do we have it yet?
  if dfe2
    then do -- Yes.  Parse it, and see what happened.
      r <- parseRobots fspath
      pure $ if isURLAllowed r "gopherbot" (path ga) then RobotsOK else RobotsDeny
    else pure RobotsError -- No.  TCP error occured.

{- | Run an IO action, but only if it's OK according to robots.txt. -}
procIfRobotsOK :: Env -> Connection -> GAddress -> IO () -> IO ()
procIfRobotsOK env c item action = do
  r <- if path item /= "robots.txt"
      then checkRobots env c item
      else pure RobotsOK
  case r of
    RobotsOK -> action
    RobotsDeny -> do
      msg $ "Excluded by robots.txt: " ++ show item
      updateItem env c item Excluded ""
    RobotsError -> msg $ "Problem getting robots.txt: " ++ host item

-- TODO: better crash handling on robots.txt

{- | OK, we have an item.  If it's OK according to robots.txt, download
and process it. -}
procItem :: Env -> Connection -> GAddress -> IO ()
procItem env c item = procIfRobotsOK env c item $ do
  msg (show item)
  fspath <- getFSPath (baseDir env) item

  -- Create the directory for the file to go in, if necessary.
  catch (withLock (lock env) (createDirectoryIfMissing True (fst $ splitFileName fspath)))
        (\(e :: IOException) -> do
            msg $ "Single-Item Error on " ++ show item ++ ": " ++ show e
            updateItem env c item ErrorState (show e))

  fh <- catch (Just <$> openFile fspath WriteMode)
        (\(e :: IOException) -> do
            msg $ "Single-item error on " ++ show item ++ ": " ++ show e
            updateItem env c item ErrorState (show e)
            pure Nothing)

  case fh of
    Nothing -> pure ()
    Just h -> catch (do dlItem item h
                        when (dtype item == '1') (spider env c fspath)
                        updateItem env c item Visited "")
                    (\(e :: IOException) -> do
                        msg $ "Error on " ++ show item ++ ": " ++ show e
                        noteErrorOnHost env c (host item) (show e))

{- | This function is called by procItem whenever it downloads a
menu.  This function calles the parser, extracts items, and calles
DB.queueItems to handle them.  (Insert into DB if new) -}
spider :: Env -> Connection -> FilePath -> IO ()
spider env c fspath =
  queueItems env c . filter shouldProcess =<< parseGMap fspath
  where
    shouldProcess a =
      (dtype a `notElem` ['i', '3', '8', '7', '2']) &&
      (host a `notElem` excludeServers env)

{- | This thread prints a periodic status update. -}
statsthread :: Env -> IO ()
statsthread env = do
  withdb env $ \conn -> do
    res <- quickQuery conn "SELECT state, COUNT(*) from files group by state order by state" []
    let counts = map (\[s, c] -> (fromSql s, fromSql c :: Integer)) res
    let total = sum (map snd counts)
    let totaltext = "Total " ++ show total
    let statetxts = map (\(h, c) -> h ++ " " ++ show c) counts
    msg $ intercalate ", " (totaltext : statetxts)
  threadDelay (120 * 1000000)
  statsthread env
