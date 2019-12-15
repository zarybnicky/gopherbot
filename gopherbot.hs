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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception (IOException, bracket, catch, finally)
import Control.Monad (replicateM, unless, when)
import Data.List (intercalate)
import Database.HDBC.PostgreSQL (Connection)
import Database.HDBC (disconnect, fromSql, quickQuery)
import Network.Socket (withSocketsDo)
import System.Directory (setCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath (splitFileName)
import System.IO (openFile, IOMode(WriteMode))
import qualified Data.Map as Map

import Config
import DirParser (parseGMap)
import DB
import NetClient (dlItem)
import RobotsTxt (parseRobots, isURLAllowed)
import Types
import Utils (msg, newLock, withLock, getFSPath)

{- | Main entry point for the program. -}
main :: IO ()
main = withSocketsDo $ do         -- Prepare things for sockets
  setCurrentDirectory baseDir -- chdir to the working dir
  l <- newLock             -- Global lock for db updates
  initdb                   -- Initialize the database and get a conn
  gasupply <- newMVar Map.empty -- Global MVar for current status
  runScan gasupply l       -- main scanner

{- | Set up all the threads and get them going. -}
runScan :: GASupply -> Lock -> IO ()
runScan gasupply l = do
  c <- dbconnect
  n <- numToProc c
  msg $ show n ++ " items to process"
  when (n == 0)            -- Nothing to do: prime the db
     (mapM_ (\g -> updateItem l c g NotVisited "") startingAddresses)
  {- Fork off the childthreads.  Each one goes into a loop
     of waiting for new items to process and processing them. -}
  disconnect c
  children <- replicateM numThreads (myForkOS (procLoop l gasupply))
  -- This is the thread that displays status updates every so often
  --stats <- forkOS (statsthread l)
  -- When the main thread exits, so does the program, so
  -- we wait for all children before exiting.
  waitForChildren children

{- | A simple wrapper around forkOS to notify the main thread when each
individual thread dies. -}
myForkOS :: IO () -> IO (MVar ThreadId)
myForkOS io = do
  mvar <- newEmptyMVar
  _ <- forkIO ((msg "started." >> io) `finally` (myThreadId >>= putMVar mvar))
  return mvar

{- | Wait for child threads to die.
This should only happen when there is nothing else to spider. -}
waitForChildren :: [MVar ThreadId] -> IO ()
waitForChildren [] = msg "All children died; exiting."
waitForChildren (c:xs) = do
  t <- takeMVar c
  msg $ " *********** Thread died: " ++ show t
  waitForChildren xs

{- | Main entry point for each worker thread.  We just pop the first item,
then call procLoop'. -}
procLoop :: Lock -> GASupply -> IO ()
procLoop lock gasupply = bracket dbconnect disconnect (procLoop' lock gasupply)


{- | Main worker loop.  We receive an item and process it.  If it's
Nothing, there is nothing else to do, so the thread shuts down.
Otherwise, call procItem, pop the next, and then call itself. -}
procLoop' :: Lock -> GASupply -> Connection -> IO ()
procLoop' lock gasupply c = do
  mi <- popItem lock gasupply c
  case mi of
    Nothing -> msg "Exiting"
    Just item -> do
      procItem lock gasupply c item
      -- Popping the next item before releasing the current
      -- host is a simple form of being nice to remotes
      procLoop' lock gasupply c

{- | What happened when we checked the robots.txt file? -}
data RobotStatus
  = RobotsOK -- ^ Proceed
  | RobotsDeny -- ^ Do not download this file
  | RobotsError -- ^ Error occured; abort.

{- | Given a 'GAddress' (corresponding to a single item),
check to see if it's OK to download according to robots.txt.
-}
checkRobots :: Lock -> GASupply -> Connection -> GAddress -> IO RobotStatus
checkRobots lock gasupply c ga = do
  let fspath = getFSPath garobots
  dfe <- doesFileExist fspath
  unless dfe (procItem lock gasupply c garobots) -- Download file if needed
  dfe2 <- doesFileExist fspath -- Do we have it yet?
  if dfe2
    then do -- Yes.  Parse it, and see what happened.
      r <- parseRobots fspath
      return $ if isURLAllowed r "gopherbot" (path ga) then RobotsOK else RobotsDeny
    else return RobotsError -- No.  TCP error occured.
  where
    garobots = ga {path = "robots.txt", dtype = '0'}

{- | Run an IO action, but only if it's OK according to robots.txt. -}
procIfRobotsOK :: Lock -> GASupply -> Connection -> GAddress -> IO () -> IO ()
procIfRobotsOK lock gasupply c item action = do
  r <- if path item /= "robots.txt"
      then checkRobots lock gasupply c item
      else return RobotsOK
  case r of
    RobotsOK -> action
    RobotsDeny -> do
      msg $ "Excluded by robots.txt: " ++ show item
      updateItem lock c item Excluded ""
    RobotsError -> msg $ "Problem getting robots.txt: " ++ host item

-- TODO: better crash handling on robots.txt

{- | OK, we have an item.  If it's OK according to robots.txt, download
and process it. -}
procItem :: Lock -> GASupply -> Connection -> GAddress -> IO ()
procItem lock gasupply c item = procIfRobotsOK lock gasupply c item $ do
  msg $ show item          -- Show what we're up to
  let fspath = getFSPath item

  -- Create the directory for the file to go in, if necessary.
  catch (withLock lock (createDirectoryIfMissing True (fst $ splitFileName fspath)))
        (\(e :: IOException) -> do
            msg $ "Single-Item Error on " ++ show item ++ ": " ++ show e
            updateItem lock c item ErrorState (show e))

  fh <- catch (Just <$> openFile fspath WriteMode)
        (\(e :: IOException) -> do
            msg $ "Single-item error on " ++ show item ++ ": " ++ show e
            updateItem lock c item ErrorState (show e)
            return Nothing)

  case fh of
    Nothing -> return ()
    Just h -> -- Now, download it.  If it's a menu
              --(item type 1), check it for links
              -- (spider it).  Error here means a TCP
              -- problem, so mark every
              -- item on this host as having the error.
              catch (do dlItem item h
                        when (dtype item == '1') (spider lock c fspath)
                        updateItem lock c item Visited "")
                    (\(e :: IOException) -> do
                        msg $ "Error on " ++ show item ++ ": " ++ show e
                        noteErrorOnHost lock gasupply c (host item) (show e))

{- | This function is called by procItem whenever it downloads a
menu.  This function calles the parser, extracts items, and calles
DB.queueItems to handle them.  (Insert into DB if new) -}
spider :: Lock -> Connection -> FilePath -> IO ()
spider l c fspath = do
  netreferences <- parseGMap fspath
  queueItems l c (filter isMenu netreferences)
  where
    isMenu a = (dtype a `notElem` ['i', '3', '8', '7', '2']) &&
               (host a `notElem` excludeServers)

{- | This thread prints a periodic status update. -}
statsthread :: Lock -> IO ()
statsthread l = do
  c <- dbconnect
  statsthread' l c
  disconnect c

statsthread' :: Lock -> Connection -> IO ()
statsthread' l conn = do
  res <- quickQuery conn "SELECT state, COUNT(*) from files group by state order by state" []
  let counts = map (\[s, c] -> (fromSql s, fromSql c :: Integer)) res
  let total = sum (map snd counts)
  let totaltext = "Total " ++ show total
  let statetxts = map (\(h, c) -> h ++ " " ++ show c) counts
  msg $ intercalate ", " (totaltext : statetxts)
  threadDelay (120 * 1000000)
  statsthread' l conn
