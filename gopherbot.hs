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

module Main where

import Config
import Control.Monad(when)
import Control.Exception(finally)
import System.Directory
import DB
import Database.HSQL
import Utils
import MissingH.Path.FilePath
import MissingH.Network
import NetClient
import DirParser
import Control.Concurrent
import Data.List
import Control.Exception(bracket_)

main = niceSocketsDo $
    do setCurrentDirectory baseDir
       l <- newLock
       c <- initdb
       runScan l c
       disconnect c

runScan l c =
    do n <- numToProc c
       msg $ (show n) ++ " items to process"
       if n == 0
          then do mapM_ (\g -> updateItem l c g NotVisited) startingAddresses
          else return ()
       hl <- newHostList
       children <- mapM (\_ -> myForkIO (procLoop l c hl)) [1..numThreads]
       stats <- forkIO (statsthread l c hl)
       waitForChildren children
       

myForkIO :: IO () -> IO (MVar ())
myForkIO io =
    do mvar <- newEmptyMVar
       forkIO (action `finally` putMVar mvar ())
       return mvar
    where action = do t <- myThreadId
                      msg "started."
                      io

waitForChildren :: [MVar ()] -> IO ()
waitForChildren [] = return ()
waitForChildren (c:xs) =
    do takeMVar c
       waitForChildren xs

procLoop lock c hl =
    do i <- popItem lock c hl
       procLoop' lock c hl i

procLoop' lock c hl i =
    do case i of
         Nothing -> msg $ "Exiting"
         Just item -> do procItem lock c item
                         -- Popping the next item before releasing the current
                         -- host is a simple form of being nice to remotes
                         i <- popItem lock c hl
                         delHost hl (host item)
                         procLoop' lock c hl i
    
procItem lock c item =
    do t <- myThreadId
       msg $ show item
       let fspath = getFSPath item
       catch (bracket_ (acquire lock) (release lock) (createDirectoryIfMissing True (fst . splitFileName $ fspath)))
             (\e -> do msg $ "Single-Item Error on " ++ (show item) ++ ": " 
                           ++ (show e)
                       updateItem lock c item ErrorState
             )
       catch (do dlItem item fspath
                 when (dtype item == '1') (spider lock c fspath)
                 updateItem lock c item Visited
             )
          (\e -> do msg $ "Error on " ++ (show item) ++ ": " ++ (show e)
                    noteErrorOnHost lock c (host item)
          )

spider l c fspath =
    do netreferences <- parseGMap fspath
       let refs = filter filt netreferences
       queueItems l c refs
    where filt a = (not (dtype a) `elem` ['i', '3', '8', '7', '2']) &&
                   not (host a `elem` excludeServers)

statsthread :: Lock -> Connection -> MVar [String] -> IO ()
statsthread l c hl =
    do total <- getCount c "1 = 1"
       let totaltext = show total ++ " total"
       statetxts <- mapM (procstate total) states
       let disp = concat . intersperse ", " $ totaltext : statetxts
       msg disp
       threadDelay (30 * 1000000)
       statsthread l c hl
    where states = [NotVisited, VisitingNow, Visited, ErrorState]
          procstate total s =
              do r <- getCount c ("state = " ++ toSqlValue (show s))
                 let pct = r * 100 `div` total
                 return $ show r ++ " (" ++ show pct ++ "%) " ++ (show s)
                 