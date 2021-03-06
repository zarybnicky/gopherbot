{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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


module DB
  ( withdb
  , initdb
  , initTables
  , updateItem
  , popItem
  , queueItems
  , numToProc
  , noteErrorOnHost
  ) where

import Control.Concurrent (ThreadId, modifyMVar, myThreadId, threadDelay)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Char (toUpper)
import Data.List (intercalate)
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Lock (withLock)
import System.Time (ClockTime(TOD), getClockTime)
import Types (Env(..), GAddress(..), State(..))
import Utils (msg)

withdb :: Env -> (Connection -> IO a) -> IO a
withdb env = bracket dbconnect disconnect
  where
    dbconnect = handleSqlError $ do
      msg " *** Connecting to DB"
      connectPostgreSQL (dbConn env)

{- | Initialize the database system. -}
initdb :: Connection -> IO ()
initdb c = do
  msg " *** Initializing database system..."
  initTables c
  r <- getCount c "state = ?" [toSql VisitingNow]
  when (r > 0) (msg $ "Resetting " ++ show r ++ " files from VisitingNow to NotVisited.")
  _ <- run c "UPDATE FILES SET STATE = ? WHERE state = ?" [toSql NotVisited, toSql VisitingNow]
  commit c

initTables :: Connection -> IO ()
initTables conn = handleSqlError $ do
  t <- fmap (fmap toUpper) <$> getTables conn
  when ("FILES" `notElem` t) $ do
    _ <- run conn "CREATE TABLE files (host TEXT, port INTEGER, dtype TEXT, path TEXT, state TEXT, timestamp INTEGER, log TEXT)" []
    _ <- run conn "CREATE UNIQUE INDEX files1 ON files(host, port, path)" []
    _ <- run conn "CREATE INDEX filesstate ON files (state, host)" []
    _ <- flip (run conn) [] $ intercalate
      "\n"
      [ "CREATE FUNCTION merge_files(h TEXT, p INTEGER, dt TEXT, pa TEXT, s TEXT, ts INTEGER, l TEXT) RETURNS VOID AS"
      , "$$"
      , "BEGIN"
      , "    LOOP"
      , "        UPDATE files SET dtype = dt, state = s, timestamp = ts, log = l"
      , "               WHERE host = h AND port = p AND path = pa;"
      , "        IF found THEN"
      , "            RETURN;"
      , "        END IF;"
      , ""
      , "        BEGIN"
      , "            INSERT INTO files(host,port,dtype,path,state,timestamp,log) VALUES "
      , "               (h, p, dt, pa, s, ts, l);"
      , "            RETURN;"
      , "        EXCEPTION WHEN unique_violation THEN"
      , "            -- do nothing"
      , "        END;"
      , "    END LOOP;"
      , "END;"
      , "$$"
      , "LANGUAGE plpgsql"
      ]
    _ <- flip (run conn) [] $ intercalate
      "\n"
      [ "CREATE FUNCTION queue_files(h TEXT, p INTEGER, dt TEXT, pa TEXT, s TEXT, ts INTEGER, l TEXT) RETURNS VOID AS"
      , "$$"
      , "BEGIN"
      , "            INSERT INTO files(host,port,dtype,path,state,timestamp,log) VALUES "
      , "               (h, p, dt, pa, s, ts, l);"
      , "            RETURN;"
      , "        EXCEPTION WHEN unique_violation THEN"
      , "            RETURN;"
      , "END;"
      , "$$"
      , "LANGUAGE plpgsql"
      ]
    pure ()

noteErrorOnHost :: Env -> Connection -> String -> String -> IO ()
noteErrorOnHost env c h logString = handleSqlError $ do
  t <- myThreadId
  modifyMVar (gasupply env) $ \m -> withLock (lock env) $ do
    ti <- now
    msg $ " *** Noting error on host " ++ h
    case Map.lookup t m of
      Nothing -> pure ()
      Just (_, sth) -> finish sth
    _ <- run c ("UPDATE FILES SET state = ?, log = ?, timestamp = ?"
           ++ " WHERE host = ? AND (state = ? OR state = ?)")
          [toSql ErrorState, toSql logString, ti,
           toSql h, toSql NotVisited, toSql VisitingNow]
    commit c
    pure (Map.delete t m, ())

updateItem :: Env -> Connection -> GAddress -> State -> String -> IO ()
updateItem env conn g s = withLock (lock env) . updateItemNL conn g s

updateItemNL :: Connection -> GAddress -> State -> String -> IO ()
updateItemNL conn g s logString =
  handleSqlError . withTransaction conn $ \c -> mergeItemNLNT c g s logString

mergeItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
mergeItemNLNT conn g s logString = do
  t <- now
  void $ run conn "SELECT merge_files (?, ?, ?, ?, ?, ?, ?)"
    [ toSql (host g), toSql (port g), toSql (dtype g)
    , toSql (path g), toSql s, t, toSql logString
    ]

queueItemNLNT :: Connection -> GAddress -> State -> String -> IO ()
queueItemNLNT conn g s logString = do
  t <- now
  void $ run conn "SELECT queue_files (?, ?, ?, ?, ?, ?, ?)"
    [ toSql (host g), toSql (port g), toSql (dtype g)
    , toSql (path g), toSql s, t, toSql logString
    ]

now :: IO SqlValue
now = toSql . (\(TOD x _) -> x) <$> getClockTime

getCount :: Connection -> String -> [SqlValue] -> IO Integer
getCount conn whereclause parms = do
  r <- quickQuery conn
        ("SELECT COUNT(*) FROM FILES WHERE " ++ whereclause) parms
  pure . fromSql . head $ head r

queueItems :: Env -> Connection -> [GAddress] -> IO ()
queueItems env conn g =
  withLock (lock env) $ withTransaction conn (\c -> mapM_ (queueItemNL c) g)

-- Don't care if the insert fails; that means we already know of it.
queueItemNL :: Connection -> GAddress -> IO ()
queueItemNL conn g = handleSqlError $ queueItemNLNT conn g NotVisited ""

numToProc :: Connection -> IO Integer
numToProc conn = handleSqlError $ getCount conn "state = ?" [toSql NotVisited]

{- | Gets the next item to visit, if any, and sets the status
to Visiting.  Returns Nothing if there is no next item.

General algorithm: pick an available host (one not being serviced by
another thread) and process everything possible within it.
-}
popItem :: Env -> Connection -> IO (Maybe GAddress)
popItem env conn =
  handleSqlError $ do
    threadDelay 1000000
    t <- myThreadId
    modifyMVar (gasupply env) $ \gmap -> withLock (lock env) $
      case Map.lookup t gmap of
        Nothing -> beginSearch gmap conn
        Just (host', sth) -> fetchSth gmap (host', sth) conn

{- | Begin the search for new selectors. We can assume that this thread is not in
the map, since this function is only called in that case. -}
beginSearch ::
     Map.Map ThreadId (String, Statement)
  -> Connection
  -> IO (Map.Map ThreadId (String, Statement), Maybe GAddress)
beginSearch m conn = handleSqlError $ do
  sth <- prepare conn ("SELECT host FROM files WHERE state = ? " ++ whereclause ++ " LIMIT 1")
  _ <- execute sth (toSql NotVisited:params)
  t <- myThreadId
  r <- fetchRow sth
  finish sth
  case r of
    Just [h] -> do
      newsth <- prepare conn "SELECT * FROM files WHERE state = ? AND host = ? LIMIT 2000"
      _ <- execute newsth [toSql NotVisited, h]
      let newmap = Map.insert t (fromSql h, newsth) m
      fetchSth newmap (fromSql h, newsth) conn
    Nothing -> do
      msg "No available hosts; dying"
      -- Couldn't find any available hosts.  For now, we just die.
      -- FIXME: later should find a better way to do this.
      pure (m, Nothing)
    x -> fail $ "Unexpected result in beginSearch: " ++ show x
  where
    whereclause =
      case map fst (Map.elems m) of
        [] -> ""
        x -> " AND " ++ intercalate " AND " (map (const " host != ?") x)
    params = map (toSql . fst) (Map.elems m)

fetchSth ::
     Map.Map ThreadId (String, Statement)
  -> (String, Statement)
  -> Connection
  -> IO (Map.Map ThreadId (String, Statement), Maybe GAddress)
fetchSth m (_, sth) conn = fetchRow sth >>= \case
  Just row -> pure
      ( m
      , Just GAddress
        { host = fromSql (head row)
        , port = fromSql (row !! 1)
        , dtype = fromSql (row !! 2)
        , path = fromSql (row !! 3)
        }
      )
  Nothing -> do
    t <- myThreadId
    finish sth
    beginSearch (Map.delete t m) conn
