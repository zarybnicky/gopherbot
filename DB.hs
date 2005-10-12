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

module DB where

import Config
import Database.HSQL
import Database.HSQL.SQLite3
import Data.Char
import System.IO
import Data.List

initdb :: IO Connection

initdb =
    do putStrLn " *** Initializing database system..."
       handleSqlError $
         do c <- connect ("state.sql3") ReadWriteMode
            initTables c
            return c

initTables conn = handleSqlError $
    do t <- tables conn
       let t2 = map (map toUpper) t
       if not (elem "FILES" t2)
          then do execute conn "CREATE TABLE files (host TEXT, port INTEGER,  path TEXT, state TEXT)"
                  execute conn "CREATE UNIQUE INDEX files1 ON files(host, port, path, state)"
                  execute conn "CREATE INDEX files2 ON files(host, port)"
          else return ()

updateItem :: Connection -> GAddress -> State -> IO ()
updateItem conn g s = handleSqlError $
    do execute conn $ "DELETE FROM files WHERE host = " 
        ++ toSqlValue (host g) ++ " AND port = " 
        ++ toSqlValue (port g) ++ " AND path = "
        ++ toSqlValue (path g)
       execute conn $ "INSERT INTO files VALUES (" ++
           toSqlValue (host g) ++ ", " ++
           toSqlValue (port g) ++ ", " ++
           toSqlValue (path g) ++ ", " ++
           toSqlValue (show s) ++ ")"

-- | Gets the next item to visit, if any, and sets the status
-- to Visiting.  Returns Nothing if there is no next item.
popItem :: Connection -> IO (Maybe GAddress)
popItem conn =
    do sth <- query conn $ "SELECT * FROM files WHERE state = " ++
                           (toSqlValue (show NotVisited))
       h <- fetch sth
       if h
          then do h <- getFieldValue sth "host"
                  p <- getFieldValue sth "port"
                  pa <- getFieldValue sth "path"
                  let po = read p
                  let ga = GAddress {host = h, port = po, path = pa}
                  closeStatement sth
                  updateItem conn ga VisitingNow
                  return (Just ga)
          else do closeStatement sth
                  return Nothing

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)
