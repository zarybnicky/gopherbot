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

module Utils
  ( getFSPath
  , msg
  ) where

import Control.Concurrent (myThreadId)
import Data.List (isPrefixOf)
import System.Directory (makeAbsolute)
import System.IO (hFlush, stdout)
import Types (GAddress(..))

getFSPath :: String -> GAddress -> IO FilePath
getFSPath base ga = do
  let dirtype = case dtype ga of
        '1' -> "/.gophermap"
        _ -> ""
  basepath <- makeAbsolute (host ga ++ "/" ++ show (port ga) ++ "/" ++ path ga ++ dirtype)
  if (base ++ "/gopher/") `isPrefixOf` basepath
    then pure basepath
    else error ("getFSPath1 " ++ show ga)

msg :: String -> IO ()
msg l = do
  t <- myThreadId
  putStrLn (show t ++ ": " ++ l)
  hFlush stdout
