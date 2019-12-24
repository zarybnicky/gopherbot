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

{-# LANGUAGE OverloadedStrings #-}

module NetClient
  ( dlItem
  ) where

import Control.Exception (bracket, finally)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Network.Socket
  (AddrInfo(..), Socket, SocketType(..), connect, close, defaultHints, getAddrInfo, socket)
import Network.Socket.ByteString (recv, sendAll)
import Types (GAddress(..))
import System.IO (Handle, hClose)
import System.Timeout (timeout)

dlItem :: GAddress -> Handle -> IO ()
dlItem ga fh = flip finally (hClose fh) $ do
  addr <- head <$> getAddrInfo (Just streamHints) (Just $ host ga) (Just . show $ port ga)
  bracket (cto "connect" $ open addr) (cto "close" . close) $ \s -> do
    cto "send" $ sendAll s (BC.pack (path ga) <> "\r\n")
    if dtype ga == '1'
      then dlTillDot s fh
      else dlTo s fh
  where
    streamHints = defaultHints { addrSocketType = Stream }
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      pure sock

dlTo :: Socket -> Handle -> IO ()
dlTo s fh = do
  chunk <- cto "recv" (recv s 4096)
  case chunk of
    "" -> pure ()
    _ -> BC.hPut fh chunk >> dlTo s fh

dlTillDot :: Socket -> Handle -> IO ()
dlTillDot s fh = BC.hPut fh . process . BL.toStrict =<< sGetContents
  where
    sGetContents = do
      chunk <- cto "recv" (recv s 4096)
      case chunk of
        "" -> pure ""
        _ -> (BL.fromStrict chunk <>) <$> sGetContents
    process = BC.unlines . filter (`notElem` [".", ".\r", ".\r\n"]) . BC.lines

cto :: String -> IO a -> IO a
cto msg action =
  maybe (fail ("Timeout on " ++ msg)) pure =<< timeout timeo action
  where
    timeo = 120 * 1000000
