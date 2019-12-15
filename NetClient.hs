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

module NetClient where

import Control.Exception(catch, finally, bracket)
import Network.Socket
import Types
import System.IO
import System.IO.Error
import System.Timeout (timeout)

timeo :: Int
timeo = 120 * 1000000

cto :: String -> IO a -> IO a
cto msg action = do
  r <- timeout timeo action
  case r of
    Nothing -> fail msg
    Just x -> return x

dlItem :: GAddress -> Handle -> IO ()
dlItem ga fh = flip finally (hClose fh) $ do
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just $ host ga) (Just . show $ port ga)
  bracket (cto "Timeout on connect" $ open addr) (cto "Timeout on close" . close) $ \s -> do
    cto "Timeout on send" $ sendAll s $ path ga ++ "\r\n"
    --cto "Timeout on shotdown" $ shutdown s ShutdownSend
    if dtype ga == '1'
      then dlTillDot s fh
      else dlTo s fh
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      pure sock

dlTillDot :: Socket -> Handle -> IO ()
dlTillDot s fh = do
  c <- sGetContents s
  hPutStr fh (process c)
  where
    process = unlines . filter (`notElem` [".", ".\r", ".\r\n"]) . lines

sendAll :: Socket -> String -> IO ()
sendAll _ [] = return ()
sendAll s buf = do
  bytessent <- send s buf
  sendAll s (drop bytessent buf)

recvBlocks :: Socket -> (a -> String -> IO a) -> a -> IO a
recvBlocks s action state = do
  buf <- cto "Timeout on recv" (dorecv 8192)
  if null buf
    then return state
    else do
      newstate <- action state buf
      recvBlocks s action newstate
  where
    dorecv len = catch
      (recv s len)
      (\e -> if isEOFError e then pure [] else ioError e)

-- FIXME: this is slow and a RAM hog.

sGetContents :: Socket -> IO String
sGetContents s = recvBlocks s (\o n -> return $ o ++ n) []

dlTo :: Socket -> Handle -> IO ()
dlTo s fh = recvBlocks s (\() buf -> hPutStr fh buf) ()
