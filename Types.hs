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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  ( GAddress(..)
  , State(..)
  , Config(..)
  , Env(..)
  , envFromConfig
  ) where

import Control.Concurrent (ThreadId, MVar, newMVar)
import Data.Convertible
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Database.HDBC
import GHC.Generics (Generic)
import Lock (Lock, newLock)
import Options.Generic
  ( ParseField
  , ParseRecord(..)
  , defaultModifiers
  , parseRecordWithModifiers
  , shortNameModifier
  )
import System.Directory (makeAbsolute)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (Read(..))

data State
  = NotVisited
  | VisitingNow
  | Visited
  | ErrorState
  | Excluded
  deriving (Eq, Read, Show)

instance Convertible SqlValue State where
  safeConvert (SqlString s) = read s
  safeConvert x = error $ "Cannot convert " ++ show x ++ " into gopherbot.State"

instance Convertible State SqlValue where
  safeConvert s = pure $ SqlString (show s)

data GAddress = GAddress
  { host :: String
  , port :: Int
  , dtype :: Char
  , path :: String
  } deriving (Eq, Typeable)

instance Read GAddress where
  readPrec = lift $ do
    host <- munch1 (`notElem` [':', '/'])
    port <- option 70 $ do
      _ <- char ':'
      read <$> munch1 (`elem` ['0'..'9'])
    dtype <- option '1' $ do
      _ <- char ':'
      read <$> munch1 (`elem` ['0'..'9'])
    path <- option "/" $ do
      _ <- char '/'
      manyTill get eof
    pure GAddress{..}

instance Show GAddress where
  show a = intercalate ":" [host a, show (port a), [dtype a], path a]

instance ParseField GAddress

data Config = Config
  { startingAddresses :: [GAddress]
  , excludeServers :: [String]
  , baseDir :: Maybe String
  , numThreads :: Maybe Int
  , dbConn :: Maybe String
  } deriving (Generic, Show)

instance ParseRecord Config where
  parseRecord = parseRecordWithModifiers $ defaultModifiers
    { shortNameModifier = \case
        "baseDir" -> Just 'd'
        "dbConn" -> Just 'c'
        "numThreads" -> Just 'j'
        "excludeServers" -> Just 'e'
        _ -> Nothing
    }

data Env = Env
  { startingAddresses :: [GAddress]
  , excludeServers :: [String]
  , baseDir :: String
  , numThreads :: Int
  , dbConn :: String
  , lock :: Lock
  , gasupply :: MVar (Map ThreadId (String, Statement))
  }

envFromConfig :: Config -> IO Env
envFromConfig Config{..} = do
  baseDir <- makeAbsolute (fromMaybe "." baseDir)
  numThreads <- pure (fromMaybe 1 numThreads)
  dbConn <- pure (fromMaybe "" dbConn)
  lock <- newLock
  gasupply <- newMVar M.empty
  pure Env{..}
