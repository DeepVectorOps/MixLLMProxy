{-# LANGUAGE OverloadedStrings #-}

module AppEnv
  ( AppEnv(..)
  , GlobalSettings(..)
  , initAppEnv
  , withPool
  ) where

import Database.PostgreSQL.Simple (Connection, close)
import DB (connectDB)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)
import Data.IORef (IORef, newIORef)
import Data.Time.Clock (UTCTime)

data GlobalSettings = GlobalSettings
  { gsPaused :: !Bool
  , gsSlowLimit :: !(Maybe Double)
  } deriving (Show)

data AppEnv = AppEnv
  { envPool :: Pool Connection
  , envSettings :: IORef GlobalSettings
  , envRequestTimes :: IORef [UTCTime]
  }

initAppEnv :: IO AppEnv
initAppEnv = do
  let poolConfig = defaultPoolConfig connectDB close 60 5
  pool <- newPool poolConfig
  settings <- newIORef (GlobalSettings False Nothing)
  reqTimes <- newIORef []
  pure AppEnv
    { envPool = pool
    , envSettings = settings
    , envRequestTimes = reqTimes
    }

withPool :: AppEnv -> (Connection -> IO a) -> IO a
withPool env action = withResource (envPool env) action
