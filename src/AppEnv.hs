{-# LANGUAGE OverloadedStrings #-}

module AppEnv
  ( AppEnv(..)
  , initAppEnv
  , withPool
  ) where

import Database.PostgreSQL.Simple (Connection, close)
import DB (connectDB)
import Data.Pool (Pool, newPool, defaultPoolConfig, withResource)

data AppEnv = AppEnv
  { envPool :: Pool Connection
  }

initAppEnv :: IO AppEnv
initAppEnv = do
  let poolConfig = defaultPoolConfig connectDB close 60 5
  pool <- newPool poolConfig
  pure AppEnv { envPool = pool }

withPool :: AppEnv -> (Connection -> IO a) -> IO a
withPool env action = withResource (envPool env) action
