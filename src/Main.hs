{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import AppEnv (initAppEnv)
import Route.OpenAI (openAIRoutes)
import Route.UI (uiRoutes)
import Route.Aliases (aliasesRoutes)
import Route.Debug (debugRoutes)

main :: IO ()
main = do
  env <- initAppEnv
  port <- maybe 8015 id . (>>= readMaybe) <$> lookupEnv "LLM_PORT"
  scotty port $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    openAIRoutes env
    debugRoutes env
    uiRoutes env
    aliasesRoutes env
