{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import AppEnv (initAppEnv)
import Route.OpenAI (openAIRoutes)
import Route.UI (uiRoutes)
import Route.Aliases (aliasesRoutes)

main :: IO ()
main = do
  env <- initAppEnv
  port <- maybe 8015 id . (>>= readMaybe) <$> lookupEnv "LLM_PORT"
  scotty port $ do
    openAIRoutes env
    uiRoutes env
    aliasesRoutes env
