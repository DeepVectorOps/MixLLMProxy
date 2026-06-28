{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Route.OpenAI (openAIRoutes, buildRequest) where

import Web.Scotty
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import Data.String.Conversions (cs)
import Data.Maybe (listToMaybe, isJust)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException, finally)
import qualified Control.Exception as E
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.CaseInsensitive as CI

import Data.Time.Clock (getCurrentTime, diffUTCTime, addUTCTime)
import Database.PostgreSQL.Simple (Connection)
import AppEnv (AppEnv(..), GlobalSettings(..), withPool)
import DB (insertPendingRequest, updateRequest, getAliasByName, getAliasUsage24h, LlmAlias(..))
import Common (showT)
import Data.IORef (readIORef, atomicModifyIORef', newIORef)
import Network.HTTP.Types.Status (status429, status503)

checkGlobalRateLimit :: AppEnv -> IO (Maybe T.Text)
checkGlobalRateLimit env = do
  settings <- readIORef (envSettings env)
  if gsPaused settings
    then pure (Just "API is globally paused")
    else case gsSlowLimit settings of
      Nothing -> pure Nothing
      Just limit -> do
        now <- getCurrentTime
        let oneSecAgo = addUTCTime (-1) now
        blocked <- atomicModifyIORef' (envRequestTimes env) $ \times ->
          let recent = filter (> oneSecAgo) times
              count = length recent
          in if fromIntegral count >= limit
               then (recent, True)
               else (now : recent, False)
        if blocked
          then pure (Just $ "Global rate limit exceeded (" <> showT limit <> " reqs/sec)")
          else pure Nothing

jsonError :: Status -> T.Text -> T.Text -> ActionM ()
jsonError st msg typ = do
  status st
  setHeader "Content-Type" "application/json"
  raw $ A.encode $ A.object ["error" A..= msg, "type" A..= typ]

openAIRoutes :: AppEnv -> ScottyM ()
openAIRoutes env = do
  post "/api/openai/v1/chat/completions" $ do
    mGlobalBlocked <- liftIO $ checkGlobalRateLimit env
    case mGlobalBlocked of
      Just errMsg -> do
        let isPaused = errMsg == "API is globally paused"
        jsonError (if isPaused then status503 else status429) errMsg
          (if isPaused then "api_paused" else "rate_limit_exceeded")
      Nothing -> do
        reqBody <- body
        let reqModel = extractModel reqBody
        mAlias <- case reqModel of
          Just modelName -> liftIO $ withPool env $ \conn -> getAliasByName conn modelName
          Nothing -> pure Nothing
        case mAlias of
          Just alias -> do
            mBlocked <- liftIO $ withPool env $ \conn -> checkRateLimit conn alias
            case mBlocked of
              Just errMsg -> jsonError status429 errMsg "rate_limit_exceeded"
              Nothing -> do
                let overriddenBody = case A.decode reqBody of
                      Just (A.Object obj) -> A.encode $ A.Object $ KM.insert "model" (A.String $ laModel alias) obj
                      _ -> reqBody
                let endpoint = "/api/openai/v1/chat/completions (" <> laName alias <> ")"
                let reqText = Just $ cs overriddenBody
                rid <- liftIO $ withPool env $ \conn ->
                  insertPendingRequest conn endpoint "POST" reqText (Just (laModel alias)) (Just (laName alias))
                proxyAndLog env rid (laEndpointUrl alias) (laApiKey alias) (Just (laName alias)) overriddenBody
          Nothing -> do
            status status400
            json $ A.object ["error" A..= ("no alias found for model: " <> maybe "(missing)" id reqModel)]

checkRateLimit :: Connection -> LlmAlias -> IO (Maybe T.Text)
checkRateLimit conn alias = do
  (reqCount, tokenCount) <- getAliasUsage24h conn (laName alias)
  pure $ case (laDailyRequestLimit alias, laDailyTokenLimit alias) of
    (Just reqLimit, _) | reqCount >= reqLimit ->
      Just $ "Daily request limit reached for alias '" <> laName alias <> "': " <> showT reqCount <> "/" <> showT reqLimit <> " requests in the last 24h"
    (_, Just tokLimit) | tokenCount >= tokLimit ->
      Just $ "Daily token limit reached for alias '" <> laName alias <> "': " <> showT tokenCount <> "/" <> showT tokLimit <> " tokens in the last 24h"
    _ -> Nothing


extractModel :: BL.ByteString -> Maybe T.Text
extractModel body = case A.decode body of
  Just (A.Object obj) -> KM.lookup "model" obj >>= \case
    A.String t -> Just t
    _ -> Nothing
  _ -> Nothing

buildRequest :: T.Text -> T.Text -> BL.ByteString -> IO Request
buildRequest downstreamUrl apiKey reqBody = do
  initReq <- parseRequest (cs downstreamUrl)
  pure $ initReq
    { method = "POST"
    , requestBody = RequestBodyLBS reqBody
    , requestHeaders =
        [ ("Content-Type", "application/json")
        , ("Authorization", TE.encodeUtf8 ("Bearer " <> cs apiKey))
        ]
    , responseTimeout = responseTimeoutNone
    }

proxyAndLog :: AppEnv -> Int -> T.Text -> T.Text -> Maybe T.Text -> BL.ByteString -> ActionM ()
proxyAndLog env rid downstreamUrl apiKey aliasName reqBody = do
  startTime <- liftIO getCurrentTime
  (resp, manager) <- liftIO $ do
    manager <- newManager tlsManagerSettings
    req <- buildRequest downstreamUrl apiKey reqBody
    resp <- responseOpen req manager
    pure (resp, manager)

  let respStatus = responseStatus resp

  status respStatus
  forM_ (responseHeaders resp) $ \(name, val) -> do
    unless (name `elem` ["Content-Length", "Transfer-Encoding", "Content-Encoding"]) $
      setHeader (cs (CI.original name)) (cs val)

  chunksVar <- liftIO $ newIORef []

  let streamAction write flush = do
        let loop = do
              chunk <- responseBody resp
              if BS.null chunk
                then pure ()
                else do
                  atomicModifyIORef' chunksVar (\chunks -> (chunk : chunks, ()))
                  write (B.byteString chunk)
                  flush
                  loop
        loop `finally` do
          responseClose resp
          endTime <- getCurrentTime
          let latency = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
          chunks <- readIORef chunksVar
          let respBody = BL.fromChunks (reverse chunks)
          let (model, (promptT, complT, totalT)) = extractPayload respBody
          let code = statusCode respStatus
          let respText = Just $ cs respBody
          withPool env (\conn ->
            updateRequest conn rid (Just code) respText latency model promptT complT totalT)
              `E.catch` (\(_ :: SomeException) -> pure ())

  stream streamAction

extractPayload :: BL.ByteString -> (Maybe T.Text, (Maybe Int, Maybe Int, Maybe Int))
extractPayload body
  | isSSE = (firstModel, lastTokens)
  | otherwise = case A.decode body of
      Just v  -> (parseMaybe (A..: "model") v, extractTokens v)
      Nothing -> (Nothing, (Nothing, Nothing, Nothing))
  where
    lines' = BLC.lines body
    isSSE = any ("data: " `BLC.isPrefixOf`) lines'

    parsedChunks :: [A.Object]
    parsedChunks =
      [ obj
      | line <- lines'
      , let cleanLine = BLC.drop 6 line
      , cleanLine /= "[DONE]"
      , Just (A.Object obj) <- [A.decode cleanLine]
      ]

    firstModel = listToMaybe [ m | obj <- parsedChunks, Just m <- [parseMaybe (A..: "model") obj] ]

    tokenUsages = [ extractTokens obj | obj <- parsedChunks ]
    lastTokens = case filter (\(p, c, t) -> isJust p || isJust c || isJust t) tokenUsages of
      [] -> (Nothing, Nothing, Nothing)
      us -> last us

    extractTokens obj = case parseMaybe (A..: "usage") obj of
      Just u  -> ( parseMaybe (A..: "prompt_tokens") u
                 , parseMaybe (A..: "completion_tokens") u
                 , parseMaybe (A..: "total_tokens") u
                 )
      Nothing -> (Nothing, Nothing, Nothing)
