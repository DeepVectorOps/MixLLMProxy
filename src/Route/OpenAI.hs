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
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Database.PostgreSQL.Simple (Connection)
import AppEnv (AppEnv, withPool)
import DB (insertRequest, getAliasByName, getAliasUsage24h, LlmAlias(..))
import Common (showT)

openAIRoutes :: AppEnv -> ScottyM ()
openAIRoutes env = do
  post "/api/openai/v1/chat/completions" $ do
    reqBody <- body
    let reqModel = extractModel reqBody
    mAlias <- case reqModel of
      Just modelName -> liftIO $ withPool env $ \conn -> getAliasByName conn modelName
      Nothing -> pure Nothing
    case mAlias of
      Just alias -> do
        mBlocked <- liftIO $ withPool env $ \conn -> checkRateLimit conn alias
        case mBlocked of
          Just errMsg -> do
            status status429
            setHeader "Content-Type" "application/json"
            raw $ A.encode $ A.object
              [ "error" A..= errMsg
              , "type" A..= ("rate_limit_exceeded" :: T.Text)
              ]
          Nothing -> do
            let overriddenBody = case A.decode reqBody of
                  Just (A.Object obj) -> A.encode $ A.Object $ KM.insert "model" (A.String $ laModel alias) obj
                  _ -> reqBody
            proxyAndLog env (laEndpointUrl alias) (laApiKey alias) (Just (laName alias)) overriddenBody
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

proxyAndLog :: AppEnv -> T.Text -> T.Text -> Maybe T.Text -> BL.ByteString -> ActionM ()
proxyAndLog env downstreamUrl apiKey aliasName reqBody = do
  startTime <- liftIO getCurrentTime
  (respStatus, respBody) <- liftIO $ do
    manager <- newManager tlsManagerSettings
    req <- buildRequest downstreamUrl apiKey reqBody
    resp <- httpLbs req manager
    pure (responseStatus resp, responseBody resp)
  endTime <- liftIO getCurrentTime

  let latency = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
  let (model, (promptT, complT, totalT)) = extractPayload respBody
  let code = statusCode respStatus
  let reqText = Just $ cs reqBody
  let respText = Just $ cs respBody

  let endpoint = maybe "/api/openai/v1/chat/completions" (\a -> "/api/openai/v1/chat/completions (" <> a <> ")") aliasName

  liftIO (withPool env $ \conn ->
    insertRequest conn endpoint "POST" reqText
      (Just code) respText latency model promptT complT totalT aliasName) `catch` (\(_ :: SomeException) -> pure ())

  status respStatus
  setHeader "Content-Type" "application/json"
  raw respBody

extractPayload :: BL.ByteString -> (Maybe T.Text, (Maybe Int, Maybe Int, Maybe Int))
extractPayload body = case A.decode cleanBody of
  Just v  -> (parseMaybe (A..: "model") v, extractTokens v)
  Nothing -> (Nothing, (Nothing, Nothing, Nothing))
  where
    cleanBody
      | "data: " `BLC.isPrefixOf` body =
          BLC.takeWhile (/= '\n') (BLC.dropWhile (/= '{') body)
      | otherwise = body
    extractTokens v = case parseMaybe (A..: "usage") v of
      Just u  -> ( parseMaybe (A..: "prompt_tokens") u
                 , parseMaybe (A..: "completion_tokens") u
                 , parseMaybe (A..: "total_tokens") u
                 )
      Nothing -> (Nothing, Nothing, Nothing)
