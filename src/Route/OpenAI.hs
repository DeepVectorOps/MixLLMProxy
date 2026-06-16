{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.OpenAI (openAIRoutes) where

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
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import AppEnv (AppEnv, withPool)
import DB (insertRequest, getAliasByName, LlmAlias(..))

handleProxy :: AppEnv -> Maybe LlmAlias -> ActionM ()
handleProxy env mAlias = do
  reqBody <- body
  case mAlias of
    Just alias -> do
      let overriddenReqBody = case A.decode reqBody of
            Just (A.Object obj) -> A.encode $ A.Object $ KM.insert "model" (A.String $ laModel alias) obj
            _ -> reqBody
      proxyAndLog env (laEndpointUrl alias) (laApiKey alias) (Just (laName alias)) overriddenReqBody
    Nothing -> do
      downstreamUrl <- liftIO $ getEnv "LLM_API_URL"
      apiKey <- liftIO $ getEnv "LLM_API_KEY"
      proxyAndLog env (cs downstreamUrl) (cs apiKey) Nothing reqBody

openAIRoutes :: AppEnv -> ScottyM ()
openAIRoutes env = do
  post "/api/openai/v1/chat/completions/:alias" $ do
    aliasName <- pathParam "alias"
    mAlias <- liftIO $ withPool env $ \conn -> getAliasByName conn aliasName
    case mAlias of
      Nothing -> do
        status status404
        json $ A.object ["error" A..= ("alias not found: " <> aliasName)]
      Just alias -> handleProxy env (Just alias)

  post "/api/openai/v1/chat/completions" $ handleProxy env Nothing

proxyAndLog :: AppEnv -> T.Text -> T.Text -> Maybe T.Text -> BL.ByteString -> ActionM ()
proxyAndLog env downstreamUrl apiKey aliasName reqBody = do
  startTime <- liftIO getCurrentTime
  (respStatus, respBody) <- liftIO $ do
    manager <- newManager tlsManagerSettings
    initReq <- parseRequest (cs downstreamUrl)
    let req = initReq
          { method = "POST"
          , requestBody = RequestBodyLBS reqBody
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", TE.encodeUtf8 ("Bearer " <> cs apiKey))
              ]
          , responseTimeout = responseTimeoutNone
          }
    resp <- httpLbs req manager
    pure (responseStatus resp, responseBody resp)
  endTime <- liftIO getCurrentTime

  let latency = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
  let (model, (promptT, complT, totalT)) = extractPayload respBody
  let code = statusCode respStatus
  let reqText = Just $ cs reqBody
  let respText = Just $ cs respBody

  let endpoint = maybe "/api/openai/v1/chat/completions" (\a -> "/api/openai/v1/chat/completions/" <> a) aliasName

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
