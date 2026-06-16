{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DB
  ( connectDB
  , insertRequest
  , getRecentRequests
  , getRequest
  , countRequests
  , LlmRequest(..)
  , LlmStats(..)
  , getStats
  , LlmAlias(..)
  , getAliases
  , getAliasByName
  , getAliasById
  , insertAlias
  , updateAlias
  , deleteAlias
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import NeatInterpolation (text)
import Data.String.Conversions (cs)
import Data.String (fromString)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, stderr)
import Control.Monad (void)

data LlmRequest = LlmRequest
  { lrId :: Int
  , lrEndpoint :: Text
  , lrMethod :: Text
  , lrRequestBody :: Maybe Text
  , lrResponseStatus :: Maybe Int
  , lrResponseBody :: Maybe Text
  , lrLatencyMs :: Maybe Double
  , lrModel :: Maybe Text
  , lrPromptTokens :: Maybe Int
  , lrCompletionTokens :: Maybe Int
  , lrTotalTokens :: Maybe Int
  , lrCreatedAt :: UTCTime
  , lrAliasName :: Maybe Text
  } deriving (Show, Generic)

instance FromRow LlmRequest where
  fromRow = LlmRequest <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data LlmStats = LlmStats
  { lsTotalRequests :: Int
  , lsTotalPromptTokens :: Maybe Int
  , lsTotalCompletionTokens :: Maybe Int
  , lsTotalTokens :: Maybe Int
  } deriving (Show, Generic)

instance FromRow LlmStats where
  fromRow = LlmStats <$> field <*> field <*> field <*> field

data LlmAlias = LlmAlias
  { laId :: Int
  , laName :: Text
  , laEndpointUrl :: Text
  , laApiKey :: Text
  , laModel :: Text
  , laCreatedAt :: UTCTime
  } deriving (Show, Generic)

instance FromRow LlmAlias where
  fromRow = LlmAlias <$> field <*> field <*> field <*> field <*> field <*> field

requestColumns :: Query
requestColumns = fromString $ cs [text|
  id, endpoint, method, request_body, response_status, response_body, latency_ms, model, prompt_tokens, completion_tokens, total_tokens, created_at, alias_name
|]

readDbConnectInfo :: IO ConnectInfo
readDbConnectInfo = do
  dbHost <- getEnv "DB_HOST"
  dbUser <- getEnv "DB_USER"
  dbPassword <- getEnv "DB_PASSWORD"
  dbPortStr <- getEnv "DB_PORT"
  dbPort <- case readMaybe dbPortStr :: Maybe Int of
    Just p -> pure $ fromIntegral p
    Nothing -> do
      hPutStrLn stderr $ "[WARN] Cannot parse DB_PORT value: " ++ show dbPortStr ++ ", falling back to 5432"
      pure 5432
  pure defaultConnectInfo
    { connectHost = dbHost
    , connectUser = dbUser
    , connectPassword = dbPassword
    , connectPort = dbPort
    }

connectDB :: IO Connection
connectDB = do
  info <- readDbConnectInfo
  dbName <- getEnv "DB_NAME"
  connect info { connectDatabase = dbName }

insertRequest :: Connection -> Text -> Text -> Maybe Text -> Maybe Int -> Maybe Text -> Double -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> IO ()
insertRequest conn endpoint method reqBody respStatus respBody latencyMs model promptT complT totalT aliasName =
  void $ execute conn (fromString $ cs [text|
    INSERT INTO llm_requests (endpoint, method, request_body, response_status, response_body, latency_ms, model, prompt_tokens, completion_tokens, total_tokens, alias_name)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |]) (endpoint, method, reqBody, respStatus, respBody, latencyMs, model, promptT, complT, totalT, aliasName)

getRecentRequests :: Connection -> Int -> Int -> IO [LlmRequest]
getRecentRequests conn limit offset = query conn ("SELECT " <> requestColumns <> " FROM llm_requests ORDER BY created_at DESC LIMIT ? OFFSET ?") (limit, offset)

getRequest :: Connection -> Int -> IO (Maybe LlmRequest)
getRequest conn rid = do
  results <- query conn ("SELECT " <> requestColumns <> " FROM llm_requests WHERE id = ?") (Only rid)
  pure $ case results of
    [r] -> Just r
    _ -> Nothing

countRequests :: Connection -> IO Int
countRequests conn = do
  [Only c] <- query_ conn "SELECT COUNT(*) FROM llm_requests"
  pure c

getStats :: Connection -> IO LlmStats
getStats conn = do
  [s] <- query_ conn "SELECT COUNT(*), COALESCE(SUM(prompt_tokens),0), COALESCE(SUM(completion_tokens),0), COALESCE(SUM(total_tokens),0) FROM llm_requests"
  pure s

aliasColumns :: Query
aliasColumns = fromString $ cs [text|
  id, name, endpoint_url, api_key, model, created_at
|]

aliasSelect :: Query
aliasSelect = "SELECT " <> aliasColumns <> " FROM aliases"

getAliases :: Connection -> IO [LlmAlias]
getAliases conn = query_ conn (aliasSelect <> " ORDER BY created_at DESC")

getAliasByName :: Connection -> Text -> IO (Maybe LlmAlias)
getAliasByName conn name = do
  results <- query conn (aliasSelect <> " WHERE name = ?") (Only name)
  pure $ case results of
    [a] -> Just a
    _   -> Nothing

getAliasById :: Connection -> Int -> IO (Maybe LlmAlias)
getAliasById conn aid = do
  results <- query conn (aliasSelect <> " WHERE id = ?") (Only aid)
  pure $ case results of
    [a] -> Just a
    _   -> Nothing

insertAlias :: Connection -> Text -> Text -> Text -> Text -> IO ()
insertAlias conn name url key model =
  void $ execute conn (fromString $ cs [text|
    INSERT INTO aliases (name, endpoint_url, api_key, model)
    VALUES (?, ?, ?, ?)
  |]) (name, url, key, model)

updateAlias :: Connection -> Int -> Text -> Text -> Text -> Text -> IO ()
updateAlias conn aid name url key model =
  void $ execute conn (fromString $ cs [text|
    UPDATE aliases SET name = ?, endpoint_url = ?, api_key = ?, model = ? WHERE id = ?
  |]) (name, url, key, model, aid)

deleteAlias :: Connection -> Int -> IO ()
deleteAlias conn aid =
  void $ execute conn "DELETE FROM aliases WHERE id = ?" (Only aid)
