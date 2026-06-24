{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DB
  ( connectDB
  , insertRequest
  , insertPendingRequest
  , updateRequest
  , getRecentRequests
  , getRequest
  , countRequests
  , truncateRequests
  , LlmRequest(..)

  , LlmAlias(..)
  , getAliases
  , getAliasesWithUsage
  , AliasUsage(..)
  , getAliasByName
  , getAliasById
  , insertAlias
  , updateAlias
  , deleteAlias
  , getAliasUsage24h
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

data LlmAlias = LlmAlias
  { laId :: Int
  , laName :: Text
  , laEndpointUrl :: Text
  , laApiKey :: Text
  , laModel :: Text
  , laCreatedAt :: UTCTime
  , laDailyTokenLimit :: Maybe Int
  , laDailyRequestLimit :: Maybe Int
  } deriving (Show, Generic)

instance FromRow LlmAlias where
  fromRow = LlmAlias <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data AliasUsage = AliasUsage
  { auAlias :: LlmAlias
  , auRequestCount :: Int
  , auTokenCount :: Int
  } deriving (Show, Generic)

instance FromRow AliasUsage where
  fromRow = AliasUsage <$> fromRow <*> field <*> field

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

insertPendingRequest :: Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Int
insertPendingRequest conn endpoint method reqBody model aliasName = do
  [Only rid] <- query conn (fromString $ cs [text|
    INSERT INTO llm_requests (endpoint, method, request_body, model, alias_name)
    VALUES (?, ?, ?, ?, ?)
    RETURNING id
  |]) (endpoint, method, reqBody, model, aliasName)
  pure rid

updateRequest :: Connection -> Int -> Maybe Int -> Maybe Text -> Double -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
updateRequest conn rid respStatus respBody latencyMs model promptT complT totalT =
  void $ execute conn (fromString $ cs [text|
    UPDATE llm_requests
    SET response_status = ?, response_body = ?, latency_ms = ?, model = COALESCE(?, model), prompt_tokens = ?, completion_tokens = ?, total_tokens = ?
    WHERE id = ?
  |]) (respStatus, respBody, latencyMs, model, promptT, complT, totalT, rid)

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

aliasColumns :: Query
aliasColumns = fromString $ cs [text|
  id, name, endpoint_url, api_key, model, created_at, daily_token_limit, daily_request_limit
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

insertAlias :: Connection -> Text -> Text -> Text -> Text -> Maybe Int -> Maybe Int -> IO ()
insertAlias conn name url key model tokenLimit reqLimit =
  void $ execute conn (fromString $ cs [text|
    INSERT INTO aliases (name, endpoint_url, api_key, model, daily_token_limit, daily_request_limit)
    VALUES (?, ?, ?, ?, ?, ?)
  |]) (name, url, key, model, tokenLimit, reqLimit)

updateAlias :: Connection -> Int -> Text -> Text -> Text -> Text -> Maybe Int -> Maybe Int -> IO ()
updateAlias conn aid name url key model tokenLimit reqLimit =
  void $ execute conn (fromString $ cs [text|
    UPDATE aliases SET name = ?, endpoint_url = ?, api_key = ?, model = ?, daily_token_limit = ?, daily_request_limit = ? WHERE id = ?
  |]) (name, url, key, model, tokenLimit, reqLimit, aid)

deleteAlias :: Connection -> Int -> IO ()
deleteAlias conn aid =
  void $ execute conn "DELETE FROM aliases WHERE id = ?" (Only aid)

getAliasUsage24h :: Connection -> Text -> IO (Int, Int)
getAliasUsage24h conn aliasName = do
  rows <- query conn (fromString $ cs [text|
    SELECT COUNT(*), COALESCE(SUM(total_tokens), 0)
    FROM llm_requests
    WHERE alias_name = ? AND created_at >= NOW() - INTERVAL '24 hours'
  |]) (Only aliasName) :: IO [(Int, Int)]
  pure $ case rows of
    [(reqCount, tokenCount)] -> (reqCount, tokenCount)
    _ -> (0, 0)

getAliasesWithUsage :: Connection -> IO [AliasUsage]
getAliasesWithUsage conn =
  query_ conn (fromString $ cs [text|
    SELECT a.id, a.name, a.endpoint_url, a.api_key, a.model, a.created_at, a.daily_token_limit, a.daily_request_limit,
           COALESCE(r.req_count, 0), COALESCE(r.token_count, 0)
    FROM aliases a
    LEFT JOIN (
      SELECT alias_name, COUNT(*) AS req_count, COALESCE(SUM(total_tokens), 0) AS token_count
      FROM llm_requests
      WHERE created_at >= NOW() - INTERVAL '24 hours'
      GROUP BY alias_name
    ) r ON r.alias_name = a.name
    ORDER BY a.created_at DESC
  |])

truncateRequests :: Connection -> IO ()
truncateRequests conn =
  void $ execute_ conn "TRUNCATE TABLE llm_requests"
