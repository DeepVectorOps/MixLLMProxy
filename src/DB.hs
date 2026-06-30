{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DB
  ( connectDB
  , insertRequest
  , insertPendingRequest
  , updateRequest
  , getRecentRequests
  , getRecentRequestsFiltered
  , getRequest
  , countRequests
  , countRequestsFiltered
  , truncateRequests
  , truncateRequestsOlderThan
  , LlmRequest(..)
  , LlmEndpoint(..)
  , LlmAlias(..)
  , ResolvedAlias(..)
  , getEndpoints
  , getEndpointById
  , insertEndpoint
  , updateEndpoint
  , deleteEndpoint
  , countAliasesForEndpoint
  , getAliases
  , getAliasesWithUsage
  , AliasUsage(..)
  , getAliasByName
  , getAliasById
  , insertAlias
  , updateAlias
  , deleteAlias
  , getAliasUsage24h
  , getAliasRequestChartData
  , AliasChartRow(..)
  , RequestEvent(..)
  , getRecentRequestEvents
  , parseDuration
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField (toField, Action)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import NeatInterpolation (text)
import Data.String.Conversions (cs)
import Data.String (fromString)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, stderr)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Char (isDigit, isAlpha, isSpace)

queryOne :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO (Maybe r)
queryOne conn sql params = listToMaybe <$> query conn sql params


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

data LlmEndpoint = LlmEndpoint
  { leId :: Int
  , leName :: Text
  , leUrl :: Text
  , leApiKey :: Text
  , leCreatedAt :: UTCTime
  } deriving (Show, Generic)

instance FromRow LlmEndpoint where
  fromRow = LlmEndpoint <$> field <*> field <*> field <*> field <*> field

data LlmAlias = LlmAlias
  { laId :: Int
  , laName :: Text
  , laEndpointId :: Int
  , laEndpointName :: Text
  , laModel :: Text
  , laCreatedAt :: UTCTime
  , laDailyTokenLimit :: Maybe Int
  , laDailyRequestLimit :: Maybe Int
  } deriving (Show, Generic)

instance FromRow LlmAlias where
  fromRow = LlmAlias <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data ResolvedAlias = ResolvedAlias
  { raAlias :: LlmAlias
  , raEndpointUrl :: Text
  , raApiKey :: Text
  } deriving (Show, Generic)

instance FromRow ResolvedAlias where
  fromRow = ResolvedAlias <$> fromRow <*> field <*> field

data AliasUsage = AliasUsage
  { auAlias :: LlmAlias
  , auRequestCount :: Int
  , auTokenCount :: Int
  , auCharCount :: Int
  } deriving (Show, Generic)

instance FromRow AliasUsage where
  fromRow = AliasUsage <$> fromRow <*> field <*> field <*> field

data AliasChartRow = AliasChartRow
  { acrAliasId :: Int
  , acrAliasName :: Text
  , acrBucket :: UTCTime
  , acrCount :: Int
  } deriving (Show, Generic)

instance FromRow AliasChartRow where
  fromRow = AliasChartRow <$> field <*> field <*> field <*> field

data RequestEvent = RequestEvent
  { reId :: Int
  , reStatus :: Maybe Int
  , reAliasName :: Maybe Text
  } deriving (Show, Generic)

instance FromRow RequestEvent where
  fromRow = RequestEvent <$> field <*> field <*> field

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

getRecentRequestEvents :: Connection -> Int -> IO [RequestEvent]
getRecentRequestEvents conn limit =
  query conn (fromString $ cs [text|
    SELECT id, response_status, alias_name
    FROM (
      SELECT id, response_status, alias_name
      FROM llm_requests
      ORDER BY id DESC
      LIMIT ?
    ) recent
    ORDER BY id ASC
  |]) (Only limit)

getSortSql :: Text -> Text -> Query
getSortSql sortBy sortDir =
  let validCol :: Text
      validCol = case sortBy of
        "id"                -> "id"
        "created_at"        -> "created_at"
        "model"             -> "model"
        "alias_name"        -> "alias_name"
        "prompt_tokens"     -> "prompt_tokens"
        "completion_tokens" -> "completion_tokens"
        "total_tokens"      -> "total_tokens"
        "response_status"   -> "response_status"
        "latency_ms"        -> "latency_ms"
        "request_body"      -> "request_body"
        "response_body"     -> "response_body"
        "input_chars"       -> "length(request_body)"
        "output_chars"      -> "length(response_body)"
        _                   -> "created_at"
      validDir :: Text
      validDir = case T.toLower sortDir of
        "asc"  -> "ASC"
        "desc" -> "DESC"
        _      -> "DESC"
  in fromString (cs $ validCol <> " " <> validDir)

buildFilters :: Text -> Text -> Maybe Text -> (Query, [Action])
buildFilters searchField searchQuery mDuration =
  let (searchSql, searchParams) = buildSearchFilter searchField searchQuery
      (durSql, durParams) = buildDurationFilter mDuration
  in case (searchSql, durSql) of
       ("", "") -> ("", [])
       (s, "")  -> (" WHERE " <> s, searchParams)
       ("", d)  -> (" WHERE " <> d, durParams)
       (s, d)   -> (" WHERE (" <> s <> ") AND (" <> d <> ")", searchParams ++ durParams)

buildSearchFilter :: Text -> Text -> (Query, [Action])
buildSearchFilter field queryVal
  | T.null (T.strip queryVal) = ("", [])
  | otherwise =
      let likeVal = "%" <> queryVal <> "%"
      in case field of
        "any" ->
          ( "(model ILIKE ? OR alias_name ILIKE ? OR request_body ILIKE ? OR response_body ILIKE ? OR endpoint ILIKE ? OR method ILIKE ? OR response_status::text ILIKE ?)"
          , replicate 7 (toField likeVal)
          )
        "model" ->
          ( "model ILIKE ?"
          , [toField likeVal]
          )
        "alias" ->
          ( "alias_name ILIKE ?"
          , [toField likeVal]
          )
        "req_body" ->
          ( "request_body ILIKE ?"
          , [toField likeVal]
          )
        "resp_body" ->
          ( "response_body ILIKE ?"
          , [toField likeVal]
          )
        "endpoint" ->
          ( "endpoint ILIKE ?"
          , [toField likeVal]
          )
        "status" ->
          ( "response_status::text ILIKE ?"
          , [toField likeVal]
          )
        _ -> ("", [])

buildDurationFilter :: Maybe Text -> (Query, [Action])
buildDurationFilter Nothing = ("", [])
buildDurationFilter (Just intervalStr) =
  ( "created_at >= NOW() - ?::interval"
  , [toField intervalStr]
  )

parseDuration :: Text -> Maybe Text
parseDuration t =
  let clean = T.filter (not . isSpace) t
  in if T.null clean
       then Nothing
       else case parseSegments clean of
              [] -> Nothing
              segs -> Just (T.unwords segs)

parseSegments :: Text -> [Text]
parseSegments t
  | T.null t = []
  | otherwise =
      let (digits, rest) = T.span isDigit t
          (unit, remaining) = T.span isAlpha rest
      in if T.null digits || T.null unit
           then []
           else case mapUnit unit of
                  Just pgUnit -> (digits <> " " <> pgUnit) : parseSegments remaining
                  Nothing -> parseSegments remaining

mapUnit :: Text -> Maybe Text
mapUnit u = case T.toLower u of
  "s"       -> Just "seconds"
  "sec"     -> Just "seconds"
  "secs"    -> Just "seconds"
  "second"  -> Just "seconds"
  "seconds" -> Just "seconds"
  "m"       -> Just "minutes"
  "min"     -> Just "minutes"
  "mins"    -> Just "minutes"
  "minute"  -> Just "minutes"
  "minutes" -> Just "minutes"
  "h"       -> Just "hours"
  "hr"      -> Just "hours"
  "hrs"     -> Just "hours"
  "hour"    -> Just "hours"
  "hours"   -> Just "hours"
  "d"       -> Just "days"
  "day"     -> Just "days"
  "days"    -> Just "days"
  "w"       -> Just "weeks"
  "wk"      -> Just "weeks"
  "wks"     -> Just "weeks"
  "week"    -> Just "weeks"
  "weeks"   -> Just "weeks"
  _         -> Nothing

getRecentRequestsFiltered :: Connection -> Int -> Int -> Text -> Text -> Text -> Text -> Text -> IO [LlmRequest]
getRecentRequestsFiltered conn limit offset sortBy sortDir searchField searchQuery duration = do
  let (filterSql, filterParams) = buildFilters searchField searchQuery (parseDuration duration)
      sortSql = getSortSql sortBy sortDir
      sql = "SELECT " <> requestColumns <> " FROM llm_requests" <> filterSql <> " ORDER BY " <> sortSql <> " LIMIT ? OFFSET ?"
      params = filterParams ++ [toField limit, toField offset]
  query conn sql params

getRequest :: Connection -> Int -> IO (Maybe LlmRequest)
getRequest conn rid =
  queryOne conn ("SELECT " <> requestColumns <> " FROM llm_requests WHERE id = ?") (Only rid)

countRequests :: Connection -> IO Int
countRequests conn = do
  [Only c] <- query_ conn "SELECT COUNT(*) FROM llm_requests"
  pure c

countRequestsFiltered :: Connection -> Text -> Text -> Text -> IO Int
countRequestsFiltered conn searchField searchQuery duration = do
  let (filterSql, filterParams) = buildFilters searchField searchQuery (parseDuration duration)
      sql = "SELECT COUNT(*) FROM llm_requests" <> filterSql
  [Only c] <- query conn sql filterParams
  pure c

endpointColumns :: Query
endpointColumns = fromString $ cs [text|
  id, name, url, api_key, created_at
|]

endpointSelect :: Query
endpointSelect = "SELECT " <> endpointColumns <> " FROM endpoints"

getEndpoints :: Connection -> IO [LlmEndpoint]
getEndpoints conn = query_ conn (endpointSelect <> " ORDER BY created_at DESC")

getEndpointById :: Connection -> Int -> IO (Maybe LlmEndpoint)
getEndpointById conn eid =
  queryOne conn (endpointSelect <> " WHERE id = ?") (Only eid)

insertEndpoint :: Connection -> Text -> Text -> Text -> IO Int
insertEndpoint conn name url key = do
  [Only eid] <- query conn (fromString $ cs [text|
    INSERT INTO endpoints (name, url, api_key)
    VALUES (?, ?, ?)
    RETURNING id
  |]) (name, url, key)
  pure eid

updateEndpoint :: Connection -> Int -> Text -> Text -> Text -> IO ()
updateEndpoint conn eid name url key =
  void $ execute conn (fromString $ cs [text|
    UPDATE endpoints SET name = ?, url = ?, api_key = ? WHERE id = ?
  |]) (name, url, key, eid)

deleteEndpoint :: Connection -> Int -> IO ()
deleteEndpoint conn eid =
  void $ execute conn "DELETE FROM endpoints WHERE id = ?" (Only eid)

countAliasesForEndpoint :: Connection -> Int -> IO Int
countAliasesForEndpoint conn eid = do
  [Only c] <- query conn "SELECT COUNT(*) FROM aliases WHERE endpoint_id = ?" (Only eid)
  pure c

aliasColumns :: Query
aliasColumns = fromString $ cs [text|
  a.id, a.name, a.endpoint_id, e.name, a.model, a.created_at, a.daily_token_limit, a.daily_request_limit
|]

aliasFrom :: Query
aliasFrom = " FROM aliases a JOIN endpoints e ON e.id = a.endpoint_id"

aliasSelect :: Query
aliasSelect = "SELECT " <> aliasColumns <> aliasFrom

getAliases :: Connection -> IO [LlmAlias]
getAliases conn = query_ conn (aliasSelect <> " ORDER BY a.created_at DESC")

getAliasByName :: Connection -> Text -> IO (Maybe ResolvedAlias)
getAliasByName conn name =
  queryOne conn ("SELECT " <> aliasColumns <> ", e.url, e.api_key" <> aliasFrom <> " WHERE a.name = ?") (Only name)

getAliasById :: Connection -> Int -> IO (Maybe LlmAlias)
getAliasById conn aid =
  queryOne conn (aliasSelect <> " WHERE a.id = ?") (Only aid)

insertAlias :: Connection -> Text -> Int -> Text -> Maybe Int -> Maybe Int -> IO ()
insertAlias conn name endpointId model tokenLimit reqLimit =
  void $ execute conn (fromString $ cs [text|
    INSERT INTO aliases (name, endpoint_id, model, daily_token_limit, daily_request_limit)
    VALUES (?, ?, ?, ?, ?)
  |]) (name, endpointId, model, tokenLimit, reqLimit)

updateAlias :: Connection -> Int -> Text -> Int -> Text -> Maybe Int -> Maybe Int -> IO ()
updateAlias conn aid name endpointId model tokenLimit reqLimit =
  void $ execute conn (fromString $ cs [text|
    UPDATE aliases SET name = ?, endpoint_id = ?, model = ?, daily_token_limit = ?, daily_request_limit = ? WHERE id = ?
  |]) (name, endpointId, model, tokenLimit, reqLimit, aid)

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
    SELECT a.id, a.name, a.endpoint_id, e.name, a.model, a.created_at, a.daily_token_limit, a.daily_request_limit,
           COALESCE(r.req_count, 0), COALESCE(r.token_count, 0), COALESCE(r.char_count, 0)
    FROM aliases a
    JOIN endpoints e ON e.id = a.endpoint_id
    LEFT JOIN (
      SELECT alias_name, COUNT(*) AS req_count, COALESCE(SUM(total_tokens), 0) AS token_count,
             COALESCE(SUM(length(request_body) + length(response_body)), 0) AS char_count
      FROM llm_requests
      WHERE created_at >= NOW() - INTERVAL '24 hours'
      GROUP BY alias_name
    ) r ON r.alias_name = a.name
    ORDER BY COALESCE(r.req_count, 0) DESC, a.created_at DESC
  |])

getAliasRequestChartData :: Connection -> Int -> Int -> IO [AliasChartRow]
getAliasRequestChartData conn windowMin bucketSec =
  let secTxt = T.pack (show bucketSec)
      winTxt = T.pack (show windowMin)
  in query_ conn (fromString $ cs [text|
    WITH cfg AS (
      SELECT ${secTxt}::numeric AS sec, ${winTxt}::int AS win
    ),
    bounds AS (
      SELECT
        to_timestamp(floor(extract(epoch FROM NOW() - c.win * INTERVAL '1 minute') / c.sec) * c.sec) AS start_ts,
        to_timestamp(floor(extract(epoch FROM NOW()) / c.sec) * c.sec) AS end_ts
      FROM cfg c
    ),
    buckets AS (
      SELECT generate_series(b.start_ts, b.end_ts, c.sec * INTERVAL '1 second') AS bucket
      FROM bounds b CROSS JOIN cfg c
    ),
    counts AS (
      SELECT
        r.alias_name,
        to_timestamp(floor(extract(epoch FROM r.created_at) / c.sec) * c.sec) AS bucket,
        COUNT(*)::int AS cnt
      FROM llm_requests r, bounds b, cfg c
      WHERE r.created_at >= b.start_ts AND r.alias_name IS NOT NULL
      GROUP BY 1, 2
    )
    SELECT a.id, a.name, b.bucket, COALESCE(c.cnt, 0)
    FROM aliases a
    CROSS JOIN buckets b
    LEFT JOIN counts c ON c.alias_name = a.name AND c.bucket = b.bucket
    ORDER BY a.id, b.bucket
  |])

truncateRequests :: Connection -> IO ()
truncateRequests conn =
  void $ execute_ conn "TRUNCATE TABLE llm_requests"

truncateRequestsOlderThan :: Connection -> Text -> IO ()
truncateRequestsOlderThan conn interval =
  void $ execute conn "DELETE FROM llm_requests WHERE created_at < NOW() - ?::interval" (Only interval)
