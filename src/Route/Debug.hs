{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Route.Debug (debugRoutes) where

import Web.Scotty
import Network.HTTP.Types.Status (status404)
import AppEnv (AppEnv(..), GlobalSettings(..), withPool)
import DB
  ( LlmRequest(..), LlmEndpoint(..), LlmAlias(..), AliasUsage(..)
  , getRecentRequestsFiltered, getRequest, countRequests, countRequestsFiltered
  , getAliasesWithUsage, getEndpoints
  )
import Common (queryParamDefault)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Text as T

data DebugParam = DebugParam
  { paramName :: T.Text
  , paramType :: T.Text
  , paramRequired :: Bool
  , paramDefault :: Maybe T.Text
  , paramDescription :: T.Text
  , paramExample :: Maybe T.Text
  } deriving (Generic)

instance A.ToJSON DebugParam where
  toJSON p = A.object
    [ "name" A..= paramName p
    , "type" A..= paramType p
    , "required" A..= paramRequired p
    , "default" A..= paramDefault p
    , "description" A..= paramDescription p
    , "example" A..= paramExample p
    ]

data DebugEndpointSpec = DebugEndpointSpec
  { specMethod :: T.Text
  , specPath :: T.Text
  , specDescription :: T.Text
  , specParams :: [DebugParam]
  , specExample :: T.Text
  } deriving (Generic)

instance A.ToJSON DebugEndpointSpec where
  toJSON s = A.object
    [ "method" A..= specMethod s
    , "path" A..= specPath s
    , "description" A..= specDescription s
    , "params" A..= specParams s
    , "example" A..= specExample s
    ]

debugEndpointSpecs :: [DebugEndpointSpec]
debugEndpointSpecs =
  [ DebugEndpointSpec "GET" "/debug/"
      "Self-documenting catalog of all debug endpoints. Fetch this first."
      [] "curl http://localhost:8015/debug/"
  , DebugEndpointSpec "GET" "/debug/summary"
      "Quick health snapshot: global settings, request counts, recent errors."
      [] "curl http://localhost:8015/debug/summary"
  , DebugEndpointSpec "GET" "/debug/settings"
      "Current global proxy settings (pause state, rate limit)."
      [] "curl http://localhost:8015/debug/settings"
  , DebugEndpointSpec "GET" "/debug/requests"
      "List logged LLM requests with filtering, sorting, and pagination."
      [ DebugParam "page" "integer" False (Just "1") "Page number (25 per page)." (Just "1")
      , DebugParam "sort_by" "string" False (Just "created_at")
          "Sort column: id, created_at, model, alias_name, prompt_tokens, completion_tokens, total_tokens, response_status, latency_ms, request_body, response_body, input_chars, output_chars."
          (Just "created_at")
      , DebugParam "sort_dir" "string" False (Just "desc") "Sort direction: asc or desc." (Just "desc")
      , DebugParam "search_field" "string" False (Just "any")
          "Field to search: any, model, alias, req_body, resp_body, endpoint, status."
          (Just "any")
      , DebugParam "search_query" "string" False (Just "") "Search text (ILIKE match)." (Just "429")
      , DebugParam "duration" "string" False (Just "")
          "Age filter. Examples: 10m, 1h, 24h, 7d. Empty = all time."
          (Just "1h")
      ]
      "curl 'http://localhost:8015/debug/requests?duration=1h&search_field=status&search_query=429'"
  , DebugEndpointSpec "GET" "/debug/requests/:id"
      "Full detail for a single request, including complete request/response bodies."
      [ DebugParam "id" "integer" True Nothing "Request ID from the list endpoint." (Just "42")
      ]
      "curl http://localhost:8015/debug/requests/42"
  , DebugEndpointSpec "GET" "/debug/aliases"
      "All aliases with endpoint mapping, rate limits, and rolling 24h usage."
      [] "curl http://localhost:8015/debug/aliases"
  , DebugEndpointSpec "GET" "/debug/endpoints"
      "Configured downstream endpoints. API keys are redacted."
      [] "curl http://localhost:8015/debug/endpoints"
  ]

debugCatalog :: A.Value
debugCatalog = A.object
  [ "name" A..= ("MixLLMProxy Debug API" :: T.Text)
  , "purpose" A..= ("Inspect proxy state and logged requests without the UI. Read-only." :: T.Text)
  , "usage" A..= ("Fetch GET /debug/ first, then call endpoints as needed." :: T.Text)
  , "endpoints" A..= debugEndpointSpecs
  ]

redactApiKey :: T.Text -> T.Text
redactApiKey key
  | T.null key = ""
  | T.length key <= 8 = "***"
  | otherwise = "..." <> T.takeEnd 4 key

requestSummaryJson :: LlmRequest -> A.Value
requestSummaryJson r = A.object
  [ "id" A..= lrId r
  , "created_at" A..= lrCreatedAt r
  , "endpoint" A..= lrEndpoint r
  , "method" A..= lrMethod r
  , "model" A..= lrModel r
  , "alias_name" A..= lrAliasName r
  , "response_status" A..= lrResponseStatus r
  , "latency_ms" A..= lrLatencyMs r
  , "prompt_tokens" A..= lrPromptTokens r
  , "completion_tokens" A..= lrCompletionTokens r
  , "total_tokens" A..= lrTotalTokens r
  , "request_body" A..= clipBody 200 (lrRequestBody r)
  , "response_body" A..= clipBody 200 (lrResponseBody r)
  , "input_chars" A..= bodyLen (lrRequestBody r)
  , "output_chars" A..= bodyLen (lrResponseBody r)
  ]

requestDetailJson :: LlmRequest -> A.Value
requestDetailJson r = A.object
  [ "id" A..= lrId r
  , "created_at" A..= lrCreatedAt r
  , "endpoint" A..= lrEndpoint r
  , "method" A..= lrMethod r
  , "model" A..= lrModel r
  , "alias_name" A..= lrAliasName r
  , "response_status" A..= lrResponseStatus r
  , "latency_ms" A..= lrLatencyMs r
  , "prompt_tokens" A..= lrPromptTokens r
  , "completion_tokens" A..= lrCompletionTokens r
  , "total_tokens" A..= lrTotalTokens r
  , "request_body" A..= lrRequestBody r
  , "response_body" A..= lrResponseBody r
  , "input_chars" A..= bodyLen (lrRequestBody r)
  , "output_chars" A..= bodyLen (lrResponseBody r)
  ]

endpointJson :: LlmEndpoint -> A.Value
endpointJson e = A.object
  [ "id" A..= leId e
  , "name" A..= leName e
  , "url" A..= leUrl e
  , "api_key" A..= redactApiKey (leApiKey e)
  , "created_at" A..= leCreatedAt e
  ]

aliasJson :: LlmAlias -> A.Value
aliasJson a = A.object
  [ "id" A..= laId a
  , "name" A..= laName a
  , "endpoint_id" A..= laEndpointId a
  , "endpoint_name" A..= laEndpointName a
  , "model" A..= laModel a
  , "daily_token_limit" A..= laDailyTokenLimit a
  , "daily_request_limit" A..= laDailyRequestLimit a
  , "created_at" A..= laCreatedAt a
  ]

aliasUsageJson :: AliasUsage -> A.Value
aliasUsageJson u = A.object
  [ "alias" A..= aliasJson (auAlias u)
  , "requests_24h" A..= auRequestCount u
  , "tokens_24h" A..= auTokenCount u
  , "chars_24h" A..= auCharCount u
  ]

settingsJson :: GlobalSettings -> A.Value
settingsJson s = A.object
  [ "paused" A..= gsPaused s
  , "slow_limit_per_sec" A..= gsSlowLimit s
  ]

clipBody :: Int -> Maybe T.Text -> Maybe T.Text
clipBody n (Just t)
  | T.length t > n = Just (T.take n t <> "...")
clipBody _ v = v

bodyLen :: Maybe T.Text -> Maybe Int
bodyLen = fmap T.length

isErrorStatus :: Maybe Int -> Bool
isErrorStatus (Just s) = s >= 400
isErrorStatus Nothing = False

debugRoutes :: AppEnv -> ScottyM ()
debugRoutes env = do
  get "/debug/" $ json debugCatalog

  get "/debug/summary" $ do
    settings <- liftIO $ readIORef (envSettings env)
    (total, recent1h, recentErrors1h) <- liftIO $ withPool env $ \conn -> do
      totalCount <- countRequests conn
      count1h <- countRequestsFiltered conn "any" "" "1h"
      recent <- getRecentRequestsFiltered conn 500 0 "created_at" "desc" "any" "" "1h"
      let errors = length $ filter (isErrorStatus . lrResponseStatus) recent
      pure (totalCount, count1h, errors)
    json $ A.object
      [ "paused" A..= gsPaused settings
      , "slow_limit_per_sec" A..= gsSlowLimit settings
      , "total_requests" A..= total
      , "requests_last_1h" A..= recent1h
      , "errors_last_1h" A..= recentErrors1h
      ]

  get "/debug/settings" $ do
    settings <- liftIO $ readIORef (envSettings env)
    json $ settingsJson settings

  get "/debug/requests" $ do
    pageNum <- queryParamDefault "page" 1
    sortBy <- queryParamDefault "sort_by" ("created_at" :: T.Text)
    sortDir <- queryParamDefault "sort_dir" ("desc" :: T.Text)
    searchField <- queryParamDefault "search_field" ("any" :: T.Text)
    searchQuery <- queryParamDefault "search_query" ("" :: T.Text)
    duration <- queryParamDefault "duration" ("" :: T.Text)
    let perPage = 25
        offset = (pageNum - 1) * perPage
    (requests, total) <- liftIO $ withPool env $ \conn -> do
      reqs <- getRecentRequestsFiltered conn perPage offset sortBy sortDir searchField searchQuery duration
      cnt <- countRequestsFiltered conn searchField searchQuery duration
      pure (reqs, cnt)
    let totalPages = max 1 ((total + perPage - 1) `div` perPage)
    json $ A.object
      [ "page" A..= pageNum
      , "per_page" A..= perPage
      , "total" A..= total
      , "total_pages" A..= totalPages
      , "requests" A..= map requestSummaryJson requests
      ]

  get "/debug/requests/:id" $ do
    rid <- pathParam "id"
    mreq <- liftIO $ withPool env $ \conn -> getRequest conn rid
    case mreq of
      Just req -> json $ requestDetailJson req
      Nothing -> do
        status status404
        json $ A.object ["error" A..= ("request not found" :: T.Text), "id" A..= rid]

  get "/debug/aliases" $ do
    usages <- liftIO $ withPool env $ \conn -> getAliasesWithUsage conn
    json $ A.object ["aliases" A..= map aliasUsageJson usages]

  get "/debug/endpoints" $ do
    endpoints <- liftIO $ withPool env $ \conn -> getEndpoints conn
    json $ A.object ["endpoints" A..= map endpointJson endpoints]