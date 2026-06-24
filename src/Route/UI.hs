{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.UI (uiRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmRequest(..), LlmAlias(..), AliasUsage(..), getRecentRequests, getRecentRequestsFiltered, getRequest, countRequests, countRequestsFiltered, truncateRequests, getAliasesWithUsage)
import Common (icon, showT, maybeDash, basePage, queryParamDefault)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP


fmtLatency :: Double -> T.Text
fmtLatency ms
  | ms >= 1000 = T.pack (show (fromIntegral (round (ms / 100) :: Int) / 10)) <> "s"
  | otherwise  = showT (round ms :: Int) <> "ms"

rateLimitSection :: [AliasUsage] -> Html ()
rateLimitSection aliasUsages =
  if null aliasUsages
    then ""
    else div_ [class_ "rate-limits"] $ do
      h2_ (icon "ph-speedometer" >> " Rate Limits (rolling 24h)")
      div_ [class_ "rate-limit-grid"] $ mapM_ rateLimitCard aliasUsages

rateLimitCard :: AliasUsage -> Html ()
rateLimitCard u = do
  let a = auAlias u
      reqCount = auRequestCount u
      tokCount = auTokenCount u
  div_ [class_ "rate-limit-card"] $ do
    div_ [class_ "rate-limit-name"] (code_ (toHtml (laName a)))
    limitBar "Requests" reqCount (laDailyRequestLimit a)
    limitBar "Tokens" tokCount (laDailyTokenLimit a)

limitBar :: T.Text -> Int -> Maybe Int -> Html ()
limitBar label count mlim = case mlim of
  Just lim | lim > 0 -> do
    let pct = count * 100 `div` lim
        pctTxt = showT pct <> "%"
        widthPct = min 100 pct
        barClass = if pct >= 100 then "bar-full" else if pct >= 80 then "bar-warn" else ""
    div_ [class_ "limit-bar"] $ do
      div_ [class_ "limit-bar-label"] $ do
        span_ (toHtml label)
        span_ [class_ "limit-bar-nums"] (toHtml (showT count <> " / " <> showT lim <> "  (" <> pctTxt <> ")"))
      div_ [class_ "limit-bar-track"] $
        div_ [class_ ("limit-bar-fill " <> barClass), style_ ("width:" <> showT widthPct <> "%")] ""
  _ -> do
    div_ [class_ "limit-bar"] $ do
      div_ [class_ "limit-bar-label"] $ do
        span_ (toHtml label)
        span_ [class_ "limit-bar-nums"] (toHtml (showT count <> " / ∞"))

uiRoutes :: AppEnv -> ScottyM ()
uiRoutes env = do
  get "/ui/" $ do
    pageNum <- queryParamDefault "page" 1
    sortBy <- queryParamDefault "sort_by" ("created_at" :: T.Text)
    sortDir <- queryParamDefault "sort_dir" ("desc" :: T.Text)
    searchField <- queryParamDefault "search_field" ("any" :: T.Text)
    searchQuery <- queryParamDefault "search_query" ("" :: T.Text)
    duration <- queryParamDefault "duration" ("" :: T.Text)
    let perPage = 25
    let offset = (pageNum - 1) * perPage
    requests <- liftIO $ withPool env $ \conn ->
      getRecentRequestsFiltered conn perPage offset sortBy sortDir searchField searchQuery duration
    total <- liftIO $ withPool env $ \conn ->
      countRequestsFiltered conn searchField searchQuery duration
    let totalPages = max 1 ((total + perPage - 1) `div` perPage)
    aliasUsages <- liftIO $ withPool env $ \conn -> getAliasesWithUsage conn
    host <- header "Host"
    html $ renderText $ basePage "MixLLMProxy" $ page host requests pageNum totalPages total aliasUsages sortBy sortDir searchField searchQuery duration
  get "/ui/request/:id" $ do
    rid <- pathParam "id"
    mreq <- liftIO $ withPool env $ \conn -> getRequest conn rid
    case mreq of
      Just req -> html $ renderText $ basePage ("MixLLMProxy — Request #" <> showT (lrId req)) $ detailPage req
      Nothing -> html "not found"
  post "/ui/truncate" $ do
    liftIO $ withPool env $ \conn -> truncateRequests conn
    redirect "/ui/"

makeUrl :: Int -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
makeUrl pageNum sortBy sortDir searchField searchQuery duration =
  T.concat
    [ "/ui/?page=", showT pageNum
    , "&sort_by=", sortBy
    , "&sort_dir=", sortDir
    , "&search_field=", searchField
    , "&search_query=", searchQuery
    , "&duration=", duration
    ]

sortableHeader :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Html () -> Html ()
sortableHeader targetCol currentSortBy currentSortDir currentSearchField currentSearchQuery duration content = do
  let isSorted = currentSortBy == targetCol
      newDir = if isSorted && currentSortDir == "asc" then "desc" else "asc"
      queryStr = makeUrl 1 targetCol newDir currentSearchField currentSearchQuery duration
      indicator :: T.Text
      indicator = if isSorted
                    then if currentSortDir == "asc" then " ▲" else " ▼"
                    else ""
  th_ $ do
    a_ [href_ queryStr, style_ "color: inherit; text-decoration: none; display: inline-flex; align-items: center; gap: 4px;"] $ do
      content
      toHtml indicator

searchForm :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Html ()
searchForm searchField searchQuery sortBy sortDir duration =
  div_ [style_ "background: #161b22; border: 1px solid #21262d; border-radius: 8px; padding: 14px 18px; margin: 16px 0;"] $ do
    form_ [action_ "/ui/", method_ "get", style_ "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;"] $ do
      select_ [name_ "search_field", style_ "background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 6px 10px; color: #c9d1d9; font-size: 13px; cursor: pointer;"] $ do
        optionSelected "any" "Any text field"
        optionSelected "model" "Model"
        optionSelected "alias" "Alias"
        optionSelected "req_body" "Request Body"
        optionSelected "resp_body" "Response Body"
        optionSelected "endpoint" "Endpoint"
        optionSelected "status" "Status"
      input_ [type_ "text", name_ "search_query", value_ searchQuery, placeholder_ "Search query...", style_ "background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 6px 10px; color: #c9d1d9; font-size: 13px; width: 280px;"]
      span_ [style_ "color: #8b949e; font-size: 13px; margin-left: 4px;"] (icon "ph-clock" >> " Age:")
      input_ [type_ "text", name_ "duration", value_ duration, placeholder_ "All time (e.g. 10m, 1h)", style_ "background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 6px 10px; color: #c9d1d9; font-size: 13px; width: 160px;"]
      input_ [type_ "hidden", name_ "sort_by", value_ sortBy]
      input_ [type_ "hidden", name_ "sort_dir", value_ sortDir]
      button_ [type_ "submit", style_ "background: #21262d; color: #c9d1d9; border: 1px solid #30363d; border-radius: 6px; padding: 6px 14px; font-size: 13px; cursor: pointer; font-weight: 600;"] "Filter"
      a_ [href_ "/ui/", class_ "btn-cancel", style_ "text-decoration: none; line-height: 1.8; text-align: center; font-size: 13px;"] "Clear"
    div_ [style_ "display: flex; gap: 6px; align-items: center; margin-top: 8px; font-size: 12px; color: #8b949e;"] $ do
      span_ "Quick age:"
      let quickLink :: T.Text -> T.Text -> Html ()
          quickLink label dur =
            let active = duration == dur
                bg = if active then "#58a6ff" else "#21262d"
                fg = if active then "#0d1117" else "#c9d1d9"
                border = if active then "1px solid #58a6ff" else "1px solid #30363d"
                url = makeUrl 1 sortBy sortDir searchField searchQuery dur
            in a_ [href_ url, style_ ("background: " <> bg <> "; color: " <> fg <> "; border: " <> border <> "; border-radius: 4px; padding: 2px 8px; text-decoration: none; font-weight: 500; transition: all 0.2s;")] (toHtml label)
      quickLink "All" ""
      quickLink "10m" "10m"
      quickLink "1h" "1h"
      quickLink "24h" "24h"
      quickLink "7d" "7d"
  where
    optionSelected :: T.Text -> T.Text -> Html ()
    optionSelected val label =
      let attrs = [value_ val] ++ [selected_ "selected" | searchField == val]
      in option_ attrs (toHtml label)

page :: Maybe TL.Text -> [LlmRequest] -> Int -> Int -> Int -> [AliasUsage] -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Html ()
page host requests pageNum totalPages totalResults aliasUsages sortBy sortDir searchField searchQuery duration = do
    div_ [class_ "header-row"] $ do
      h1_ $ a_ [href_ "/ui/", style_ "color: inherit; text-decoration: none;"] "🔭 MixLLMProxy"
      a_ [href_ "/ui/aliases", class_ "nav-btn"] (icon "gear" >> " Aliases")
      a_ [href_ "/ui/aliases/info", class_ "nav-btn"] (icon "info" >> " Info")
      let base = fromMaybe "localhost" (TL.toStrict <$> host)
          endpoint = T.concat ["http://", base, "/api/openai/v1/chat/completions"]
      code_ [class_ "endpoint"] (icon "ph-link" >> " Endpoint: " >> toHtml endpoint)
      form_ [action_ "/ui/truncate", method_ "post", class_ "form-inline"] $
        button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Wipe all logged requests?')"] (icon "ph-trash" >> " Truncate")
    rateLimitSection aliasUsages
    searchForm searchField searchQuery sortBy sortDir duration
    pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration
    table_ [class_ "requests"] $ do
      thead_ $ do
        tr_ $ do
          let hdr col label = sortableHeader col sortBy sortDir searchField searchQuery duration label
          hdr "id" "#"
          hdr "created_at" (icon "ph-clock" >> " Time")
          hdr "input_chars" (icon "ph-download-simple" >> " Input Chars")
          hdr "output_chars" (icon "ph-upload-simple" >> " Output Chars")
          hdr "model" (icon "ph-cpu" >> " Model")
          hdr "alias_name" (icon "ph-tag" >> " Alias")
          hdr "prompt_tokens" (icon "ph-arrow-line-down" >> " In Tok")
          hdr "completion_tokens" (icon "ph-arrow-line-up" >> " Out Tok")
          hdr "total_tokens" (icon "ph-equals" >> " Total")
          hdr "response_status" (icon "ph-check-circle" >> " Status")
          hdr "latency_ms" (icon "ph-timer" >> " Duration")
          hdr "request_body" (icon "ph-paper-plane-right" >> " Request")
          hdr "response_body" (icon "ph-paper-plane-left" >> " Response")
      tbody_ $ mapM_ requestRow requests
    pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration
    script_ (refreshScript <> clickScript)

requestRow :: LlmRequest -> Html ()
requestRow r = tr_ [class_ "req-row", data_ "href" ("/ui/request/" <> showT (lrId r))] $ do
  td_ (toHtml (showT (lrId r)))
  td_ [class_ "time"] (toHtml (T.take 19 (showT (lrCreatedAt r))))
  td_ [class_ "len"] (toHtml (maybeDash (fmap T.length (lrRequestBody r))))
  td_ [class_ "len"] (toHtml (maybeDash (fmap T.length (lrResponseBody r))))
  td_ [class_ "model"] (toHtml (fromMaybe "-" (lrModel r)))
  td_ [class_ "alias"] (toHtml (fromMaybe "-" (lrAliasName r)))
  td_ [class_ "len"] (toHtml (maybeDash (lrPromptTokens r)))
  td_ [class_ "len"] (toHtml (maybeDash (lrCompletionTokens r)))
  td_ [class_ "len"] (toHtml (maybeDash (lrTotalTokens r)))
  td_ [class_ (statusClass (lrResponseStatus r))] (toHtml (maybeDash (lrResponseStatus r)))
  td_ [class_ "latency"] (toHtml (maybe "-" fmtLatency (lrLatencyMs r)))
  td_ [class_ "req"] (toHtml (fromMaybe "-" (clip 80 (lrRequestBody r))))
  td_ [class_ "resp"] (toHtml (fromMaybe "-" (clip 80 (lrResponseBody r))))

detailCell :: T.Text -> T.Text -> Html () -> Html ()
detailCell iconName label value = div_ [class_ "detail-cell"] $ do
  span_ [class_ "detail-label"] (icon iconName >> " " >> toHtml label)
  span_ [class_ "detail-value"] value

detailPage :: LlmRequest -> Html ()
detailPage r = do
    h1_ (toHtml ("Request #" <> showT (lrId r)))
    div_ [class_ "detail-grid"] $ do
      detailCell "ph-fingerprint" "ID" (toHtml (showT (lrId r)))
      detailCell "ph-clock" "Time" (toHtml (showT (lrCreatedAt r)))
      detailCell "ph-arrow-down-up" "Method" (toHtml (lrMethod r))
      detailCell "ph-link" "Endpoint" (toHtml (lrEndpoint r))
      detailCell "ph-cpu" "Model" (toHtml (fromMaybe "-" (lrModel r)))
      detailCell "ph-tag" "Alias" (toHtml (fromMaybe "-" (lrAliasName r)))
      detailCell "ph-arrow-line-down" "Prompt tokens" (toHtml (maybeDash (lrPromptTokens r)))
      detailCell "ph-arrow-line-up" "Completion tokens" (toHtml (maybeDash (lrCompletionTokens r)))
      detailCell "ph-equals" "Total tokens" (toHtml (maybeDash (lrTotalTokens r)))
      detailCell "ph-check-circle" "Status" (toHtml (maybeDash (lrResponseStatus r)))
      detailCell "ph-timer" "Duration" (toHtml (maybe "-" fmtLatency (lrLatencyMs r)))
    script_ [src_ "https://cdn.jsdelivr.net/npm/json-formatter-js@2"] ("" :: T.Text)
    div_ [class_ "bodies"] $ do
      bodyPanel "Request Body" "req-body" (lrRequestBody r)
      bodyPanel "Response Body" "resp-body" (lrResponseBody r)
    script_ detailJsonScript

bodyPanel :: T.Text -> T.Text -> Maybe T.Text -> Html ()
bodyPanel heading elId body = div_ [class_ "body-col"] $ do
  h2_ (toHtml heading)
  div_ [class_ "body json-tree", id_ elId] (toHtml (fromMaybe "" (fmap prettyJson body)))

prettyJson :: T.Text -> T.Text
prettyJson t = case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) of
  Just (v :: A.Value) -> TL.toStrict $ TLE.decodeUtf8 $ AP.encodePretty v
  Nothing -> t

clip :: Int -> Maybe T.Text -> Maybe T.Text
clip n (Just t) | T.length t > n = Just (T.take n t <> "...")
clip _ v = v

statusClass :: Maybe Int -> T.Text
statusClass (Just s)
  | s >= 200 && s < 300 = "status-ok"
  | s >= 400 && s < 500 = "status-err"
  | s >= 500 = "status-fail"
  | otherwise = ""
statusClass Nothing = ""

refreshScript :: T.Text
refreshScript = "setTimeout(function() { window.location.reload(); }, 10000);"

clickScript :: T.Text
clickScript = T.intercalate "\n"
  [ "document.querySelectorAll('.req-row').forEach(function(row){"
  , "  row.addEventListener('click',function(){"
  , "    window.location=this.getAttribute('data-href');"
  , "  });"
  , "});"
  ]

detailJsonScript :: T.Text
detailJsonScript = T.intercalate "\n"
  [ "(function(){"
  , "  if (typeof JSONFormatter === 'undefined') return;"
  , "  function parseJson(raw) {"
  , "    try { return JSON.parse(raw); } catch(e) { return null; }"
  , "  }"
  , "  function renderJson(el, json) {"
  , "    el.textContent = '';"
  , "    el.appendChild(new JSONFormatter(json, Infinity, { theme: 'dark' }).render());"
  , "  }"
  , "  ['req-body', 'resp-body'].forEach(function(id) {"
  , "    var el = document.getElementById(id);"
  , "    if (!el) return;"
  , "    var raw = el.textContent.trim();"
  , "    if (!raw) { el.textContent = '(none)'; return; }"
  , "    var json = parseJson(raw);"
  , "    if (json) return renderJson(el, json);"
  , "    if (raw.indexOf('data: ') !== 0) return;"
  , "    var chunks = raw.split(/\\n\\ndata: /);"
  , "    if (chunks.length < 2) return;"
  , "    var parsed = [];"
  , "    chunks.forEach(function(chunk) {"
  , "      var line = chunk.trim();"
  , "      if (line.indexOf('data: ') === 0) line = line.slice(6);"
  , "      if (line === '[DONE]') return;"
  , "      var obj = parseJson(line);"
  , "      if (obj) parsed.push(obj);"
  , "    });"
  , "    if (parsed.length) renderJson(el, parsed);"
  , "  });"
  , "})();"
  ]

pagination :: Int -> Int -> Int -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> Html ()
pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration = div_ [class_ "pagination"] $ do
  let prevUrl = makeUrl (pageNum - 1) sortBy sortDir searchField searchQuery duration
      nextUrl = makeUrl (pageNum + 1) sortBy sortDir searchField searchQuery duration
  if pageNum > 1
    then a_ [href_ prevUrl] (icon "caret-left" >> " Prev")
    else span_ [class_ "disabled"] (icon "caret-left" >> " Prev")
  span_ [class_ "page-info"] (toHtml ("Page " <> showT pageNum <> " of " <> showT totalPages <> " (" <> showT totalResults <> " total results)"))
  if pageNum < totalPages
    then a_ [href_ nextUrl] ("Next " >> icon "caret-right")
    else span_ [class_ "disabled"] ("Next " >> icon "caret-right")


