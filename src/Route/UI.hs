{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.UI (uiRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv(..), GlobalSettings(..), withPool)
import DB (LlmRequest(..), LlmAlias(..), AliasUsage(..), getRecentRequestsFiltered, getRequest, countRequests, countRequestsFiltered, truncateRequests, truncateRequestsOlderThan, parseDuration, getAliasesWithUsage)
import ChartJson (chartPollSeconds, chartSubtitle, loadChartJson)
import RequestEventsJson (requestSoundPollSeconds, loadRequestEventsJson)
import Data.IORef (readIORef, modifyIORef')
import Text.Read (readMaybe)
import Common
  ( icon, showT, showWithCommas, showCompact, maybeCompact, maybeTextLenCompact, maybeDash, basePage
  , queryParamDefault, formParamDefault, aliasBadge, aliasBadgeWithEdit, pageHeader, hostFromHeader
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP


fmtLatency :: Double -> T.Text
fmtLatency ms
  | ms >= 1000 = T.pack (show (fromIntegral (round (ms / 100) :: Int) / 10)) <> "s"
  | otherwise  = showT (round ms :: Int) <> "ms"

data MetricView = Tokens | Chars deriving (Eq)

parseMetric :: T.Text -> MetricView
parseMetric "chars" = Chars
parseMetric _ = Tokens

metricParam :: MetricView -> T.Text
metricParam Tokens = "tokens"
metricParam Chars = "chars"

rateLimitSection :: T.Text -> T.Text -> T.Text -> MetricView -> [AliasUsage] -> Html ()
rateLimitSection sortBy sortDir duration metric aliasUsages =
  if null aliasUsages
    then ""
    else let (active, unused) = partition ((> 0) . auRequestCount) aliasUsages
         in div_ [class_ "rate-limits"] $ do
      h2_ (icon "ph-speedometer" >> " Rate Limits (rolling 24h)")
      p_ [class_ "rate-limits-subtitle"] (toHtml chartSubtitle)
      div_ [class_ "rate-limit-grid"] $ do
        mapM_ (rateLimitCard sortBy sortDir duration metric) active
        unless (null unused) $ unusedAliasesCard unused

rateLimitAliasHeader :: Maybe T.Text -> LlmAlias -> Html ()
rateLimitAliasHeader mFilter a =
  aliasBadgeWithEdit mFilter (laId a) (laName a)

unusedAliasesCard :: [AliasUsage] -> Html ()
unusedAliasesCard unused =
  div_ [class_ "rate-limit-card rate-limit-card-unused"] $ do
    div_ [class_ "rate-limit-name"] $
      span_ [class_ "unused-card-label"] "Unused"
    div_ [class_ "unused-aliases-badges"] $
      mapM_ (rateLimitAliasHeader Nothing . auAlias) unused

rateLimitCard :: T.Text -> T.Text -> T.Text -> MetricView -> AliasUsage -> Html ()
rateLimitCard sortBy sortDir duration metric u = do
  let a = auAlias u
      reqCount = auRequestCount u
      tokCount = auTokenCount u
      filterUrl = makeUrl 1 sortBy sortDir "alias" (laName a) duration metric
  div_ [class_ "rate-limit-card"] $ do
    div_ [class_ "rate-limit-name"] $
      rateLimitAliasHeader (Just filterUrl) a
    limitBar "Requests" reqCount (laDailyRequestLimit a)
    limitBar "Tokens" tokCount (laDailyTokenLimit a)
    div_ [class_ "alias-chart-wrap"] $
      canvas_ [class_ "alias-chart", id_ ("chart-" <> showT (laId a))] ""

fmtLimitNum :: Int -> T.Text
fmtLimitNum n
  | n >= 100000 = showCompact n
  | otherwise   = showWithCommas n

limitBarShell :: T.Text -> T.Text -> Maybe T.Text -> Maybe (Int, T.Text) -> Html ()
limitBarShell label nums mTitle mFill = div_ [class_ "limit-bar"] $ do
  div_ [class_ "limit-bar-label"] $ do
    span_ [class_ "limit-bar-name"] (toHtml label)
    case mTitle of
      Just titleTxt -> span_ [class_ "limit-bar-nums", title_ titleTxt] (toHtml nums)
      Nothing -> span_ [class_ "limit-bar-nums"] (toHtml nums)
  case mFill of
    Just (widthPct, barClass) ->
      div_ [class_ "limit-bar-track"] $
        div_ [class_ ("limit-bar-fill " <> barClass), style_ ("width:" <> showT widthPct <> "%")] ""
    Nothing -> pure ()

limitBar :: T.Text -> Int -> Maybe Int -> Html ()
limitBar label count mlim = case mlim of
  Just lim | lim > 0 ->
    let pct = count * 100 `div` lim
        pctTxt = showT pct <> "%"
        widthPct = min 100 pct
        barClass = if pct >= 100 then "bar-full" else if pct >= 80 then "bar-warn" else ""
        numsTxt = fmtLimitNum count <> " / " <> fmtLimitNum lim <> " (" <> pctTxt <> ")"
        titleTxt = showWithCommas count <> " / " <> showWithCommas lim <> " (" <> pctTxt <> ")"
    in limitBarShell label numsTxt (Just titleTxt) (Just (widthPct, barClass))
  _ -> limitBarShell label (fmtLimitNum count <> " / ∞") Nothing Nothing

renderModel :: Maybe T.Text -> Html ()
renderModel = toHtml . fromMaybe "-"

renderAlias :: Maybe T.Text -> Html ()
renderAlias = maybe (toHtml ("-" :: T.Text)) aliasBadge

renderLatency :: Maybe Double -> Html ()
renderLatency = toHtml . maybe "-" fmtLatency

uiRoutes :: AppEnv -> ScottyM ()
uiRoutes env = do
  get "/ui/api/alias-charts" $ do
    val <- liftIO $ withPool env loadChartJson
    json val

  get "/ui/api/request-events" $ do
    val <- liftIO $ withPool env loadRequestEventsJson
    json val

  get "/ui/" $ do
    pageNum <- queryParamDefault "page" 1
    sortBy <- queryParamDefault "sort_by" ("created_at" :: T.Text)
    sortDir <- queryParamDefault "sort_dir" ("desc" :: T.Text)
    searchField <- queryParamDefault "search_field" ("any" :: T.Text)
    searchQuery <- queryParamDefault "search_query" ("" :: T.Text)
    duration <- queryParamDefault "duration" ("" :: T.Text)
    metricTxt <- queryParamDefault "metric" ("tokens" :: T.Text)
    let metric = parseMetric metricTxt
        perPage = 25
    let offset = (pageNum - 1) * perPage
    requests <- liftIO $ withPool env $ \conn ->
      getRecentRequestsFiltered conn perPage offset sortBy sortDir searchField searchQuery duration
    total <- liftIO $ withPool env $ \conn ->
      countRequestsFiltered conn searchField searchQuery duration
    let totalPages = max 1 ((total + perPage - 1) `div` perPage)
    aliasUsages <- liftIO $ withPool env $ \conn -> getAliasesWithUsage conn
    settings <- liftIO $ readIORef (envSettings env)
    host <- header "Host"
    html $ renderText $ basePage "MixLLMProxy" $ page host requests pageNum totalPages total aliasUsages sortBy sortDir searchField searchQuery duration metric settings
  get "/ui/request/:id" $ do
    rid <- pathParam "id"
    mreq <- liftIO $ withPool env $ \conn -> getRequest conn rid
    case mreq of
      Just req -> html $ renderText $ basePage ("MixLLMProxy — Request #" <> showT (lrId req)) $ detailPage req
      Nothing -> html "not found"
  post "/ui/truncate" $ do
    liftIO $ withPool env $ \conn -> truncateRequests conn
    redirect "/ui/"
  post "/ui/truncate-older" $ do
    let interval = fromMaybe "1 hours" (parseDuration "1h")
    liftIO $ withPool env $ \conn -> truncateRequestsOlderThan conn interval
    redirect "/ui/"
  post "/ui/global-settings/toggle-pause" $ do
    liftIO $ modifyIORef' (envSettings env) $ \s -> s { gsPaused = not (gsPaused s) }
    redirect "/ui/"
  post "/ui/global-settings/set-slow-limit" $ do
    limitStr :: T.Text <- formParamDefault "slow_limit" ""
    let limit = if T.null (T.strip limitStr)
                  then Nothing
                  else readMaybe (T.unpack limitStr)
    liftIO $ modifyIORef' (envSettings env) $ \s -> s { gsSlowLimit = limit }
    redirect "/ui/"

makeUrl :: Int -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> MetricView -> T.Text
makeUrl pageNum sortBy sortDir searchField searchQuery duration metric =
  T.concat
    [ "/ui/?page=", showT pageNum
    , "&sort_by=", sortBy
    , "&sort_dir=", sortDir
    , "&search_field=", searchField
    , "&search_query=", searchQuery
    , "&duration=", duration
    , "&metric=", metricParam metric
    ]

sortableHeader :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> MetricView -> Html () -> Html ()
sortableHeader targetCol currentSortBy currentSortDir currentSearchField currentSearchQuery duration metric content = do
  let isSorted = currentSortBy == targetCol
      newDir = if isSorted && currentSortDir == "asc" then "desc" else "asc"
      queryStr = makeUrl 1 targetCol newDir currentSearchField currentSearchQuery duration metric
      indicator :: T.Text
      indicator = if isSorted
                    then if currentSortDir == "asc" then " ▲" else " ▼"
                    else ""
  th_ $ do
    a_ [href_ queryStr, class_ "sort-link"] $ do
      content
      toHtml indicator

searchForm :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> MetricView -> Html ()
searchForm searchField searchQuery sortBy sortDir duration metric =
  div_ [class_ "search-card"] $ do
    form_ [action_ "/ui/", method_ "get", class_ "search-form"] $ do
      div_ [class_ "search-fields-row"] $ do
        select_ [name_ "search_field", class_ "input-select"] $ do
          optionSelected "any" "Any text field"
          optionSelected "model" "Model"
          optionSelected "alias" "Alias"
          optionSelected "req_body" "Request Body"
          optionSelected "resp_body" "Response Body"
          optionSelected "endpoint" "Endpoint"
          optionSelected "status" "Status"
        input_ [type_ "text", name_ "search_query", value_ searchQuery, placeholder_ "Search query...", class_ "input search-query-input"]
        span_ [class_ "label-text"] (icon "ph-clock" >> " Age:")
        input_ [type_ "text", name_ "duration", value_ duration, placeholder_ "All time (e.g. 10m, 1h)", class_ "input search-age-input"]
        input_ [type_ "hidden", name_ "sort_by", value_ sortBy]
        input_ [type_ "hidden", name_ "sort_dir", value_ sortDir]
        input_ [type_ "hidden", name_ "metric", value_ (metricParam metric)]
        button_ [type_ "submit", class_ "btn"] "Filter"
        a_ [href_ "/ui/", class_ "btn-cancel"] "Clear"
    div_ [class_ "quick-filter-row"] $ do
      span_ "Show:"
      let metricLink :: T.Text -> MetricView -> Html ()
          metricLink label m =
            let active = metric == m
                linkClass = if active then "quick-link quick-link-active" else "quick-link quick-link-inactive"
                url = makeUrl 1 sortBy sortDir searchField searchQuery duration m
            in a_ [href_ url, class_ linkClass] (toHtml label)
      metricLink "Tokens" Tokens
      metricLink "Chars" Chars
    div_ [class_ "quick-filter-row"] $ do
      span_ "Quick age:"
      let quickLink :: T.Text -> T.Text -> Html ()
          quickLink label dur =
            let active = duration == dur
                linkClass = if active then "quick-link quick-link-active" else "quick-link quick-link-inactive"
                url = makeUrl 1 sortBy sortDir searchField searchQuery dur metric
            in a_ [href_ url, class_ linkClass] (toHtml label)
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

settingsSection :: GlobalSettings -> Html ()
settingsSection s = do
  let paused = gsPaused s
      mSlow = gsSlowLimit s
      isSlowActive = case mSlow of { Just _ -> True; Nothing -> False }
      slowValText = case mSlow of { Just v -> showT v; Nothing -> "2.0" }
  div_ [class_ "settings-row"] $ do
    
    -- Row 1: Global Pause
    div_ [class_ "settings-card settings-card-pause"] $ do
      div_ [class_ "flex-row"] $ do
        span_ [class_ "icon-blue"] (icon "ph-power")
        strong_ "Global Pause"
        if paused
          then span_ [class_ "badge badge-red"] "PAUSED"
          else span_ [class_ "badge badge-green"] "ACTIVE"
        span_ [class_ "desc-text"] "— Temporarily pause all proxy traffic"
      
      form_ [action_ "/ui/global-settings/toggle-pause", method_ "post", class_ "form-inline"] $ do
        if paused
          then button_ [type_ "submit", class_ "btn-green"] (icon "ph-play" >> " Resume API")
          else button_ [type_ "submit", class_ "btn-red"] (icon "ph-pause" >> " Pause API")

    -- Row 2: Speed Limiter
    div_ [class_ "settings-card settings-card-speed"] $ do
      div_ [class_ "flex-row"] $ do
        span_ [class_ "icon-yellow"] (icon "ph-gauge")
        strong_ "Speed Limiter"
        if isSlowActive
          then span_ [class_ "badge badge-yellow"] (toHtml ("SLOWED (" <> slowValText <> "/s)"))
          else span_ [class_ "badge badge-green"] "UNLIMITED"
        span_ [class_ "desc-text"] "— Enforce rate limits across all model endpoints"

      div_ [class_ "flex-row-gap6"] $ do
        form_ [action_ "/ui/global-settings/set-slow-limit", method_ "post", class_ "flex-form"] $ do
          input_ [type_ "text", name_ "slow_limit", value_ (if isSlowActive then slowValText else ""), placeholder_ "2.0", class_ "input-sm"]
          span_ [class_ "label-sm", style_ "margin-right: 4px;"] "req/s"
          button_ [type_ "submit", class_ "btn-sm"] "Set Limit"
        
        if isSlowActive
          then form_ [action_ "/ui/global-settings/set-slow-limit", method_ "post", class_ "btn-ghost"] $ do
            input_ [type_ "hidden", name_ "slow_limit", value_ ""]
            button_ [type_ "submit", class_ "btn-red"] "Disable"
          else ""

    soundSettingsCard

data SoundEventOption = SoundEventOption
  { seoId :: T.Text
  , seoLabel :: T.Text
  }

soundEventOptions :: [SoundEventOption]
soundEventOptions =
  [ SoundEventOption "new_request" "New"
  , SoundEventOption "completed" "Completed"
  , SoundEventOption "error" "Error"
  ]

soundEventCheckbox :: SoundEventOption -> Html ()
soundEventCheckbox (SoundEventOption eid label) =
  label_ [class_ "sound-event-option"] $ do
    input_ [type_ "checkbox", id_ ("sound-event-" <> eid), checked_, class_ "sound-event-item"]
    toHtml (" " <> label)

soundSettingsCard :: Html ()
soundSettingsCard =
  div_ [class_ "settings-card settings-card-sounds"] $ do
    div_ [class_ "flex-row"] $ do
      span_ [class_ "icon-purple"] (icon "ph-speaker-high")
      strong_ "Event Sounds"
      span_ [id_ "sound-status-badge", class_ "badge badge-green"] "ON"
      span_ [class_ "desc-text"] "— Synth alerts for new, completed, and failed requests"
    div_ [class_ "flex-row-gap6 sound-controls"] $ do
      label_ [class_ "sound-toggle-label"] $ do
        input_ [type_ "checkbox", id_ "sound-enabled", checked_]
        " Sounds on"
      label_ [class_ "sound-volume-label"] $ do
        span_ "Volume"
        input_ [type_ "range", id_ "sound-volume", class_ "sound-volume", min_ "0", max_ "100", value_ "25"]
      button_ [type_ "button", id_ "sound-test", class_ "btn-sm"] "Test"
    div_ [class_ "sound-event-filters", id_ "sound-event-filters"] $ do
      span_ [class_ "sound-event-label"] "Play:"
      label_ [class_ "sound-event-option"] $ do
        input_ [type_ "checkbox", id_ "sound-event-all", checked_]
        " All"
      mapM_ soundEventCheckbox soundEventOptions

page :: Maybe TL.Text -> [LlmRequest] -> Int -> Int -> Int -> [AliasUsage] -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> MetricView -> GlobalSettings -> Html ()
page host requests pageNum totalPages totalResults aliasUsages sortBy sortDir searchField searchQuery duration metric settings = do
    pageHeader (hostFromHeader host) (Just $ div_ [class_ "truncate-actions"] $ do
      form_ [action_ "/ui/truncate-older", method_ "post", class_ "form-inline"] $
        button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Delete all requests older than 1 hour?')"] (icon "ph-trash" >> " Truncate older than 1h")
      form_ [action_ "/ui/truncate", method_ "post", class_ "form-inline"] $
        button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Wipe all logged requests?')"] (icon "ph-trash" >> " Truncate"))
    rateLimitSection sortBy sortDir duration metric aliasUsages
    settingsSection settings
    searchForm searchField searchQuery sortBy sortDir duration metric
    pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration metric
    table_ [class_ "requests"] $ do
      thead_ $ do
        tr_ $ do
          let hdr col label = sortableHeader col sortBy sortDir searchField searchQuery duration metric label
          hdr "id" "#"
          hdr "created_at" (icon "ph-clock" >> " Time")
          case metric of
            Chars -> do
              hdr "input_chars" (icon "ph-download-simple" >> " Input Chars")
              hdr "output_chars" (icon "ph-upload-simple" >> " Output Chars")
            Tokens -> do
              hdr "prompt_tokens" (icon "ph-arrow-line-down" >> " In Tok")
              hdr "completion_tokens" (icon "ph-arrow-line-up" >> " Out Tok")
              hdr "total_tokens" (icon "ph-equals" >> " Total")
          hdr "model" (icon "ph-cpu" >> " Model")
          hdr "alias_name" (icon "ph-tag" >> " Alias")
          hdr "response_status" (icon "ph-check-circle" >> " Status")
          hdr "latency_ms" (icon "ph-timer" >> " Duration")
          hdr "request_body" (icon "ph-paper-plane-right" >> " Request")
          hdr "response_body" (icon "ph-paper-plane-left" >> " Response")
      tbody_ $ mapM_ (requestRow metric) requests
    pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration metric
    when (not (null aliasUsages)) aliasChartScripts
    requestSoundScripts
    script_ clickScript

requestRow :: MetricView -> LlmRequest -> Html ()
requestRow metric r = tr_ [class_ "req-row", data_ "href" ("/ui/request/" <> showT (lrId r))] $ do
  td_ (toHtml (showT (lrId r)))
  td_ [class_ "time"] (toHtml (T.take 19 (showT (lrCreatedAt r))))
  case metric of
    Chars -> do
      td_ [class_ "len"] (toHtml (maybeTextLenCompact (lrRequestBody r)))
      td_ [class_ "len"] (toHtml (maybeTextLenCompact (lrResponseBody r)))
    Tokens -> do
      td_ [class_ "len"] (toHtml (maybeCompact (lrPromptTokens r)))
      td_ [class_ "len"] (toHtml (maybeCompact (lrCompletionTokens r)))
      td_ [class_ "len"] (toHtml (maybeCompact (lrTotalTokens r)))
  td_ [class_ "model"] (renderModel (lrModel r))
  td_ [class_ "alias"] (renderAlias (lrAliasName r))
  td_ [class_ (statusClass (lrResponseStatus r))] (toHtml (maybeDash (lrResponseStatus r)))
  td_ [class_ "latency"] (renderLatency (lrLatencyMs r))
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
      detailCell "ph-cpu" "Model" (renderModel (lrModel r))
      detailCell "ph-tag" "Alias" (renderAlias (lrAliasName r))
      detailCell "ph-arrow-line-down" "Prompt tokens" (toHtml (maybeCompact (lrPromptTokens r)))
      detailCell "ph-arrow-line-up" "Completion tokens" (toHtml (maybeCompact (lrCompletionTokens r)))
      detailCell "ph-equals" "Total tokens" (toHtml (maybeCompact (lrTotalTokens r)))
      detailCell "ph-check-circle" "Status" (toHtml (maybeDash (lrResponseStatus r)))
      detailCell "ph-timer" "Duration" (renderLatency (lrLatencyMs r))
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

pollMsScript :: T.Text -> Int -> Html ()
pollMsScript windowVar seconds =
  script_ [type_ "text/javascript"] $
    "window." <> windowVar <> "=" <> showT (seconds * 1000) <> ";"

aliasChartScripts :: Html ()
aliasChartScripts = do
  pollMsScript "CHART_POLL_MS" chartPollSeconds
  script_ [src_ "https://cdn.jsdelivr.net/npm/chart.js@4"] ("" :: T.Text)
  script_ [src_ "/alias-charts.js"] ("" :: T.Text)

requestSoundScripts :: Html ()
requestSoundScripts = do
  pollMsScript "REQUEST_SOUND_POLL_MS" requestSoundPollSeconds
  script_ [src_ "/request-sounds.js"] ("" :: T.Text)

clickScript :: T.Text
clickScript = T.intercalate "\n"
  [ "document.addEventListener('click', function(e) {"
  , "  var row = e.target.closest('.req-row');"
  , "  if (row && e.target.tagName !== 'A' && e.target.tagName !== 'BUTTON' && !e.target.closest('a') && !e.target.closest('button')) {"
  , "    window.location = row.getAttribute('data-href');"
  , "  }"
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
  , "    var lines = raw.split(/\\r?\\n/);"
  , "    var parsed = [];"
  , "    var isSSE = false;"
  , "    lines.forEach(function(line) {"
  , "      var trimmed = line.trim();"
  , "      if (trimmed.indexOf('data: ') === 0) {"
  , "        isSSE = true;"
  , "        var dataPart = trimmed.slice(6).trim();"
  , "        if (dataPart === '[DONE]') return;"
  , "        var obj = parseJson(dataPart);"
  , "        if (obj) parsed.push(obj);"
  , "      } else if (trimmed === 'data:') {"
  , "        isSSE = true;"
  , "      }"
  , "    });"
  , "    if (isSSE && parsed.length) return renderJson(el, parsed);"
  , "  });"
  , "})();"
  ]

pagination :: Int -> Int -> Int -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> MetricView -> Html ()
pagination pageNum totalPages totalResults sortBy sortDir searchField searchQuery duration metric = div_ [class_ "pagination"] $ do
  let prevUrl = makeUrl (pageNum - 1) sortBy sortDir searchField searchQuery duration metric
      nextUrl = makeUrl (pageNum + 1) sortBy sortDir searchField searchQuery duration metric
  if pageNum > 1
    then a_ [href_ prevUrl] (icon "caret-left" >> " Prev")
    else span_ [class_ "disabled"] (icon "caret-left" >> " Prev")
  span_ [class_ "page-info"] (toHtml ("Page " <> showT pageNum <> " of " <> showT totalPages <> " (" <> showT totalResults <> " total results)"))
  if pageNum < totalPages
    then a_ [href_ nextUrl] ("Next " >> icon "caret-right")
    else span_ [class_ "disabled"] ("Next " >> icon "caret-right")


