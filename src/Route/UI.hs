{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.UI (uiRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmRequest(..), LlmStats(..), getRecentRequests, getRequest, countRequests, getStats, truncateRequests, getAliasCounts)
import Common (icon, showT, maybeDash, basePage, queryParamDefault)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (zipWithM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP


fmtLatency :: Double -> T.Text
fmtLatency ms
  | ms >= 1000 = T.pack (show (fromIntegral (round (ms / 100) :: Int) / 10)) <> "s"
  | otherwise  = showT (round ms :: Int) <> "ms"

statBox :: T.Text -> T.Text -> T.Text -> Html ()
statBox iconName label value = div_ [class_ "stat-box"] $ do
  div_ [class_ "stat-row"] $ do
    span_ [class_ "stat-icon"] (icon iconName)
    span_ [class_ "stat-label"] (toHtml label)
    span_ [class_ "stat-value"] (toHtml value)

totalBox :: T.Text -> [(T.Text, Int)] -> Html ()
totalBox total aliasCounts = div_ [class_ "stat-box"] $ do
  span_ [class_ "stat-icon"] (icon "ph-database")
  div_ [class_ "stat-body"] $ do
    div_ [class_ "stat-row"] $ do
      span_ [class_ "stat-label"] "Total Requests"
      span_ [class_ "stat-value"] (toHtml total)
    div_ [class_ "alias-breakdown"] $
      zipWithM_ (\(alias, count) color ->
        span_ [class_ "alias-chip", style_ ("color:" <> color <> ";border-color:" <> color)] (toHtml (alias <> ": " <> showT count))
      ) aliasCounts chipColors

chipColors :: [T.Text]
chipColors = ["#58a6ff", "#3fb950", "#d29922", "#f85149", "#a371f7", "#79c0ff", "#56d364", "#e3b341"]

uiRoutes :: AppEnv -> ScottyM ()
uiRoutes env = do
  get "/ui/" $ do
    pageNum <- queryParamDefault "page" 1
    let perPage = 25
    let offset = (pageNum - 1) * perPage
    requests <- liftIO $ withPool env $ \conn -> getRecentRequests conn perPage offset
    total <- liftIO $ withPool env $ \conn -> countRequests conn
    let totalPages = max 1 ((total + perPage - 1) `div` perPage)
    stats <- liftIO $ withPool env $ \conn -> getStats conn
    aliasCounts <- liftIO $ withPool env $ \conn -> getAliasCounts conn
    host <- header "Host"
    html $ renderText $ basePage "MixLLMProxy" $ page host requests pageNum totalPages stats aliasCounts
  get "/ui/request/:id" $ do
    rid <- pathParam "id"
    mreq <- liftIO $ withPool env $ \conn -> getRequest conn rid
    case mreq of
      Just req -> html $ renderText $ basePage ("MixLLMProxy — Request #" <> showT (lrId req)) $ detailPage req
      Nothing -> html "not found"
  post "/ui/truncate" $ do
    liftIO $ withPool env $ \conn -> truncateRequests conn
    redirect "/ui/"

page :: Maybe TL.Text -> [LlmRequest] -> Int -> Int -> LlmStats -> [(T.Text, Int)] -> Html ()
page host requests pageNum totalPages stats aliasCounts = do
    div_ [class_ "header-row"] $ do
      h1_ "🔭 MixLLMProxy"
      a_ [href_ "/ui/aliases", class_ "nav-btn"] (icon "gear" >> " Aliases")
      a_ [href_ "/ui/aliases/info", class_ "nav-btn"] (icon "info" >> " Info")
      let base = fromMaybe "localhost" (TL.toStrict <$> host)
          endpoint = T.concat ["http://", base, "/api/openai/v1/chat/completions"]
      code_ [class_ "endpoint"] (icon "ph-link" >> " Endpoint: " >> toHtml endpoint)
      form_ [action_ "/ui/truncate", method_ "post", class_ "form-inline"] $
        button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Wipe all logged requests?')"] (icon "ph-trash" >> " Truncate")
    div_ [class_ "stats"] $ do
      totalBox (showT (lsTotalRequests stats)) aliasCounts
      statBox "ph-arrow-line-down" "Total Prompt Tokens" (maybeDash (lsTotalPromptTokens stats))
      statBox "ph-arrow-line-up" "Total Completion Tokens" (maybeDash (lsTotalCompletionTokens stats))
      statBox "ph-equals" "Total Tokens" (maybeDash (lsTotalTokens stats))
    pagination pageNum totalPages
    table_ [class_ "requests"] $ do
      thead_ $ do
        tr_ $ do
          th_ "#"
          th_ (icon "ph-clock" >> " Time")
          th_ (icon "ph-download-simple" >> " Input Chars")
          th_ (icon "ph-upload-simple" >> " Output Chars")
          th_ (icon "ph-cpu" >> " Model")
          th_ (icon "ph-tag" >> " Alias")
          th_ (icon "ph-arrow-line-down" >> " In Tok")
          th_ (icon "ph-arrow-line-up" >> " Out Tok")
          th_ (icon "ph-equals" >> " Total")
          th_ (icon "ph-check-circle" >> " Status")
          th_ (icon "ph-timer" >> " Duration")
          th_ (icon "ph-paper-plane-right" >> " Request")
          th_ (icon "ph-paper-plane-left" >> " Response")
      tbody_ $ mapM_ requestRow requests
    pagination pageNum totalPages
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

pagination :: Int -> Int -> Html ()
pagination pageNum totalPages = div_ [class_ "pagination"] $ do
  let prevUrl = "/ui/?page=" <> showT (pageNum - 1)
      nextUrl = "/ui/?page=" <> showT (pageNum + 1)
  if pageNum > 1
    then a_ [href_ prevUrl] (icon "caret-left" >> " Prev")
    else span_ [class_ "disabled"] (icon "caret-left" >> " Prev")
  span_ [class_ "page-info"] (toHtml ("Page " <> showT pageNum <> " of " <> showT totalPages))
  if pageNum < totalPages
    then a_ [href_ nextUrl] ("Next " >> icon "caret-right")
    else span_ [class_ "disabled"] ("Next " >> icon "caret-right")
