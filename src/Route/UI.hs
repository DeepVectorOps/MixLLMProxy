{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.UI (uiRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmRequest(..), LlmStats(..), getRecentRequests, getRequest, countRequests, getStats)
import Common (icon, showT, maybeDash, basePage, baseStyles, queryParamDefault)
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

statBox :: T.Text -> T.Text -> T.Text -> Html ()
statBox iconName label value = div_ [class_ "stat-box"] $ do
  span_ [class_ "stat-icon"] (icon iconName)
  span_ [class_ "stat-label"] (toHtml label)
  span_ [class_ "stat-value"] (toHtml value)

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
    host <- header "Host"
    html $ renderText $ basePage "LLMHouse" styles $ page host requests pageNum totalPages stats
  get "/ui/request/:id" $ do
    rid <- pathParam "id"
    mreq <- liftIO $ withPool env $ \conn -> getRequest conn rid
    case mreq of
      Just req -> html $ renderText $ basePage ("LLMHouse — Request #" <> showT (lrId req)) detailStyles $ detailPage req
      Nothing -> html "not found"

page :: Maybe TL.Text -> [LlmRequest] -> Int -> Int -> LlmStats -> Html ()
page host requests pageNum totalPages stats = do
    div_ [class_ "header-row"] $ do
      h1_ "LLMHouse"
      a_ [href_ "/ui/aliases", class_ "nav-btn"] (icon "gear" >> " Aliases")
      a_ [href_ "/ui/aliases/info", class_ "nav-btn"] (icon "info" >> " Info")
      let base = fromMaybe "localhost" (TL.toStrict <$> host)
          endpoint = T.concat ["http://", base, "/api/openai/v1/chat/completions"]
      code_ [class_ "endpoint"] (icon "ph-link" >> " Endpoint: " >> toHtml endpoint)
    p_ [class_ "subtitle"] "LLM proxy observatory"
    div_ [class_ "stats"] $ do
      statBox "ph-database" "Total Requests" (showT (lsTotalRequests stats))
      statBox "ph-arrow-line-down" "Total Prompt Tokens" (maybeDash (lsTotalPromptTokens stats))
      statBox "ph-arrow-line-up" "Total Completion Tokens" (maybeDash (lsTotalCompletionTokens stats))
      statBox "ph-equals" "Total Tokens" (maybeDash (lsTotalTokens stats))
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

detailRow :: T.Text -> T.Text -> Html () -> Html ()
detailRow iconName label value = tr_ $ do
  td_ [class_ "label"] (icon iconName >> " " >> toHtml label)
  td_ value

detailPage :: LlmRequest -> Html ()
detailPage r = do
    a_ [href_ "/ui/"] (icon "ph-caret-left" >> " Back")
    h1_ (toHtml ("Request #" <> showT (lrId r)))
    table_ [class_ "detail"] $ do
      detailRow "ph-fingerprint" "ID" (toHtml (showT (lrId r)))
      detailRow "ph-clock" "Time" (toHtml (showT (lrCreatedAt r)))
      detailRow "ph-arrow-down-up" "Method" (toHtml (lrMethod r))
      detailRow "ph-link" "Endpoint" (toHtml (lrEndpoint r))
      detailRow "ph-cpu" "Model" (toHtml (fromMaybe "-" (lrModel r)))
      detailRow "ph-tag" "Alias" (toHtml (fromMaybe "-" (lrAliasName r)))
      detailRow "ph-arrow-line-down" "Prompt tokens" (toHtml (maybeDash (lrPromptTokens r)))
      detailRow "ph-arrow-line-up" "Completion tokens" (toHtml (maybeDash (lrCompletionTokens r)))
      detailRow "ph-equals" "Total tokens" (toHtml (maybeDash (lrTotalTokens r)))
      detailRow "ph-check-circle" "Status" (toHtml (maybeDash (lrResponseStatus r)))
      detailRow "ph-timer" "Duration" (toHtml (maybe "-" fmtLatency (lrLatencyMs r)))
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

styles :: T.Text
styles = baseStyles <> T.intercalate "\n"
  [ ".stats { display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px; margin: 16px 0; }"
  , ".stat-box { background: #161b22; border: 1px solid #21262d; border-radius: 8px; padding: 12px 16px; display: flex; flex-direction: column; gap: 4px; }"
  , ".stat-icon { color: #58a6ff; font-size: 18px; }"
  , ".stat-label { color: #8b949e; font-size: 11px; }"
  , ".stat-value { color: #c9d1d9; font-size: 18px; font-weight: 600; }"
  , ".endpoint { background: #1a2130; color: #58a6ff; padding: 4px 10px; border-radius: 6px; font-size: 12px; }"
  , ".nav-btn { background: #161b22; color: #58a6ff; border: 1px solid #30363d; border-radius: 6px; padding: 5px 12px; font-size: 13px; text-decoration: none; display: inline-flex; align-items: center; gap: 5px; }"
  , ".nav-btn:hover { background: #1a2130; border-color: #58a6ff; }"
  , ".req-row { cursor: pointer; }"
  , ".req, .resp { max-width: 300px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }"
  , ".len { white-space: nowrap; color: #8b949e; font-size: 11px; }"
  , ".latency { white-space: nowrap; }"
  , ".model { white-space: nowrap; max-width: 120px; overflow: hidden; text-overflow: ellipsis; }"
  , ".alias { white-space: nowrap; max-width: 120px; overflow: hidden; text-overflow: ellipsis; }"
  , ".status-ok { color: #3fb950; }"
  , ".status-err { color: #d29922; }"
  , ".status-fail { color: #f85149; }"
  , ".pagination { margin-top: 16px; display: flex; align-items: center; gap: 12px; }"
  , ".pagination a { color: #58a6ff; text-decoration: none; }"
  , ".pagination .disabled { color: #484f58; }"
  , ".page-info { color: #8b949e; }"
  ]

detailStyles :: T.Text
detailStyles = styles <> T.intercalate "\n"
  [ "a { color: #58a6ff; text-decoration: none; }"
  , ".detail td { white-space: normal; max-width: none; }"
  , ".detail .label { color: #8b949e; font-weight: 600; width: 120px; white-space: nowrap; }"
  , ".body { background: #161b22; padding: 16px; border-radius: 6px; font-size: 12px; overflow-y: auto; overflow-x: hidden; font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; }"
  , ".body * { overflow-wrap: break-word; word-break: break-all; white-space: normal; text-wrap: wrap; }"
  , ".bodies { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }"
  , ".body-col { min-width: 0; }"
  , "@media (max-width: 768px) { .bodies { grid-template-columns: 1fr; } }"
  , "h2 { color: #8b949e; margin-top: 24px; font-size: 14px; }"
  ]

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
