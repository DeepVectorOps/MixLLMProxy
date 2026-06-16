{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.Aliases (aliasesRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmAlias(..), getAliases, getAliasById, insertAlias, updateAlias, deleteAlias)
import Common (icon, showT, basePage, baseStyles)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust)
import Control.Monad.IO.Class (liftIO)

renderAliases :: AppEnv -> Maybe Int -> T.Text -> T.Text -> T.Text -> Maybe T.Text -> ActionM ()
renderAliases env editId name url model errMsg = do
  aliases <- liftIO $ withPool env $ \conn -> getAliases conn
  html $ renderText $ aliasesPage aliases editId name url model errMsg

validateAliasForm :: T.Text -> T.Text -> T.Text -> T.Text -> Bool
validateAliasForm name url key model = T.null name || T.null url || T.null key || T.null model

aliasesRoutes :: AppEnv -> ScottyM ()
aliasesRoutes env = do
  get "/ui/aliases" $ renderAliases env Nothing T.empty T.empty T.empty Nothing
  get "/ui/aliases/info" $ html $ renderText aliasesInfoPage

  post "/ui/aliases/create" $ do
    name <- formParam "name"
    url <- formParam "url"
    key <- formParam "key"
    model <- formParam "model"
    if validateAliasForm name url key model
      then renderAliases env Nothing name url model (Just "All fields are required")
      else do
        liftIO $ withPool env $ \conn -> insertAlias conn name url key model
        redirect "/ui/aliases"

  get "/ui/aliases/:id/edit" $ do
    (aid :: Int) <- pathParam "id"
    malias <- liftIO $ withPool env $ \conn -> getAliasById conn aid
    case malias of
      Just a -> renderAliases env (Just (laId a)) (laName a) (laEndpointUrl a) (laModel a) Nothing
      Nothing -> redirect "/ui/aliases"

  post "/ui/aliases/:id/update" $ do
    (aid :: Int) <- pathParam "id"
    name <- formParam "name"
    url <- formParam "url"
    key <- formParam "key"
    model <- formParam "model"
    if validateAliasForm name url key model
      then renderAliases env (Just aid) name url model (Just "All fields are required")
      else do
        liftIO $ withPool env $ \conn -> updateAlias conn aid name url key model
        redirect "/ui/aliases"

  post "/ui/aliases/:id/delete" $ do
    (aid :: Int) <- pathParam "id"
    liftIO $ withPool env $ \conn -> deleteAlias conn aid
    redirect "/ui/aliases"

aliasesPage :: [LlmAlias] -> Maybe Int -> T.Text -> T.Text -> T.Text -> Maybe T.Text -> Html ()
aliasesPage aliases editId nameFilled urlFilled modelFilled errorMsg = basePage "LLMHouse — Aliases" aliasesStyles $ do
  div_ [class_ "header-row"] $ do
    h1_ "Aliases"
  p_ [class_ "subtitle"] "Manage LLM endpoint aliases"

  div_ [class_ "form-section"] $ do
    h2_ $ toHtml (if isJust editId then ("Edit Alias" :: T.Text) else "New Alias")
    maybe "" (\e -> div_ [class_ "error"] (toHtml e)) errorMsg
    form_ [method_ "post", action_ (if isJust editId then "/ui/aliases/" <> showT (fromMaybe 0 editId) <> "/update" else "/ui/aliases/create")] $ do
      div_ [class_ "form-grid"] $ do
        label_ [for_ "name"] "Name"
        input_ [type_ "text", name_ "name", id_ "name", value_ nameFilled, placeholder_ "e.g. openai-prod", required_ ""]

        label_ [for_ "url"] "Endpoint URL"
        input_ [type_ "text", name_ "url", id_ "url", value_ urlFilled, placeholder_ "https://api.openai.com/v1/chat/completions", required_ ""]

        label_ [for_ "key"] "API Key"
        input_ [type_ "password", name_ "key", id_ "key", placeholder_ "sk-...", required_ (if isJust editId then "" else ""), autocomplete_ "off"]

        label_ [for_ "model"] "Model"
        input_ [type_ "text", name_ "model", id_ "model", value_ modelFilled, placeholder_ "gpt-4o", required_ ""]

      div_ [class_ "form-actions"] $ do
        button_ [type_ "submit", class_ "btn-save"] (icon "floppy-disk" >> " " >> toHtml (if isJust editId then ("Update" :: T.Text) else "Create"))
        if isJust editId
          then a_ [href_ "/ui/aliases", class_ "btn-cancel"] "Cancel"
          else ""

  h2_ "All Aliases"
  if null aliases
    then p_ [class_ "empty"] "No aliases configured. Create one above."
    else table_ [class_ "aliases-table"] $ do
      thead_ $ tr_ $ do
        th_ "Name"
        th_ "Endpoint URL"
        th_ "Model"
        th_ "Actions"
      tbody_ $ mapM_ aliasRow aliases

aliasesStyles :: T.Text
aliasesStyles = baseStyles <> T.intercalate "\n"
  [ ".form-section { background: #161b22; border: 1px solid #21262d; border-radius: 8px; padding: 16px; margin: 16px 0; }"
  , ".form-grid { display: grid; grid-template-columns: 120px 1fr; gap: 8px 12px; align-items: center; }"
  , ".form-grid label { color: #8b949e; font-size: 13px; }"
  , ".form-grid input { background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 6px 10px; color: #c9d1d9; font-size: 13px; }"
  , ".form-grid input:focus { border-color: #58a6ff; outline: none; }"
  , ".form-actions { grid-column: 1 / -1; margin-top: 8px; display: flex; gap: 8px; }"
  , ".btn-save { background: #238636; color: white; border: none; border-radius: 6px; padding: 6px 14px; font-size: 13px; cursor: pointer; }"
  , ".btn-save:hover { background: #2ea043; }"
  , ".btn-cancel { background: #21262d; color: #c9d1d9; border: 1px solid #30363d; border-radius: 6px; padding: 6px 14px; font-size: 13px; }"
  , ".btn-danger { background: #da3633; color: white; border: none; border-radius: 6px; padding: 4px 10px; font-size: 12px; cursor: pointer; }"
  , ".btn-danger:hover { background: #f85149; }"
  , ".btn-edit { background: #21262d; color: #58a6ff; border: 1px solid #30363d; border-radius: 6px; padding: 4px 10px; font-size: 12px; cursor: pointer; text-decoration: none; display: inline-block; }"
  , ".error { background: #490202; color: #f85149; border: 1px solid #da3633; border-radius: 6px; padding: 8px 12px; margin-bottom: 12px; font-size: 13px; }"
  , ".empty { color: #484f58; }"
  , ".actions { display: flex; gap: 8px; }"
  , ".form-inline { display: inline; }"
  , ".nav-btn { background: #161b22; color: #58a6ff; border: 1px solid #30363d; border-radius: 6px; padding: 5px 12px; font-size: 13px; text-decoration: none; display: inline-flex; align-items: center; gap: 5px; }"
  , ".nav-btn:hover { background: #1a2130; border-color: #58a6ff; }"
  ]

aliasesInfoPage :: Html ()
aliasesInfoPage = basePage "LLMHouse — Aliases Info" infoStyles $ do
  div_ [class_ "header-row"] $ do
    h1_ (icon "info" >> " Alias Info")
  p_ [class_ "subtitle"] "How aliases work"

  div_ [class_ "info-section"] $ do
    p_ $ do
      "Aliases let you map model names to different LLM backends. Create an alias with a name, endpoint URL, API key, and model — then use that name as the model in your requests."
    h2_ "Example"
    p_ $ do
      "Create an alias named "
      strong_ "test-bob"
      " pointing to "
      strong_ "opencode.ai"
      " with model "
      strong_ "deepseek-v4-flash"
      ". Then:"
    pre_ [class_ "code-block"] $ code_ (toHtml $ T.intercalate "\n"
      [ "curl -X POST http://llmhouse.chris/api/openai/v1/chat/completions \\"
      , "  -H \"Content-Type: application/json\" \\"
      , "  -d '{\"model\":\"test-bob\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}'"
      ])
    p_ $ do
      "Proxies to "
      strong_ "https://opencode.ai/zen/go/v1/chat/completions"
      " with model "
      strong_ "deepseek-v4-flash"
      "."
    p_ "No matching alias gives a 400 error."

infoStyles :: T.Text
infoStyles = baseStyles <> T.intercalate "\n"
  [ ".info-section { background: #161b22; border: 1px solid #21262d; border-radius: 8px; padding: 16px 24px; margin: 16px 0; }"
  , ".info-section h2 { color: #58a6ff; font-size: 15px; margin-top: 20px; margin-bottom: 6px; }"
  , ".info-section h2:first-child { margin-top: 0; }"
  , ".info-section p, .info-section li { color: #8b949e; font-size: 13px; line-height: 1.6; }"
  , ".info-section ul, .info-section ol { margin: 6px 0; padding-left: 20px; }"
  , ".info-section li { margin-bottom: 4px; }"
  , ".mono { font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; font-size: 12px; color: #c9d1d9; }"
  , ".code-block { background: #0d1117; border: 1px solid #30363d; border-radius: 6px; padding: 12px 16px; overflow-x: auto; margin: 10px 0; }"
  , ".code-block code { font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; font-size: 12px; color: #7ee787; line-height: 1.5; }"
  , "strong { color: #e6edf3; font-weight: 600; }"
  , ".nav-btn { background: #161b22; color: #58a6ff; border: 1px solid #30363d; border-radius: 6px; padding: 5px 12px; font-size: 13px; text-decoration: none; display: inline-flex; align-items: center; gap: 5px; }"
  , ".nav-btn:hover { background: #1a2130; border-color: #58a6ff; }"
  ]

aliasRow :: LlmAlias -> Html ()
aliasRow a = tr_ $ do
  td_ [class_ "alias-name"] (code_ (toHtml (laName a)))
  td_ [class_ "alias-url"] (code_ (toHtml (laEndpointUrl a)))
  td_ [class_ "alias-model"] (toHtml (laModel a))
  td_ [class_ "actions"] $ do
    a_ [href_ ("/ui/aliases/" <> showT (laId a) <> "/edit"), class_ "btn-edit"] (icon "pencil" >> " Edit")
    form_ [method_ "post", action_ ("/ui/aliases/" <> showT (laId a) <> "/delete"), class_ "form-inline"] $
      button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Delete this alias?')"] (icon "trash" >> " Delete")
