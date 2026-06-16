{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.Aliases (aliasesRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmAlias(..), getAliases, getAliasById, insertAlias, updateAlias, deleteAlias)
import Common (icon, showT, basePage)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
  get "/ui/aliases/info" $ do
    host <- fromMaybe "localhost" . fmap TL.toStrict <$> header "Host"
    html $ renderText $ aliasesInfoPage host

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
aliasesPage aliases editId nameFilled urlFilled modelFilled errorMsg = basePage "MixLLMProxy — Aliases" $ do
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

aliasesInfoPage :: T.Text -> Html ()
aliasesInfoPage host = basePage "MixLLMProxy — Aliases Info" $ do
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
      [ "curl -X POST http://" <> host <> "/api/openai/v1/chat/completions \\"
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

aliasRow :: LlmAlias -> Html ()
aliasRow a = tr_ $ do
  td_ [class_ "alias-name"] (code_ (toHtml (laName a)))
  td_ [class_ "alias-url"] (code_ (toHtml (laEndpointUrl a)))
  td_ [class_ "alias-model"] (toHtml (laModel a))
  td_ [class_ "actions"] $ do
    a_ [href_ ("/ui/aliases/" <> showT (laId a) <> "/edit"), class_ "btn-edit"] (icon "pencil" >> " Edit")
    form_ [method_ "post", action_ ("/ui/aliases/" <> showT (laId a) <> "/delete"), class_ "form-inline"] $
      button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Delete this alias?')"] (icon "trash" >> " Delete")
