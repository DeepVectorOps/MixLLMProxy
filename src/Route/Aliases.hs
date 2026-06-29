{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.Aliases (aliasesRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB
  ( LlmEndpoint(..), LlmAlias(..)
  , getEndpoints, getEndpointById, insertEndpoint, updateEndpoint, deleteEndpoint, countAliasesForEndpoint
  , getAliases, getAliasById, insertAlias, updateAlias, deleteAlias
  )
import Common
  ( icon, showT, basePage, aliasBadge, pageHeader, pageToolbar, hostFromHeader
  , optionalIntFormParam, limitValueAttr
  , aliasEditUrl, aliasUpdateUrl, aliasDuplicateUrl, aliasDeleteUrl
  , endpointEditUrl, endpointUpdateUrl, endpointDeleteUrl
  )
import qualified Data.Text as T
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes, listToMaybe)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)

data EndpointForm = EndpointForm
  { efName :: T.Text
  , efUrl :: T.Text
  , efKey :: T.Text
  }

data AliasForm = AliasForm
  { afName :: T.Text
  , afEndpointId :: Maybe Int
  , afModel :: T.Text
  , afTokenLimit :: Maybe Int
  , afReqLimit :: Maybe Int
  }

data PageState = PageState
  { psEditEndpointId :: Maybe Int
  , psEditAliasId :: Maybe Int
  , psEndpointForm :: EndpointForm
  , psAliasForm :: AliasForm
  , psEndpointError :: Maybe T.Text
  , psAliasError :: Maybe T.Text
  }

emptyPageState :: PageState
emptyPageState = PageState Nothing Nothing
  (EndpointForm T.empty T.empty T.empty)
  (AliasForm T.empty Nothing T.empty Nothing Nothing)
  Nothing Nothing

parseEndpointForm :: ActionM EndpointForm
parseEndpointForm = EndpointForm
  <$> formParam "endpoint_name"
  <*> formParam "endpoint_url"
  <*> formParam "endpoint_key"

parseAliasForm :: ActionM AliasForm
parseAliasForm = AliasForm
  <$> formParam "name"
  <*> optionalIntFormParam "endpoint_id"
  <*> formParam "model"
  <*> optionalIntFormParam "daily_token_limit"
  <*> optionalIntFormParam "daily_request_limit"

duplicateName :: T.Text -> [T.Text] -> T.Text
duplicateName name existing = fromMaybe (name <> "-1") $ listToMaybe $ dropWhile (`elem` existing)
  [ name <> "-" <> T.pack (show n) | n <- [1..] ]

renderPage :: AppEnv -> PageState -> ActionM ()
renderPage env state = do
  (endpoints, aliases) <- liftIO $ withPool env $ \conn ->
    (,) <$> getEndpoints conn <*> getAliases conn
  host <- hostFromHeader <$> header "Host"
  html $ renderText $ aliasesPage host endpoints aliases state

handleEndpointSubmit :: AppEnv -> Maybe Int -> EndpointForm -> ActionM ()
handleEndpointSubmit env mEditId form
  | T.null (efName form) || T.null (efUrl form) || T.null (efKey form) =
      renderPage env $ emptyPageState
        { psEditEndpointId = mEditId
        , psEndpointForm = form
        , psEndpointError = Just "All endpoint fields are required"
        }
  | otherwise = do
      liftIO $ withPool env $ \conn -> case mEditId of
        Nothing -> void $ insertEndpoint conn (efName form) (efUrl form) (efKey form)
        Just eid -> updateEndpoint conn eid (efName form) (efUrl form) (efKey form)
      redirect "/ui/aliases"

handleAliasSubmit :: AppEnv -> Maybe Int -> AliasForm -> ActionM ()
handleAliasSubmit env mEditId form
  | T.null (afName form) || isNothing (afEndpointId form) || T.null (afModel form) =
      renderPage env $ emptyPageState
        { psEditAliasId = mEditId
        , psAliasForm = form
        , psAliasError = Just "Name, endpoint, and model are required"
        }
  | otherwise = do
      let endpointId = fromMaybe 0 (afEndpointId form)
      liftIO $ withPool env $ \conn -> case mEditId of
        Nothing -> insertAlias conn (afName form) endpointId (afModel form) (afTokenLimit form) (afReqLimit form)
        Just aid -> updateAlias conn aid (afName form) endpointId (afModel form) (afTokenLimit form) (afReqLimit form)
      redirect "/ui/aliases"

aliasesRoutes :: AppEnv -> ScottyM ()
aliasesRoutes env = do
  get "/ui/aliases" $ renderPage env emptyPageState

  post "/ui/aliases/endpoints/create" $ handleEndpointSubmit env Nothing =<< parseEndpointForm

  get "/ui/aliases/endpoints/:id/edit" $ do
    (eid :: Int) <- pathParam "id"
    mendpoint <- liftIO $ withPool env $ \conn -> getEndpointById conn eid
    case mendpoint of
      Just e -> renderPage env emptyPageState
        { psEditEndpointId = Just eid
        , psEndpointForm = endpointToForm e
        }
      Nothing -> redirect "/ui/aliases"

  post "/ui/aliases/endpoints/:id/update" $ do
    (eid :: Int) <- pathParam "id"
    handleEndpointSubmit env (Just eid) =<< parseEndpointForm

  post "/ui/aliases/endpoints/:id/delete" $ do
    (eid :: Int) <- pathParam "id"
    aliasCount <- liftIO $ withPool env $ \conn -> countAliasesForEndpoint conn eid
    if aliasCount > 0
      then renderPage env emptyPageState
          { psEndpointError = Just $ "Cannot delete: " <> showT aliasCount <> " alias(es) still use this endpoint"
          }
      else do
        liftIO $ withPool env $ \conn -> deleteEndpoint conn eid
        redirect "/ui/aliases"

  post "/ui/aliases/create" $ handleAliasSubmit env Nothing =<< parseAliasForm

  get "/ui/aliases/:id/edit" $ do
    (aid :: Int) <- pathParam "id"
    malias <- liftIO $ withPool env $ \conn -> getAliasById conn aid
    case malias of
      Just a -> renderPage env emptyPageState
        { psEditAliasId = Just aid
        , psAliasForm = aliasToForm a
        }
      Nothing -> redirect "/ui/aliases"

  post "/ui/aliases/:id/update" $ do
    (aid :: Int) <- pathParam "id"
    handleAliasSubmit env (Just aid) =<< parseAliasForm

  post "/ui/aliases/:id/duplicate" $ do
    (aid :: Int) <- pathParam "id"
    malias <- liftIO $ withPool env $ \conn -> getAliasById conn aid
    case malias of
      Just a -> do
        existingNames <- liftIO $ withPool env $ \conn -> map laName <$> getAliases conn
        let newName = duplicateName (laName a) existingNames
        liftIO $ withPool env $ \conn ->
          insertAlias conn newName (laEndpointId a) (laModel a) (laDailyTokenLimit a) (laDailyRequestLimit a)
        redirect "/ui/aliases"
      Nothing -> redirect "/ui/aliases"

  post "/ui/aliases/:id/delete" $ do
    (aid :: Int) <- pathParam "id"
    liftIO $ withPool env $ \conn -> deleteAlias conn aid
    redirect "/ui/aliases"

errorBanner :: Maybe T.Text -> Html ()
errorBanner = maybe mempty (\e -> div_ [class_ "error"] (toHtml e))

formActions :: Bool -> T.Text -> Html ()
formActions editing saveLabel = div_ [class_ "form-actions"] $ do
  button_ [type_ "submit", class_ "btn-save"] (icon "floppy-disk" >> " " >> toHtml saveLabel)
  when editing $ a_ [href_ "/ui/aliases", class_ "btn-cancel"] "Cancel"

endpointKeyField :: T.Text -> Html ()
endpointKeyField keyValue =
  div_ [class_ "key-wrapper"] $ do
    input_ [type_ "password", name_ "endpoint_key", id_ "endpoint_key", value_ keyValue, placeholder_ "sk-...", autocomplete_ "off"]
    button_ [type_ "button", class_ "btn-toggle-key", id_ "toggleEndpointKeyBtn", onclick_ "toggleKey('endpoint_key','toggleEndpointKeyBtn')"] (icon "eye" >> "")

endpointToForm :: LlmEndpoint -> EndpointForm
endpointToForm e = EndpointForm (leName e) (leUrl e) (leApiKey e)

aliasToForm :: LlmAlias -> AliasForm
aliasToForm a = AliasForm
  (laName a) (Just (laEndpointId a)) (laModel a) (laDailyTokenLimit a) (laDailyRequestLimit a)

aliasesPage :: T.Text -> [LlmEndpoint] -> [LlmAlias] -> PageState -> Html ()
aliasesPage host endpoints aliases state = basePage "MixLLMProxy — Aliases" $ do
  pageHeader
  div_ [class_ "page-content"] $ do
    pageToolbar host Nothing
    p_ [class_ "subtitle"] "Configure downstream endpoints and route aliases to them"

    endpointsSection endpoints state
    aliasesSection endpoints aliases state

    script_ [type_ "text/javascript"] $ T.unlines
      [ "function toggleKey(id,btnId){var i=document.getElementById(id),b=document.getElementById(btnId);if(i.type==='password'){i.type='text';b.innerHTML='<i class=\"ph ph-eye-slash\"></i>'}else{i.type='password';b.innerHTML='<i class=\"ph ph-eye\"></i>'}}"
      ]

endpointsSection :: [LlmEndpoint] -> PageState -> Html ()
endpointsSection endpoints state = do
  let editId = psEditEndpointId state
      form = psEndpointForm state
  div_ [class_ "form-section"] $ do
    h2_ [class_ "section-title"] (icon "plugs-connected" >> " Endpoints")
    p_ [class_ "section-desc"] "Shared downstream URLs and API keys. Aliases reference an endpoint by name."
    errorBanner (psEndpointError state)
    form_ [method_ "post", action_ (if isJust editId then endpointUpdateUrl (fromMaybe 0 editId) else "/ui/aliases/endpoints/create")] $ do
      div_ [class_ "form-grid"] $ do
        label_ [for_ "endpoint_name"] "Name"
        input_ [type_ "text", name_ "endpoint_name", id_ "endpoint_name", value_ (efName form), placeholder_ "e.g. openai-prod", required_ ""]

        label_ [for_ "endpoint_url"] "URL"
        input_ [type_ "text", name_ "endpoint_url", id_ "endpoint_url", value_ (efUrl form), placeholder_ "https://api.openai.com/v1/chat/completions", required_ ""]

        label_ [for_ "endpoint_key"] "API Key"
        endpointKeyField (efKey form)

      formActions (isJust editId) (if isJust editId then "Update Endpoint" else "Add Endpoint")

    if null endpoints
      then p_ [class_ "empty"] "No endpoints yet. Add one above."
      else table_ [class_ "config-table"] $ do
        thead_ $ tr_ $ do
          th_ "Name"
          th_ "URL"
          th_ "Actions"
        tbody_ $ mapM_ endpointRow endpoints

endpointRow :: LlmEndpoint -> Html ()
endpointRow e = tr_ $ do
  td_ [class_ "endpoint-name"] (strong_ (toHtml (leName e)))
  td_ [class_ "endpoint-url"] (code_ (toHtml (leUrl e)))
  td_ [class_ "actions"] $ do
    a_ [href_ (endpointEditUrl (leId e)), class_ "btn-edit"] (icon "pencil" >> " Edit")
    form_ [method_ "post", action_ (endpointDeleteUrl (leId e)), class_ "form-inline"] $
      button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Delete this endpoint?')"] (icon "trash" >> " Delete")

aliasesSection :: [LlmEndpoint] -> [LlmAlias] -> PageState -> Html ()
aliasesSection endpoints aliases state = do
  let editId = psEditAliasId state
      form = psAliasForm state
  div_ [class_ "card aliases-section"] $ do
    h2_ [class_ "section-title"] (icon "tag" >> " Aliases")
    p_ [class_ "section-desc"] "Client-facing names mapped to an endpoint and model. Rate limits apply per alias."
    errorBanner (psAliasError state)

    if null endpoints
      then p_ [class_ "empty"] "Create at least one endpoint before adding aliases."
      else form_ [method_ "post", action_ (if isJust editId then aliasUpdateUrl (fromMaybe 0 editId) else "/ui/aliases/create")] $ do
        div_ [class_ "form-grid"] $ do
          label_ [for_ "name"] "Name"
          input_ [type_ "text", name_ "name", id_ "name", value_ (afName form), placeholder_ "e.g. gpt4-fast", required_ ""]

          label_ [for_ "endpoint_id"] "Endpoint"
          select_ [name_ "endpoint_id", id_ "endpoint_id", class_ "input-select", required_ ""] $ do
            let placeholderAttrs = [value_ "", disabled_ ""] ++ [selected_ "selected" | isNothing (afEndpointId form)]
            option_ placeholderAttrs "Select endpoint…"
            forM_ endpoints $ \e ->
              let attrs = [value_ (showT (leId e))] ++ [selected_ "selected" | afEndpointId form == Just (leId e)]
              in option_ attrs (toHtml (leName e))

          label_ [for_ "model"] "Model"
          input_ [type_ "text", name_ "model", id_ "model", value_ (afModel form), placeholder_ "gpt-4o", required_ ""]

          label_ [for_ "daily_token_limit"] "Daily Token Limit"
          input_ [type_ "number", name_ "daily_token_limit", id_ "daily_token_limit", value_ (limitValueAttr (afTokenLimit form)), placeholder_ "blank = no limit", min_ "0"]

          label_ [for_ "daily_request_limit"] "Daily Request Limit"
          input_ [type_ "number", name_ "daily_request_limit", id_ "daily_request_limit", value_ (limitValueAttr (afReqLimit form)), placeholder_ "blank = no limit", min_ "0"]

        formActions (isJust editId) (if isJust editId then "Update Alias" else "Create Alias")

    h3_ [class_ "subsection-title"] "All Aliases"
    if null aliases
      then p_ [class_ "empty"] "No aliases configured yet."
      else table_ [class_ "config-table aliases-table"] $ do
        thead_ $ tr_ $ do
          th_ "Name"
          th_ "Endpoint"
          th_ "Model"
          th_ "Limits (24h)"
          th_ "Actions"
        tbody_ $ mapM_ aliasRow aliases

aliasRow :: LlmAlias -> Html ()
aliasRow a = tr_ $ do
  td_ [class_ "alias-name"] (aliasBadge (laName a))
  td_ [class_ "alias-endpoint"] (toHtml (laEndpointName a))
  td_ [class_ "alias-model"] (toHtml (laModel a))
  td_ [class_ "alias-limits"] (toHtml (formatLimits a))
  td_ [class_ "actions"] $ do
    a_ [href_ (aliasEditUrl (laId a)), class_ "btn-edit"] (icon "pencil" >> " Edit")
    form_ [method_ "post", action_ (aliasDuplicateUrl (laId a)), class_ "form-inline"] $
      button_ [type_ "submit", class_ "btn-duplicate"] (icon "copy" >> " Duplicate")
    form_ [method_ "post", action_ (aliasDeleteUrl (laId a)), class_ "form-inline"] $
      button_ [type_ "submit", class_ "btn-danger", onclick_ "return confirm('Delete this alias?')"] (icon "trash" >> " Delete")

formatLimits :: LlmAlias -> T.Text
formatLimits a =
  let parts = catMaybes
                [ (\n -> showT n <> " req/day") <$> laDailyRequestLimit a
                , (\n -> showT n <> " tok/day") <$> laDailyTokenLimit a
                ]
  in if null parts then "—" else T.intercalate ", " parts