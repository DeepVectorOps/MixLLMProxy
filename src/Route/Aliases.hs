{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.Aliases (aliasesRoutes) where

import Web.Scotty
import Lucid
import AppEnv (AppEnv, withPool)
import DB (LlmAlias(..), getAliases, getAliasById, insertAlias, updateAlias, deleteAlias)
import Common
  ( icon, showT, basePage, aliasBadge, pageHeader, hostFromHeader
  , optionalIntFormParam, limitValueAttr
  , aliasEditUrl, aliasUpdateUrl, aliasDuplicateUrl, aliasDeleteUrl
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe (fromMaybe, isJust, maybe, catMaybes, listToMaybe)
import Control.Monad.IO.Class (liftIO)

data AliasForm = AliasForm
  { afName :: T.Text
  , afUrl :: T.Text
  , afKey :: T.Text
  , afModel :: T.Text
  , afTokenLimit :: Maybe Int
  , afReqLimit :: Maybe Int
  }

parseAliasForm :: ActionM AliasForm
parseAliasForm = AliasForm
  <$> formParam "name"
  <*> formParam "url"
  <*> formParam "key"
  <*> formParam "model"
  <*> optionalIntFormParam "daily_token_limit"
  <*> optionalIntFormParam "daily_request_limit"

duplicateName :: T.Text -> [T.Text] -> T.Text
duplicateName name existing = fromMaybe (name <> "-1") $ listToMaybe $ dropWhile (`elem` existing)
  [ name <> "-" <> T.pack (show n) | n <- [1..] ]

renderAliases :: AppEnv -> Maybe Int -> AliasForm -> Maybe T.Text -> ActionM ()
renderAliases env editId form errMsg = do
  aliases <- liftIO $ withPool env $ \conn -> getAliases conn
  host <- hostFromHeader <$> header "Host"
  html $ renderText $ aliasesPage host aliases editId form errMsg

handleAliasSubmit :: AppEnv -> Maybe Int -> AliasForm -> ActionM ()
handleAliasSubmit env mEditId form
  | T.null (afName form) || T.null (afUrl form) || T.null (afKey form) || T.null (afModel form) =
      renderAliases env mEditId form (Just "All fields are required")
  | otherwise = do
      liftIO $ withPool env $ \conn -> case mEditId of
        Nothing -> insertAlias conn (afName form) (afUrl form) (afKey form) (afModel form) (afTokenLimit form) (afReqLimit form)
        Just aid -> updateAlias conn aid (afName form) (afUrl form) (afKey form) (afModel form) (afTokenLimit form) (afReqLimit form)
      redirect "/ui/aliases"

aliasesRoutes :: AppEnv -> ScottyM ()
aliasesRoutes env = do
  get "/ui/aliases" $
    renderAliases env Nothing (AliasForm T.empty T.empty T.empty T.empty Nothing Nothing) Nothing

  post "/ui/aliases/create" $ handleAliasSubmit env Nothing =<< parseAliasForm

  get "/ui/aliases/:id/edit" $ do
    (aid :: Int) <- pathParam "id"
    malias <- liftIO $ withPool env $ \conn -> getAliasById conn aid
    case malias of
      Just a -> renderAliases env (Just (laId a)) (aliasToForm a) Nothing
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
          insertAlias conn newName (laEndpointUrl a) (laApiKey a) (laModel a) (laDailyTokenLimit a) (laDailyRequestLimit a)
        redirect "/ui/aliases"
      Nothing -> redirect "/ui/aliases"

  post "/ui/aliases/:id/delete" $ do
    (aid :: Int) <- pathParam "id"
    liftIO $ withPool env $ \conn -> deleteAlias conn aid
    redirect "/ui/aliases"

aliasToForm :: LlmAlias -> AliasForm
aliasToForm a = AliasForm
  (laName a) (laEndpointUrl a) (laApiKey a) (laModel a) (laDailyTokenLimit a) (laDailyRequestLimit a)

aliasesPage :: T.Text -> [LlmAlias] -> Maybe Int -> AliasForm -> Maybe T.Text -> Html ()
aliasesPage host aliases editId form errorMsg = basePage "MixLLMProxy — Aliases" $ do
  div_ [class_ "container"] $ do
    pageHeader host Nothing
    p_ [class_ "subtitle"] "Manage LLM endpoint aliases"

    div_ [class_ "form-section"] $ do
      h2_ $ toHtml (if isJust editId then ("Edit Alias" :: T.Text) else "New Alias")
      maybe "" (\e -> div_ [class_ "error"] (toHtml e)) errorMsg
      form_ [method_ "post", action_ (if isJust editId then aliasUpdateUrl (fromMaybe 0 editId) else "/ui/aliases/create")] $ do
        div_ [class_ "form-grid"] $ do
          label_ [for_ "name"] "Name"
          input_ [type_ "text", name_ "name", id_ "name", value_ (afName form), placeholder_ "e.g. openai-prod", required_ ""]

          label_ [for_ "url"] "Endpoint URL"
          input_ [type_ "text", name_ "url", id_ "url", value_ (afUrl form), placeholder_ "https://api.openai.com/v1/chat/completions", required_ ""]

          label_ [for_ "key"] "API Key"
          div_ [class_ "key-wrapper"] $ do
            input_ [type_ "password", name_ "key", id_ "key", value_ (afKey form), placeholder_ "sk-...", autocomplete_ "off"]
            button_ [type_ "button", class_ "btn-toggle-key", id_ "toggleKeyBtn", onclick_ "toggleKey()"] (icon "eye" >> "")

          label_ [for_ "model"] "Model"
          input_ [type_ "text", name_ "model", id_ "model", value_ (afModel form), placeholder_ "gpt-4o", required_ ""]

          label_ [for_ "daily_token_limit"] "Daily Token Limit"
          input_ [type_ "number", name_ "daily_token_limit", id_ "daily_token_limit", value_ (limitValueAttr (afTokenLimit form)), placeholder_ "blank = no limit", min_ "0"]

          label_ [for_ "daily_request_limit"] "Daily Request Limit"
          input_ [type_ "number", name_ "daily_request_limit", id_ "daily_request_limit", value_ (limitValueAttr (afReqLimit form)), placeholder_ "blank = no limit", min_ "0"]

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
          th_ "Limits (24h)"
          th_ "Actions"
        tbody_ $ mapM_ aliasRow aliases

    script_ [type_ "text/javascript"] $ T.unlines
      [ "function toggleKey(){var i=document.getElementById('key'),b=document.getElementById('toggleKeyBtn');if(i.type==='password'){i.type='text';b.innerHTML='<i class=\"ph ph-eye-slash\"></i>'}else{i.type='password';b.innerHTML='<i class=\"ph ph-eye\"></i>'}}"
      ]

aliasRow :: LlmAlias -> Html ()
aliasRow a = tr_ $ do
  td_ [class_ "alias-name"] (aliasBadge (laName a))
  td_ [class_ "alias-url"] (code_ (toHtml (laEndpointUrl a)))
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