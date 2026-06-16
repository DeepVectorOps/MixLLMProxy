{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( icon
  , showT
  , maybeDash
  , faviconSvg
  , baseHead
  , basePage
  , baseStyles
  , queryParamDefault
  ) where

import Lucid
import Web.Scotty.Trans (Parsable)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Web.Scotty (ActionM, queryParam, catch)
import Control.Exception (SomeException)
import Data.Maybe (maybe)

showT :: Show a => a -> T.Text
showT = T.pack . show

maybeDash :: Show a => Maybe a -> T.Text
maybeDash = maybe "-" showT

faviconSvg :: T.Text
faviconSvg = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ctext y='.9em' font-size='90'%3E🔭%3C/text%3E%3C/svg%3E"

icon :: T.Text -> Html ()
icon name = i_ [class_ ("ph ph-" <> name)] ""

baseStyles :: T.Text
baseStyles = T.intercalate "\n"
  [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 20px; background: #0d1117; color: #c9d1d9; }"
  , ".header-row { display: flex; align-items: center; gap: 16px; }"
  , "h1 { color: #58a6ff; margin: 0; }"
  , ".subtitle { color: #8b949e; margin-top: 4px; }"
  , "table { width: 100%; border-collapse: collapse; margin-top: 16px; font-size: 13px; }"
  , "th { text-align: left; padding: 8px 12px; border-bottom: 1px solid #21262d; color: #8b949e; font-weight: 600; white-space: nowrap; }"
  , "td { padding: 8px 12px; border-bottom: 1px solid #21262d; max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }"
  ]

baseHead :: T.Text -> T.Text -> Html ()
baseHead titleText css = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ (toHtml titleText)
  link_ [rel_ "icon", type_ "image/svg+xml", href_ faviconSvg]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/@phosphor-icons/web@2.1.1/src/regular/style.css"]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/@phosphor-icons/web@2.1.1/src/fill/style.css"]
  style_ [type_ "text/css"] css

basePage :: T.Text -> T.Text -> Html () -> Html ()
basePage titleText css bodyContent = doctype_ >> html_ [lang_ "en"] (do
  baseHead titleText css
  body_ bodyContent
  )

queryParamDefault :: Parsable a => TL.Text -> a -> ActionM a
queryParamDefault key fallback = queryParam key `catch` (\(_ :: SomeException) -> pure fallback)
