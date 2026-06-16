{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( icon
  , showT
  , maybeDash
  , faviconSvg
  , baseHead
  , basePage
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
icon name = i_ [class_ ("ph " <> if "ph-" `T.isPrefixOf` name then name else "ph-" <> name)] ""

baseHead :: T.Text -> Html ()
baseHead titleText = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ (toHtml titleText)
  link_ [rel_ "icon", type_ "image/svg+xml", href_ faviconSvg]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/@phosphor-icons/web@2.1.1/src/regular/style.css"]
  link_ [rel_ "stylesheet", href_ "https://unpkg.com/@phosphor-icons/web@2.1.1/src/fill/style.css"]
  link_ [rel_ "stylesheet", href_ "/styles.css"]

basePage :: T.Text -> Html () -> Html ()
basePage titleText bodyContent = doctype_ >> html_ [lang_ "en"] (do
  baseHead titleText
  body_ bodyContent
  )

queryParamDefault :: Parsable a => TL.Text -> a -> ActionM a
queryParamDefault key fallback = queryParam key `catch` (\(_ :: SomeException) -> pure fallback)
