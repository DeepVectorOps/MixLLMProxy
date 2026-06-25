{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( icon
  , showT
  , showWithCommas
  , maybeDash
  , faviconSvg
  , baseHead
  , basePage
  , queryParamDefault
  , formParamDefault
  , aliasBadge
  , aliasColor
  ) where

import Lucid
import Web.Scotty.Trans (Parsable)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Web.Scotty (ActionM, queryParam, formParam, catch)
import Control.Exception (SomeException)
import Data.Maybe (maybe)
import Data.Text.Format.Numbers (prettyF, PrettyCfg(..))

showT :: Show a => a -> T.Text
showT = T.pack . show

showWithCommas :: Real a => a -> T.Text
showWithCommas = prettyF (PrettyCfg 0 (Just ',') '.') . (realToFrac :: Real a => a -> Double)

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

formParamDefault :: Parsable a => TL.Text -> a -> ActionM a
formParamDefault key fallback = formParam key `catch` (\(_ :: SomeException) -> pure fallback)

hashText :: T.Text -> Int
hashText = T.foldl' (\h c -> h * 33 + fromEnum c) 5381

aliasColor :: T.Text -> T.Text
aliasColor name =
  let colors =
        [ "hsl(210, 85%, 45%)" -- Blue
        , "hsl(120, 70%, 35%)" -- Green
        , "hsl(280, 75%, 45%)" -- Purple
        , "hsl(45,  80%, 40%)" -- Yellow
        , "hsl(320, 80%, 45%)" -- Pink
        , "hsl(180, 80%, 35%)" -- Teal
        , "hsl(90,  75%, 35%)" -- Lime
        , "hsl(240, 70%, 45%)" -- Indigo
        ]
      idx = (hashText name) `mod` length colors
  in colors !! idx

aliasBadge :: T.Text -> Html ()
aliasBadge name =
  let color = aliasColor name
      styleVal = "background: " <> color <> "; color: #ffffff; border-radius: 4px; padding: 3px 8px; font-weight: 600; font-family: ui-monospace, SFMono-Regular, SF Mono, Menlo, Consolas, monospace; font-size: 11px; display: inline-block;"
  in span_ [style_ styleVal] (toHtml name)
