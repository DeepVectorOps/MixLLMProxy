{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( icon
  , showT
  , showWithCommas
  , showCompact
  , maybeCompact
  , maybeTextLenCompact
  , maybeDash
  , faviconSvg
  , baseHead
  , basePage
  , queryParamDefault
  , formParamDefault
  , optionalIntFormParam
  , limitValueAttr
  , hostFromHeader
  , proxyEndpoint
  , pageHeader
  , aliasEditUrl
  , aliasUpdateUrl
  , aliasDuplicateUrl
  , aliasDeleteUrl
  , endpointEditUrl
  , endpointUpdateUrl
  , endpointDeleteUrl
  , aliasBadge
  , aliasBadgeWithEdit
  , aliasColor
  , endpointBox
  ) where

import Lucid
import Web.Scotty.Trans (Parsable)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Web.Scotty (ActionM, queryParam, formParam, catch)
import Control.Exception (SomeException)
import Data.Maybe (maybe, fromMaybe)
import Data.String.Conversions (cs)
import Text.Read (readMaybe)
import Data.Text.Format.Numbers (prettyF, PrettyCfg(..))

showT :: Show a => a -> T.Text
showT = T.pack . show

showWithCommas :: Real a => a -> T.Text
showWithCommas = prettyF (PrettyCfg 0 (Just ',') '.') . (realToFrac :: Real a => a -> Double)

showCompact :: (Integral a, Show a) => a -> T.Text
showCompact n
  | n >= 1000000 = fmtScaled n 1000000 "m"
  | n >= 1000    = fmtScaled n 1000 "k"
  | otherwise    = showT n
  where
    fmtScaled :: Integral a => a -> a -> T.Text -> T.Text
    fmtScaled val scale suffix =
      let tenths = fromIntegral ((val * 10 + scale `div` 2) `div` scale) :: Int
          whole = tenths `div` 10
          frac = tenths `mod` 10
      in if frac == 0 then showT whole <> suffix else showT whole <> "." <> showT frac <> suffix

maybeCompact :: Maybe Int -> T.Text
maybeCompact = maybe "-" showCompact

maybeTextLenCompact :: Maybe T.Text -> T.Text
maybeTextLenCompact = maybe "-" (showCompact . T.length)

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

optionalIntFormParam :: TL.Text -> ActionM (Maybe Int)
optionalIntFormParam key = do
  mTxt <- (Just <$> formParam key) `catch` (\(_ :: SomeException) -> pure Nothing)
  pure $ mTxt >>= readMaybe . TL.unpack

limitValueAttr :: Maybe Int -> T.Text
limitValueAttr = maybe "" showT

hostFromHeader :: Maybe TL.Text -> T.Text
hostFromHeader = fromMaybe "localhost" . fmap cs

proxyEndpoint :: T.Text -> T.Text
proxyEndpoint host = "http://" <> host <> "/api/openai/v1/chat/completions"

pageHeader :: T.Text -> Maybe (Html ()) -> Html ()
pageHeader host mExtra = div_ [class_ "header-row"] $ do
  h1_ $ a_ [href_ "/ui/", style_ "color: inherit; text-decoration: none;"] "🔭 MixLLMProxy"
  a_ [href_ "/ui/aliases", class_ "nav-btn"] (icon "gear" >> " Aliases")
  endpointBox (proxyEndpoint host)
  case mExtra of
    Just extra -> extra
    Nothing -> pure ()

aliasPath :: Int -> T.Text -> T.Text
aliasPath aid suffix = "/ui/aliases/" <> showT aid <> suffix

aliasEditUrl :: Int -> T.Text
aliasEditUrl aid = aliasPath aid "/edit"

aliasUpdateUrl :: Int -> T.Text
aliasUpdateUrl aid = aliasPath aid "/update"

aliasDuplicateUrl :: Int -> T.Text
aliasDuplicateUrl aid = aliasPath aid "/duplicate"

aliasDeleteUrl :: Int -> T.Text
aliasDeleteUrl aid = aliasPath aid "/delete"

endpointPath :: Int -> T.Text -> T.Text
endpointPath eid suffix = "/ui/aliases/endpoints/" <> showT eid <> suffix

endpointEditUrl :: Int -> T.Text
endpointEditUrl eid = endpointPath eid "/edit"

endpointUpdateUrl :: Int -> T.Text
endpointUpdateUrl eid = endpointPath eid "/update"

endpointDeleteUrl :: Int -> T.Text
endpointDeleteUrl eid = endpointPath eid "/delete"

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
  span_ [class_ "alias-badge", style_ ("background: " <> aliasColor name)] (toHtml name)

aliasBadgeLinked :: Maybe T.Text -> T.Text -> Html ()
aliasBadgeLinked mFilterUrl name = case mFilterUrl of
  Just url ->
    a_ [href_ url, class_ "alias-badge-link", title_ ("Filter requests for " <> name)] $
      aliasBadge name
  Nothing ->
    aliasBadge name

aliasEditBadge :: Int -> Html ()
aliasEditBadge aid =
  a_ [href_ (aliasEditUrl aid), class_ "alias-edit-badge", title_ "Edit alias"] (icon "pencil")

aliasBadgeWithEdit :: Maybe T.Text -> Int -> T.Text -> Html ()
aliasBadgeWithEdit mFilterUrl aid name =
  div_ [class_ "alias-badge-row"] $ do
    aliasBadgeLinked mFilterUrl name
    aliasEditBadge aid

endpointBox :: T.Text -> Html ()
endpointBox url =
  div_ [class_ "endpoint-box"] $ do
    span_ [class_ "endpoint-label"] (icon "ph-link" >> " Endpoint:")
    code_ [class_ "endpoint-url"] (toHtml url)
