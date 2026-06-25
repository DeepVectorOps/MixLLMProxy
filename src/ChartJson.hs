{-# LANGUAGE OverloadedStrings #-}

module ChartJson
  ( chartWindowMinutes
  , chartBucketSeconds
  , chartPollSeconds
  , chartSubtitle
  , buildChartJson
  , loadChartJson
  ) where

import Database.PostgreSQL.Simple (Connection)
import DB (AliasChartRow(..), getAliasRequestChartData)
import Common (aliasColor, showT)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.String.Conversions (cs)

chartWindowMinutes, chartBucketSeconds, chartPollSeconds :: Int
chartWindowMinutes = 10
chartBucketSeconds = 10
chartPollSeconds = 5

chartSubtitle :: T.Text
chartSubtitle =
  "Requests per " <> showT chartBucketSeconds <> "s · last "
    <> showT chartWindowMinutes <> " minutes · updates every "
    <> showT chartPollSeconds <> "s"

formatBucketLabel :: UTCTime -> T.Text
formatBucketLabel = cs . formatTime defaultTimeLocale "%H:%M:%S"

chartEnvelope :: [T.Text] -> [A.Value] -> A.Value
chartEnvelope labels aliases = A.object
  [ "window_minutes" A..= chartWindowMinutes
  , "bucket_seconds" A..= chartBucketSeconds
  , "labels" A..= labels
  , "aliases" A..= aliases
  ]

seriesJson :: [AliasChartRow] -> A.Value
seriesJson (r:rs) = A.object
  [ "id" A..= acrAliasId r
  , "name" A..= acrAliasName r
  , "color" A..= aliasColor (acrAliasName r)
  , "counts" A..= map acrCount (r:rs)
  ]
seriesJson [] = A.object []

buildChartJson :: [AliasChartRow] -> A.Value
buildChartJson rows =
  let grps = groupBy ((==) `on` acrAliasId) $ sortOn (\r -> (acrAliasId r, acrBucket r)) rows
  in case grps of
    [] -> chartEnvelope [] []
    (g:_) -> chartEnvelope (map (formatBucketLabel . acrBucket) g) (map seriesJson grps)

loadChartJson :: Connection -> IO A.Value
loadChartJson conn =
  buildChartJson <$> getAliasRequestChartData conn chartWindowMinutes chartBucketSeconds