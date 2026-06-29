{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module ChartJsonSpec (chartJsonTests, chartDbTests) where

import ChartJson (buildChartJson, chartWindowMinutes, chartBucketSeconds)
import DB
  ( AliasChartRow(..)
  , LlmAlias(..)
  , connectDB
  , getAliasRequestChartData
  , getAliasByName
  , insertEndpoint
  , insertAlias
  , insertRequest
  , deleteAlias
  )
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (UTCTime(..), Day, fromGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Environment (lookupEnv)
import qualified Control.Exception as E
import Control.Monad (void, replicateM_)
import Database.PostgreSQL.Simple (Connection, Only(..), execute, execute_)
import Data.List (nub)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)

data ChartAliasJSON = ChartAliasJSON
  { chartId :: Int
  , chartName :: T.Text
  , chartColor :: T.Text
  , chartCounts :: [Int]
  } deriving (Show, Eq, Generic)

instance A.FromJSON ChartAliasJSON where
  parseJSON = A.genericParseJSON $ A.defaultOptions
    { A.fieldLabelModifier = \case
        "chartId" -> "id"
        "chartName" -> "name"
        "chartColor" -> "color"
        "chartCounts" -> "counts"
        x -> x
    }

data ChartResponseJSON = ChartResponseJSON
  { window_minutes :: Int
  , bucket_seconds :: Int
  , labels :: [T.Text]
  , aliases :: [ChartAliasJSON]
  } deriving (Show, Eq, Generic)
instance A.FromJSON ChartResponseJSON

testDay :: Day
testDay = fromGregorian 2026 6 25

chartRow :: Int -> T.Text -> Integer -> Int -> AliasChartRow
chartRow aid name secs count = AliasChartRow aid name (UTCTime testDay (fromIntegral secs)) count

parseChart :: [AliasChartRow] -> Maybe ChartResponseJSON
parseChart = A.decode . A.encode . buildChartJson

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok then putStrLn $ "PASS: " ++ msg
        else error $ "FAIL: " ++ msg

chartJsonTests :: IO ()
chartJsonTests = do
  putStrLn "=== buildChartJson unit tests ==="

  assert "empty rows" $
    case parseChart [] of
      Just r ->
        window_minutes r == chartWindowMinutes
        && bucket_seconds r == chartBucketSeconds
        && labels r == []
        && aliases r == []
      _ -> False

  let rows =
        [ chartRow 2 "beta" 20 1
        , chartRow 1 "alpha" 10 2
        , chartRow 1 "alpha" 20 3
        ]
  assert "groups and sorts aliases" $
    case parseChart rows of
      Just r ->
        case aliases r of
          [a1, a2] ->
            chartId a1 == 1 && chartName a1 == "alpha" && chartCounts a1 == [2, 3]
            && chartId a2 == 2 && chartName a2 == "beta" && chartCounts a2 == [1]
          _ -> False
      _ -> False

  assert "labels follow first alias buckets" $
    maybe False ((== ["00:00:10", "00:00:20"]) . labels) (parseChart rows)

  assert "includes alias color" $
    case parseChart [chartRow 1 "alpha" 0 1] >>= listToMaybe . aliases of
      Just a -> T.isPrefixOf "hsl(" (chartColor a)
      _ -> False

chartDbTests :: IO ()
chartDbTests = do
  putStrLn "\n=== getAliasRequestChartData integration test ==="
  mHost <- lookupEnv "DB_HOST"
  case mHost of
    Nothing -> putStrLn "SKIP: chart DB test (no DB_HOST)"
    Just _ -> do
      result <- E.try @E.SomeException runChartDbTest
      case result of
        Right () -> pure ()
        Left err -> putStrLn $ "FAIL: chart DB test: " ++ show err

cleanupChartTests :: Connection -> IO ()
cleanupChartTests conn = do
  void $ execute_ conn "DELETE FROM llm_requests WHERE alias_name LIKE '_chart_test_%'"
  void $ execute_ conn "DELETE FROM aliases WHERE name LIKE '_chart_test_%'"
  void $ execute_ conn "DELETE FROM endpoints WHERE name LIKE '_chart_test_%'"

runChartDbTest :: IO ()
runChartDbTest = do
  conn <- connectDB
  cleanupChartTests conn
  now <- getCurrentTime
  let aliasName = "_chart_test_" <> T.pack (show (floor (utcTimeToPOSIXSeconds now) :: Integer))
  endpointId <- insertEndpoint conn (aliasName <> "-ep") "http://test" "key"
  insertAlias conn aliasName endpointId "model" Nothing Nothing
  alias <- getAliasByName conn aliasName >>= \case
    Just a -> pure a
    Nothing -> error "inserted chart test alias not found"
  replicateM_ 3 $
    insertRequest conn "/api" "POST" Nothing (Just 200) Nothing 1.0 (Just "model") Nothing Nothing Nothing (Just aliasName)
  rows <- getAliasRequestChartData conn chartWindowMinutes chartBucketSeconds
  let ours = filter ((== aliasName) . acrAliasName) rows
      buckets = nub $ map acrBucket rows
  assert "returns rows for test alias" (not (null ours))
  assert "bucket counts are non-negative" (all ((>= 0) . acrCount) ours)
  assert "recent requests appear in chart" (sum (map acrCount ours) >= 3)
  assert "all aliases share bucket timeline" (all (\r -> acrBucket r `elem` buckets) ours)
  void $ execute conn "DELETE FROM llm_requests WHERE alias_name = ?" (Only aliasName)
  deleteAlias conn (laId alias)
  putStrLn "PASS: chart DB integration"

