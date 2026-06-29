{-# LANGUAGE OverloadedStrings #-}

module RequestEventsJson
  ( requestSoundPollSeconds
  , requestSoundWindowSize
  , loadRequestEventsJson
  ) where

import Database.PostgreSQL.Simple (Connection)
import DB (RequestEvent(..), getRecentRequestEvents)
import qualified Data.Aeson as A

requestSoundPollSeconds, requestSoundWindowSize :: Int
requestSoundPollSeconds = 2
requestSoundWindowSize = 100

requestEventJson :: RequestEvent -> A.Value
requestEventJson e = A.object
  [ "id" A..= reId e
  , "status" A..= reStatus e
  , "alias" A..= reAliasName e
  ]

loadRequestEventsJson :: Connection -> IO A.Value
loadRequestEventsJson conn = do
  events <- getRecentRequestEvents conn requestSoundWindowSize
  pure $ A.object
    [ "poll_seconds" A..= requestSoundPollSeconds
    , "requests" A..= map requestEventJson events
    ]