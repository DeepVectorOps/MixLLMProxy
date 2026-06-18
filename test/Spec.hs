{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text as T
import Network.HTTP.Client
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.HTTP.Types (status200)
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Network.Socket (withSocketsDo)

import Route.OpenAI (buildRequest)

main :: IO ()
main = withSocketsDo $ do
  putStrLn "=== buildRequest sets responseTimeoutNone ==="
  req <- buildRequest "http://example.com" "sk-test" "{}"
  if responseTimeout req == responseTimeoutNone
    then putStrLn "PASS"
    else error $ "FAIL: " <> show (responseTimeout req)

  putStrLn "\n=== 30s threadDelay server (would fail default 30s timeout) ==="
  let slowApp :: Application
      slowApp _ respond = do
        threadDelay 30000000
        respond $ responseLBS status200 [] "{\"choices\":[{\"message\":{\"content\":\"10\"}}]}"

  testWithApplication (pure slowApp) $ \port -> do
    let url = "http://127.0.0.1:" <> T.pack (show port)
    let body = "{\"model\":\"gpt-4\",\"messages\":[{\"role\":\"user\",\"content\":\"what is 5+5\"}]}"

    manager <- newManager defaultManagerSettings

    -- 1s timeout -> fails fast
    req1s <- buildRequest url "sk-test" body
    result <- catch (Right <$> httpLbs (req1s { responseTimeout = responseTimeoutMicro 1000000 }) manager) $
      \(e :: HttpException) -> pure (Left e)
    case result of
      Left (HttpExceptionRequest _ ResponseTimeout) ->
        putStrLn "PASS: 1s timeout failed on 30s server (timeout mechanism works)"
      _ -> error "FAIL: 1s timeout should have triggered"

    -- no timeout -> survives 30s
    putStrLn "Sending 'what is 5+5' with no timeout (expect ~30s wait)..."
    reqNone <- buildRequest url "sk-test" body
    respNone <- httpLbs reqNone manager
    if responseStatus respNone == status200
      then putStrLn "PASS: responseTimeoutNone survived 30s delay, got response"
      else error $ "FAIL: " <> show (responseStatus respNone)

  putStrLn "\nDONE. If responseTimeoutDefault (30s) was in effect, test would have timed out."
