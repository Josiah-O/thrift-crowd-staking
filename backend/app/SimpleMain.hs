{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

app :: Application
app _ respond = respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    "{\"message\": \"Hello from Thrift Crowd Staking!\"}"

main :: IO ()
main = do
    putStrLn "Starting Thrift Crowd Staking backend on port 8080..."
    run 8080 app
