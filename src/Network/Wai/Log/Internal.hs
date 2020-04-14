{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Data.Aeson.Types (Value, object)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Log (LogLevel)
import Network.Wai (Middleware)

import Network.Wai.Log.Options (Options(..), ResponseTime(..))

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

-- | Create a logging 'Middleware' given a 'LoggerIO' logging function and 'Options'
logRequestsWith :: LoggerIO -> Options -> Middleware
logRequestsWith loggerIO Options{..} app req respond = do
  uuid <- nextRandom
  logIO "Request received" . object . logRequest uuid $ req
  tStart <- getCurrentTime
  app req $ \resp -> do
    tEnd <- getCurrentTime
    case logSendingResponse of
      Nothing -> return ()
      Just logSR -> logIO "Sending response" . object . logSR $ uuid
    r <- respond resp
    tFull <- getCurrentTime
    let processing = diffUTCTime tEnd  tStart
        full       = diffUTCTime tFull tStart
        times      = ResponseTime{..}
    logIO "Request complete" . object $ logResponse uuid req resp times
    return r

  where

    logIO message value = do
      now <- getCurrentTime
      loggerIO now logLevel message value
