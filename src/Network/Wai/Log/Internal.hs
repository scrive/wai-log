{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Control.Monad (when)
import Data.Aeson.Types (Value, object, emptyObject)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Log (LogLevel)
import Network.Wai (Middleware)

import Network.Wai.Log.Options (Options(..))

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

-- | Create a logging 'Middleware' given a 'LoggerIO' logging function and 'Options'
logRequestsWith :: LoggerIO -> Options -> Middleware
logRequestsWith loggerIO Options{..} app req respond = do
  logIO "Request received" . object . logRequest $ req
  tStart <- getCurrentTime
  app req $ \resp -> do
    tEnd <- getCurrentTime
    when logSendingResponse $
         logIO_ "Sending response"
    r <- respond resp
    tFull <- getCurrentTime
    logIO "Request complete" . object $
      logResponse resp tStart tEnd tFull
    return r

  where

    logIO message value = do
      now <- getCurrentTime
      loggerIO now logLevel message value

    logIO_ m = logIO m emptyObject
