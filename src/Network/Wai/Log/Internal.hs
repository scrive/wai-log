{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Data.Aeson.Types (Value, object)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Log (LogLevel)
import Network.Wai (Application)

import Network.Wai.Log.Options (Options(..), ResponseTime(..))

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

logRequestsWith :: LoggerIO -> Options -> (UUID -> Application) -> Application
logRequestsWith loggerIO Options{..} mkApp req respond = do
  uuid <- nextRandom
  logIO "Request received" $ logRequest uuid req
  tStart <- getCurrentTime
  (mkApp uuid) req $ \resp -> do
    tEnd <- getCurrentTime
    r <- respond resp
    tFull <- getCurrentTime
    let processing = diffUTCTime tEnd  tStart
        full       = diffUTCTime tFull tStart
        times      = ResponseTime{..}
    logIO "Request complete" $ logResponse uuid req resp times
    return r

  where
    logIO message pairs = do
      now <- getCurrentTime
      loggerIO now logLevel message (object pairs)
