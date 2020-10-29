{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Data.Aeson.Types (Value(..), object)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Log (LogLevel)
import Network.Wai (Application, responseToStream)

import Network.Wai.Log.Options (Options(..), ResponseTime(..), logSendingResponse)

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

-- | Create a logging 'Middleware' that takes request UUID
-- given a 'LoggerIO' logging function and 'Options'
logRequestsWith :: LoggerIO -> Options -> (UUID -> Application) -> Application
logRequestsWith loggerIO Options{..} mkApp req respond = do
  uuid <- nextRandom
  logIO "Request received" $ logRequest uuid req
  tStart <- getCurrentTime
  mkApp uuid req $ \resp -> do
    tEnd <- getCurrentTime
    logIO "Sending response" . logSendingResponse $ uuid
    r <- respond resp
    tFull <- getCurrentTime
    let processing = diffUTCTime tEnd  tStart
        full       = diffUTCTime tFull tStart
        times      = ResponseTime{..}

    _ <- case logBody of
      Nothing ->
        logIO "Request complete" $ logResponse uuid req resp Null times
      Just constructBodyToLog -> do
        let (status, _headers, bodyToIO) = responseToStream resp
        bodyToIO $ \streamingBody ->
              let bodyValue :: Builder -> Value
                  bodyValue = constructBodyToLog status

                  logWithBuilder :: Builder -> IO ()
                  logWithBuilder b = logIO "Request complete" $
                    logResponse uuid req resp (bodyValue b) times

              in streamingBody logWithBuilder (return ())
    return r

  where
    logIO message pairs = do
      now <- getCurrentTime
      loggerIO now logLevel message (object pairs)
