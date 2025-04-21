{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Data.Aeson.Types (ToJSON, Value(..), object)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Log (LogLevel)
import Network.Wai (Application, responseToStream)

import Network.Wai.Log.Options (Options(..), ResponseTime(..))

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

-- | Create a logging 'Middleware' that takes request id
-- given a 'LoggerIO' logging function and 'Options'
logRequestsWith :: ToJSON id => LoggerIO -> Options id -> (id -> Application) -> Application
logRequestsWith loggerIO Options{..} mkApp req respond = do
  reqId <- logGetRequestId req
  logIO "Request received" $ logRequest reqId req
  tStart <- getCurrentTime
  mkApp reqId req $ \resp -> do
    tEnd <- getCurrentTime
    logIO "Sending response" $ logResponseSending reqId req
    r <- respond resp
    tFull <- getCurrentTime
    let processing = diffUTCTime tEnd  tStart
        full       = diffUTCTime tFull tStart
        times      = ResponseTime{..}

    _ <- case logBody of
      Nothing ->
        logIO "Request complete" $ logResponse reqId req resp Null times
      Just bodyLogValueConstructorFunction ->
        let (status, responseHeaders, bodyToIO) = responseToStream resp
            mBodyLogValueConstructor =
              bodyLogValueConstructorFunction req status responseHeaders
        in case mBodyLogValueConstructor of
          Nothing ->
            logIO "Request complete" $ logResponse reqId req resp Null times
          Just bodyLogValueConstructor ->
            bodyToIO $ \streamingBodyToIO ->
              let logWithBuilder :: Builder -> IO ()
                  logWithBuilder b = logIO "Request complete" $
                    logResponse reqId req resp (bodyLogValueConstructor b) times

              in streamingBodyToIO logWithBuilder (return ())
    return r

  where
    logIO message pairs = do
      now <- getCurrentTime
      loggerIO now logLevel message (object pairs)
