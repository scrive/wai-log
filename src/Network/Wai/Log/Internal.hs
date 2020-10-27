{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Log.Internal where

import Data.Aeson.Types (Value, object)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Log (LogLevel)
import Network.HTTP.Types.Status (statusIsClientError, statusIsServerError)
import Network.Wai (Application, responseToStream)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

import Network.Wai.Log.Options (Options(..), ResponseTime(..))

-- | This type matches the one returned by 'getLoggerIO'
type LoggerIO = UTCTime -> LogLevel -> Text -> Value -> IO ()

logRequestsWith :: LoggerIO -> Options -> (UUID -> Application) -> Application
logRequestsWith loggerIO Options{..} mkApp req respond = do
  uuid <- nextRandom
  logIO "Request received" $ logRequest uuid req
  tStart <- getCurrentTime
  mkApp uuid req $ \resp -> do
    tEnd <- getCurrentTime
    r <- respond resp
    tFull <- getCurrentTime
    let processing = diffUTCTime tEnd  tStart
        full       = diffUTCTime tFull tStart
        times      = ResponseTime{..}

    let (status, _headers, bodyToIO) = responseToStream resp
    _ <- if statusIsClientError status || statusIsServerError status
      then
        bodyToIO $ \streamingBody ->
          let builderToText :: Builder -> Text
              builderToText b = T.decodeUtf8 $ BSL.toStrict $ toLazyByteString b
              logWithBuilder :: Builder -> IO ()
              logWithBuilder b =
                let body = Just $ builderToText b
                in logIO "Request complete" $ logResponse uuid req resp body times
          in streamingBody logWithBuilder (return ())
      else
        logIO "Request complete" $ logResponse uuid req resp Nothing times
    return r

  where
    logIO message pairs = do
      now <- getCurrentTime
      loggerIO now logLevel message (object pairs)
