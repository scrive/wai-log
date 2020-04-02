-- | A simple logging middleware for WAI applications that supports the 'log-*'
-- family of packages: <https://hackage.haskell.org/package/log-base>
--
-- Currently there are no logging options but contributions are welcome.
-- When logging to @stdout@, the output looks like this:
--
-- @
-- 2019-02-21 19:51:47 INFO my-server: Request received {
--   \"url\": \"\/api\/myapi\",
--   \"body-length\": \"KnownLength 0\",
--   \"method\": \"GET\",
--   \"user-agent\": \"curl\/7.54.0\",
--   \"remote-host\": \"127.0.0.1:61249\"
-- }
-- 2019-02-21 19:51:47 INFO my-server: Sending response
-- 2019-02-21 19:51:47 INFO my-server: Request complete {
--   \"status\": {
--     \"code\": 200,
--     \"message\": \"OK\"
--   },
--   \"time\": {
--     \"process\": 2.224e-3,
--     \"full\": 2.348e-3
--   }
-- }
-- @
module Network.Wai.Log (
  logRequestsWith
) where

import Data.Aeson ()
import Data.String.Conversions (ConvertibleStrings, StrictText, cs)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Log
import Network.HTTP.Types.Status
import Network.Wai

-- | Given a logger, create a 'Middleware' that logs incoming requests, the
-- response code, and how long it took to process and respond to the request.
logRequestsWith :: (LogT IO () -> IO ()) -> Middleware
logRequestsWith runLogger app req respond = do

  runLogger $ logRequest req
  tStart <- getCurrentTime

  app req $ \resp -> do
    tEnd <- getCurrentTime
    runLogger $ logInfo_ "Sending response"
    r <- respond resp
    tFull <- getCurrentTime

    runLogger $ logResponseAndCompletionTimes resp tStart tEnd tFull

    return r

logRequest :: MonadLog m => Request -> m ()
logRequest req = logInfo "Request received" $ object
  [ "method"      .= ts (requestMethod req)
  , "url"         .= ts (rawPathInfo req)
  , "remote-host" .= show (remoteHost req)
  , "user-agent"  .= fmap ts (requestHeaderUserAgent req)
  , "body-length" .= show (requestBodyLength req)
  ]

logResponseAndCompletionTimes
  :: MonadLog m
  => Response
  -> UTCTime -- ^ Start time
  -> UTCTime -- ^ Processing end time
  -> UTCTime -- ^ Full end time
  -> m ()
logResponseAndCompletionTimes resp tStart tEnd tFull =
  logInfo "Request complete" $ object
    [ "status" .= object [ "code"    .= statusCode (responseStatus resp)
                         , "message" .= ts (statusMessage (responseStatus resp))
                         ]
    , "time"   .= object [ "full"    .= diffSeconds tFull tStart
                         , "process" .= diffSeconds tEnd tStart
                         ]
    ]

diffSeconds :: UTCTime -> UTCTime -> Double
diffSeconds a b = realToFrac $ diffUTCTime a b

ts :: ConvertibleStrings a StrictText => a -> Text
ts = cs
