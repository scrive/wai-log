module Network.Wai.Log.Options (
  Options(..)
, defaultOptions
-- * TODO
, defaultLogRquest
, defaultLogResponse
) where

import Data.Aeson.Types (Pair)
import Data.String.Conversions (ConvertibleStrings, StrictText, cs)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Log
import Network.HTTP.Types.Status
import Network.Wai

-- | Logging options
data Options = Options {
    logLevel            :: LogLevel
  , logRequest          :: Request -> [Pair]
  , logSendingResponse  :: Bool
  -- | The 'UTCTime' values in order are : start, end, full
  , logResponse         :: Response -> UTCTime -> UTCTime -> UTCTime -> [Pair]
  -- FIXME: ^ create custom 'Timings' type instead of this "mess"
  }

-- | Default 'Options'
--
-- @
-- { logLevel = LogInfo
-- , logRequest = defaultLogRquest
-- , logSendingResponse = True
-- , logResponse = defaultLogResponse
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
  { logLevel = LogInfo
  , logRequest = defaultLogRquest
  , logSendingResponse = True
  , logResponse = defaultLogResponse
  }

-- | Logs the following request values:
-- * method
-- * url path
-- * remote host
-- * user agent
-- * body-length
defaultLogRquest :: Request -> [Pair]
defaultLogRquest req =
  [ "method"      .= ts (requestMethod req)
  , "url"         .= ts (rawPathInfo req)
  , "remote-host" .= show (remoteHost req)
  , "user-agent"  .= fmap ts (requestHeaderUserAgent req)
  , "body-length" .= show (requestBodyLength req)
  ]

-- | Logs the following values for the response:
-- * status code
-- * status message
--
-- Also logs the "process" and "full" time
defaultLogResponse
  :: Response
  -> UTCTime -- ^ Start time
  -> UTCTime -- ^ Processing end time
  -> UTCTime -- ^ Full end time
  -> [Pair]
defaultLogResponse resp tStart tEnd tFull =
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
