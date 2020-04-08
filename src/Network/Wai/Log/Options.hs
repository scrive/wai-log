module Network.Wai.Log.Options (
-- * Options & Timing
  Options(..)
, ResponseTime(..)
-- * Defaults
, defaultOptions
, defaultLogRequest
, defaultLogResponse
) where

import Data.Aeson.Types (Pair)
import Data.String.Conversions (ConvertibleStrings, StrictText, cs)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Log
import Network.HTTP.Types.Status
import Network.Wai

-- | Logging options
data Options = Options {
    logLevel            :: LogLevel
  , logRequest          :: Request -> [Pair]
  , logSendingResponse  :: Bool
  , logResponse         :: Response -> ResponseTime -> [Pair]
  }

-- | Timing data
data ResponseTime = ResponseTime {
  -- | Time between request received and application finished processing request
    processing :: NominalDiffTime
  -- | Time between request received and response sent
  , full       :: NominalDiffTime
  }

-- | Default 'Options'
--
-- @
-- { logLevel = 'LogInfo'
-- , logRequest = 'defaultLogRequest'
-- , logSendingResponse = True
-- , logResponse = 'defaultLogResponse'
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
  { logLevel = LogInfo
  , logRequest = defaultLogRequest
  , logSendingResponse = True
  , logResponse = defaultLogResponse
  }

-- | Logs the following request values:
--
-- * method
-- * url path
-- * remote host
-- * user agent
-- * body-length
defaultLogRequest :: Request -> [Pair]
defaultLogRequest req =
  [ "method"      .= ts (requestMethod req)
  , "url"         .= ts (rawPathInfo req)
  , "remote-host" .= show (remoteHost req)
  , "user-agent"  .= fmap ts (requestHeaderUserAgent req)
  , "body-length" .= show (requestBodyLength req)
  ]

-- | Logs the following values:
--
-- * status code
-- * status message
-- * time full
-- * time processing
--
-- Time is in seconds as that is how 'NominalDiffTime' is treated by default
defaultLogResponse :: Response -> ResponseTime -> [Pair]
defaultLogResponse resp time =
    [ "status" .= object [ "code"    .= statusCode (responseStatus resp)
                         , "message" .= ts (statusMessage (responseStatus resp))
                         ]
    , "time"   .= object [ "full"    .= full time
                         , "process" .= processing time
                         ]
    ]

ts :: ConvertibleStrings a StrictText => a -> Text
ts = cs
