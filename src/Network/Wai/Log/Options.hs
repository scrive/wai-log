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
import Data.UUID (UUID)
import Log
import Network.HTTP.Types.Status
import Network.Wai

-- | Logging options
data Options = Options {
    logLevel    :: LogLevel
  , logRequest  :: UUID -> Request -> [Pair]
  , logResponse :: UUID -> Request -> Response -> Maybe Text -> ResponseTime -> [Pair]
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
-- , logResponse = 'defaultLogResponse'
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
  { logLevel = LogInfo
  , logRequest = defaultLogRequest
  , logResponse = defaultLogResponse
  }

-- | Logs the following request values:
--
-- * method
-- * url path
-- * remote host
-- * user agent
-- * body-length
defaultLogRequest :: UUID -> Request -> [Pair]
defaultLogRequest uuid req =
  [ "request_uuid" .= uuid
  , "method"       .= ts (requestMethod req)
  , "url"          .= ts (rawPathInfo req)
  , "remote-host"  .= show (remoteHost req)
  , "user-agent"   .= fmap ts (requestHeaderUserAgent req)
  , "body-length"  .= show (requestBodyLength req)
  ]


-- | Logs the following values:
--
-- * status code
-- * status message
-- * time full
-- * time processing
--
-- Nothing from the 'Request' is logged
--
-- Time is in seconds as that is how 'NominalDiffTime' is treated by default
defaultLogResponse :: UUID -> Request -> Response -> Maybe Text -> ResponseTime -> [Pair]
defaultLogResponse uuid req resp responseBody time =
    [ "request_uuid"  .= uuid
    , "method"        .= ts (requestMethod req)
    , "url"           .= ts (rawPathInfo req)
    , "response_body" .= responseBody
    , "status" .= object [ "code"    .= statusCode (responseStatus resp)
                         , "message" .= ts (statusMessage (responseStatus resp))
                         ]
    , "time"   .= object [ "full"    .= full time
                         , "process" .= processing time
                         ]
    ]

ts :: ConvertibleStrings a StrictText => a -> Text
ts = cs
