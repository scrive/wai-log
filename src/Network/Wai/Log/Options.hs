module Network.Wai.Log.Options (
-- * Options & Timing
  Options(..)
, ResponseTime(..)
, logRequestUUID
-- * Defaults
, defaultOptions
, defaultLogRequest
, defaultLogResponse
) where

import Data.Aeson.Types (Pair, Value)
import Data.ByteString.Builder (Builder)
import Data.String.Conversions (ConvertibleStrings, StrictText, cs)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.UUID (UUID)
import Log
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import Network.Wai

-- | Logging options
--
-- Logging response body involves extracting it from @Response@ via IO operations,
-- therefore the @logBody@ option takes @Request@, @Status@ and @ResponseHeaders@
-- as arguments to decide whether the IO operations of body extraction have
-- to be permormed.
-- The resulting @Maybe@ function is the constructor of a loggable @Value@
-- from the body bytestring builder.
data Options = Options {
    logLevel    :: LogLevel
  , logRequest  :: UUID -> Request -> [Pair]
  , logResponse :: UUID -> Request -> Response -> Value -> ResponseTime -> [Pair]
  -- | An optional constructor of the response body log value.
  , logBody :: Maybe (Request -> Status -> ResponseHeaders -> Maybe (Builder -> Value))
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
  , logBody = Nothing
  }

-- | Logs the following request values:
--
-- * request_uuid
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
  , "remote_host"  .= show (remoteHost req)
  , "user_agent"   .= fmap ts (requestHeaderUserAgent req)
  , "body_length"  .= show (requestBodyLength req)
  ]

-- | Logs the following values:
--
-- * request_uuid
logRequestUUID :: UUID -> [Pair]
logRequestUUID uuid =
  [ "request_uuid" .= uuid ]

-- | Logs the following values:
--
-- * request_uuid
-- * request method
-- * request url path
-- * response_body details provided as 'Value'
-- * status code
-- * status message
-- * time full
-- * time processing
--
-- Time is in seconds as that is how 'NominalDiffTime' is treated by default
defaultLogResponse :: UUID -> Request -> Response -> Value -> ResponseTime -> [Pair]
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
