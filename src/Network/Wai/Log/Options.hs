module Network.Wai.Log.Options (
-- * Options & Timing
  Options(..)
, ResponseTime(..)
-- * Defaults
, defaultOptions
, defaultLogRequest
, defaultLogResponse
-- * Helpers
, mkOpaqueDefaultOptions
, logRequestId
, requestId
) where

import Data.Aeson.Types (ToJSON, Pair, Value)
import Data.ByteString.Builder (Builder)
import Data.String.Conversions (ConvertibleStrings, StrictText, cs)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Log
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, statusCode, statusMessage)
import Network.Wai

-- | Logging options
--
-- Logging response body involves extracting it from @Response@ via IO operations,
-- therefore the @logBody@ option takes @Request@, @Status@ and @ResponseHeaders@
-- as arguments to decide whether the IO operations of body extraction have
-- to be performed.
-- The resulting @Maybe@ function is the constructor of a loggable @Value@
-- from the body bytestring builder.
data Options id = Options {
    logLevel    :: LogLevel
  , logRequest  :: id -> Request -> [Pair]
  , logResponse :: id -> Request -> Response -> Value -> ResponseTime -> [Pair]
  -- | An optional constructor of the response body log value.
  , logBody :: Maybe (Request -> Status -> ResponseHeaders -> Maybe (Builder -> Value))
  -- | A function for getting the request id
  , logGetRequestId :: Request -> IO id
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
-- , logGetRequestId = 'const nextRandom'
-- }
-- @
defaultOptions :: Options UUID
defaultOptions = Options
  { logLevel = LogInfo
  , logRequest = defaultLogRequest
  , logResponse = defaultLogResponse
  , logBody = Nothing
  , logGetRequestId = const nextRandom
  }

-- | Build a default 'Options' record for an opaque id given a function
-- for retrieving an id.
mkOpaqueDefaultOptions :: ToJSON id => (Request -> IO id) -> Options id
mkOpaqueDefaultOptions getReqId = Options
  { logLevel = LogInfo
  , logRequest = defaultLogRequest
  , logResponse = defaultLogResponse
  , logBody = Nothing
  , logGetRequestId = getReqId
  }

-- | Logs the following request values:
--
-- * request_uuid
-- * method
-- * url path
-- * remote host
-- * user agent
-- * body-length
defaultLogRequest :: ToJSON id => id -> Request -> [Pair]
defaultLogRequest reqId req =
  [ "request_id"   .= reqId
  , "method"       .= ts (requestMethod req)
  , "url"          .= ts (rawPathInfo req)
  , "remote_host"  .= show (remoteHost req)
  , "user_agent"   .= fmap ts (requestHeaderUserAgent req)
  , "body_length"  .= show (requestBodyLength req)
  ]

-- | Logs the following values:
--
-- * request_id
-- * request method
-- * request url path
-- * response_body details provided as 'Value'
-- * status code
-- * status message
-- * time full
-- * time processing
--
-- Time is in seconds as that is how 'NominalDiffTime' is treated by default
defaultLogResponse :: ToJSON id => id -> Request -> Response -> Value -> ResponseTime -> [Pair]
defaultLogResponse reqId req resp responseBody time =
  [ "request_id" .= reqId
  , "method" .= ts (requestMethod req)
  , "url" .= ts (rawPathInfo req)
  , "response_body" .= responseBody
  , "response_code" .= statusCode (responseStatus resp)
  , "response_message" .= ts (statusMessage $ responseStatus resp)
  , "full_time" .= full time
  , "elapsed_time" .= processing time
  ]

-- | Helper to consistently log the request id in your application by adding
-- @request_id@ field to log's 'localData'
logRequestId :: (MonadLog m, ToJSON id) => id -> m a -> m a
logRequestId = localData . requestId

-- | Logs the following values:
--
-- * request_id
requestId :: ToJSON id => id -> [Pair]
requestId reqId =
  [ "request_id" .= reqId ]

ts :: ConvertibleStrings a StrictText => a -> Text
ts = cs
