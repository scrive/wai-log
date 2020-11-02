-- | A simple logging middleware for WAI applications that supports the 'log-*'
-- family of packages: <https://hackage.haskell.org/package/log-base>
--
-- When logging to @stdout@ using 'defaultOptions', the output looks like this:
--
-- @
-- 2020-10-27 12:30:23 INFO eid-server: Request received {
--   "url": "\/api\/v1\/transaction\/new",
--   "body_length": "KnownLength 136",
--   "method": \"POST\",
--   "user_agent": "curl/7.68.0",
--   "request_uuid": "f2c89425-9ec4-4cd2-ae56-4bab23681fce",
--   "remote_host": "127.0.0.1:34694"
-- }
-- 2020-10-27 12:30:23 INFO eid-server: Sending response {
--   "request_uuid": "f2c89425-9ec4-4cd2-ae56-4bab23681fce"
-- }
-- 2020-10-27 12:30:23 INFO eid-server: Request complete {
--   "response_body": null,
--   "url": "\/api\/v1\/transaction\/new",
--   "method": \"POST\",
--   "status": {
--     "code": 400,
--     "message": "Bad Request"
--   },
--   "time": {
--     "process": 2.97493e-3,
--     "full": 3.159565e-3
--   },
--   "request_uuid": "f2c89425-9ec4-4cd2-ae56-4bab23681fce"
-- }
-- @

module Network.Wai.Log (
-- * Create a Middleware
  mkLogMiddleware
, mkLogMiddlewareWith
-- ** Type
, LogMiddleware
-- ** Options
, Options(..)
, ResponseTime(..)
, defaultOptions
, defaultLogRequest
, defaultLogResponse
-- ** Helpers
, logRequestUUID
) where

import Prelude hiding (log)

import Data.UUID (UUID)
import Log (MonadLog, getLoggerIO)
import Network.Wai

import Network.Wai.Log.Internal
import Network.Wai.Log.Options

-- | The classic @wai@ 'Middleware' type is @Application -> Application@, but
-- that does not work when you want to pass logging context down to the
-- 'Application'.
--
-- Instead we pass a 'UUID' to the 'Application', containting the
-- @request_uuid@, so it can be logged in the application's context.
type LogMiddleware = (UUID -> Application) -> Application

-- | Create a 'LogMiddleware' using 'defaultOptions'
--
-- Use 'mkLogMiddlewareWith' for custom 'Options'
mkLogMiddleware :: MonadLog m => m LogMiddleware
mkLogMiddleware = mkLogMiddlewareWith defaultOptions

-- | Create a 'LogMiddleware' using the supplied 'Options'
mkLogMiddlewareWith :: MonadLog m => Options -> m LogMiddleware
mkLogMiddlewareWith options = do
  loggerIO <- getLoggerIO
  return $ logRequestsWith loggerIO options
