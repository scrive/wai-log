-- | A simple logging middleware for WAI applications that supports the 'log-*'
-- family of packages: <https://hackage.haskell.org/package/log-base>
--
-- FIXME
-- The example below is not accurate anymore, needs to be updated...
--
-- When logging to @stdout@ using 'defaultOptions', the output looks like this:
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
-- * Create a Middleware
  mkLogMiddleware
, mkLogMiddlewareWith
-- ** Type
, LogMiddleware
-- ** Options
, Options(..)
, defaultOptions
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
-- Use 'mkApplicationLoggerWith' for custom 'Options'
mkLogMiddleware :: MonadLog m => m LogMiddleware
mkLogMiddleware = mkLogMiddlewareWith defaultOptions

-- | Create a 'LogMiddleware' using the supplied 'Options'
mkLogMiddlewareWith :: MonadLog m => Options -> m LogMiddleware
mkLogMiddlewareWith options = do
  loggerIO <- getLoggerIO
  return $ logRequestsWith loggerIO options
