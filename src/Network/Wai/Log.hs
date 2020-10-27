-- | A simple logging middleware for WAI applications that supports the 'log-*'
-- family of packages: <https://hackage.haskell.org/package/log-base>
--
-- When logging to @stdout@ using 'defaultOptions', the output looks like this:
--
-- @
-- 2020-10-27 09:37:31 INFO eid-server: Request received {
--   "url": "\/api\/myapi",
--   "body-length": "KnownLength 0",
--   "method": "GET",
--   "user-agent": "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:81.0) Gecko/20100101 Firefox/81.0",
--   "request_uuid": "9f86d3dd-026a-4a6d-801e-d466440819e1",
--   "remote-host": "127.0.0.1:33178"
-- }
-- 2020-10-27 09:37:31 INFO eid-server: Sending response {
--   "request_uuid": "9f86d3dd-026a-4a6d-801e-d466440819e1"
-- }
-- 2020-10-27 09:37:31 INFO eid-server: Request complete {
--   "status": {
--     "code": 200,
--     "message": "OK"
--   },
--   "time": {
--     "process": 4.504112e-3,
--     "full": 4.619166e-3
--   },
--   "request_uuid": "9f86d3dd-026a-4a6d-801e-d466440819e1"
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
