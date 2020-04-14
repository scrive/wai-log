-- | A simple logging middleware for WAI applications that supports the 'log-*'
-- family of packages: <https://hackage.haskell.org/package/log-base>
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
  mkApplicationLogger
, mkApplicationLoggerWith
-- ** Options
, Options(..)
, defaultOptions
) where

import Prelude hiding (log)

import Log (MonadLog, getLoggerIO)
import Network.Wai (Middleware)

import Network.Wai.Log.Internal
import Network.Wai.Log.Options

-- | Create a logging 'Middleware' using 'defaultOptions'
--
-- Use 'mkApplicationLoggerWith' for custom 'Options'
mkApplicationLogger :: MonadLog m => m Middleware
mkApplicationLogger = mkApplicationLoggerWith defaultOptions

-- | Create a logging 'Middleware' using the supplied 'Options'
mkApplicationLoggerWith :: MonadLog m => Options -> m Middleware
mkApplicationLoggerWith options = do
  logIO <- getLoggerIO
  return $ logRequestsWith logIO options
