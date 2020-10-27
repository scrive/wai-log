# wai-log-0.3 (TODO)

* Rename `mkApplicationLogger` into `mkLogMiddleware`
* Generate ID for each request to correlate log messages
* Instead of logging three events: `Request received`, `Sending response` and
  `Request complete`, only log two of them: the first and the third one.

# wai-log-0.2 (2020-04-14)

* Entire interface has changed: now builds a `Middleware` in a `MonadLog`
  context
* Adds `Options` to control what is logged
* Currently logging behaviour is unchanged when using `defaultOptions`

# wai-log-0.1 (2019-03-07)

* Initial release (split from internal Scrive package).
