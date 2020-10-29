# wai-log-0.3 (2020-10-27)

* Rename `mkApplicationLogger` into `mkLogMiddleware`
* Generate ID for each request to correlate log messages
* Remove optional `logSendingResponse`, which means always logging `Sending response`
* Log `method` and `url` in `Request complete` too
* Log `response_body` on 4xx and 5xx

# wai-log-0.2 (2020-04-14)

* Entire interface has changed: now builds a `Middleware` in a `MonadLog`
  context
* Adds `Options` to control what is logged
* Currently logging behaviour is unchanged when using `defaultOptions`

# wai-log-0.1 (2019-03-07)

* Initial release (split from internal Scrive package).
