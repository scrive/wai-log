# wai-log-0.3 (2020-11-02)

* Rename `mkApplicationLogger` into `mkLogMiddleware`
* Generate ID for each request to correlate log messages
* Remove optional `logSendingResponse` and always log `Sending response`
* Log `method` and `url` in `defaultLogResponse` too
* Add `logBody` option to log `response_body` depending on the request and
  response parameters
* Use `snake_case` consistently in log output fields for `defaultOptions`

# wai-log-0.2 (2020-04-14)

* Entire interface has changed: now builds a `Middleware` in a `MonadLog`
  context
* Adds `Options` to control what is logged
* Currently logging behaviour is unchanged when using `defaultOptions`

# wai-log-0.1 (2019-03-07)

* Initial release (split from internal Scrive package).
