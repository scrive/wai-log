# wai-log-0.4.0.1 (2022-11-23)
* Export `mkOpaqueDefaultOptions` helper function

# wai-log-0.4 (2022-11-22)
* Parametrise the `LogMiddleware` type with an opaque type parameter for the request id
* Parametrise the `Options` record type with the opaque type parameter representing the request id
* Add new field to the `Options id` record type `logGetRequestId :: Request -> IO id` which represents a function for getting/generating a request id given a `Request`
* Update helper functions `logRequestId` and `requestId` to reflect changes to the request id type
* Add new helper function `mkOpaqueDefaultOptions` for building an `Options id` record for an opaque id
* Flatten the field structure of the log response emitted by `defaultLogResponse`

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
