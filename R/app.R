#' Start or stop the built-in HTTP server
#'
#' `httpd_start()` starts a minimal HTTP server that listens on a local TCP
#' port and dispatches incoming requests to registered handler functions.
#' `httpd_stop()` stops the server and removes all registered apps.
#'
#' In interactive R sessions the server integrates with R's event loop:
#' connections are handled immediately without the user needing to press Enter
#' (on Unix/macOS via `addInputHandler`; on Windows via `R_PolledEvents`).
#' @param host The host/IP address to bind to.  Use `"127.0.0.1"` (default)
#'   for local-only connections, or `"0.0.0.0"` for all network interfaces.
#' @param ports A vector of candidate port numbers.  The first available port
#'   is used.
#' @return `httpd_start()` returns the base server URL invisibly (e.g.
#'   `"http://127.0.0.1:4322/"`).  `httpd_stop()` returns nothing.
#' @export
httpd_start = function(host = '127.0.0.1', ports = 4321 + 1:10) {
  port = .Call(C_httpd_start, as.integer(ports), as.character(host))
  if (port < 0L) stop2("Failed to start HTTP server on any of the specified ports.")
  .httpd$port = port
  .httpd$host = host

  if (interactive()) {
    poll_fn = function() {
      apps = .httpd$apps
      if (length(apps)) .Call(C_httpd_poll, names(apps), unname(apps))
    }
    .Call(C_httpd_set_input_handler, poll_fn)
  }

  url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
  invisible(sprintf('http://%s:%d/', url_host, port))
}

#' @rdname httpd_start
#' @export
httpd_stop = function() {
  .Call(C_httpd_set_input_handler, NULL)
  .Call(C_httpd_stop)
  .httpd$apps = list()
  .httpd$port = NULL
  .httpd$host = NULL
}

#' Create or stop a local web application
#'
#' `new_app()` registers a handler function under a named URL path and starts
#' the HTTP server if it is not already running.  It is a higher-level wrapper
#' around `httpd_start()` intended for interactive use.
#'
#' `stop_app()` deregisters one or more apps and shuts the server down when no
#' apps remain.
#' @param name The app name (a character string; each app needs a unique name).
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.  The
#'   arguments are:
#'   \describe{
#'     \item{`path`}{character(1): URL path relative to the app root (never
#'       empty; the bare root maps to `"."`).}
#'     \item{`query`}{Named character vector of URL-decoded query parameters.}
#'     \item{`post`}{Raw vector containing the request body (length 0 for GET
#'       requests).}
#'     \item{`headers`}{Raw vector of request headers in the form
#'       `"Request-Method: METHOD\nField: value\n..."`.}
#'   }
#'   The return value should be a named list with one or more of:
#'   \describe{
#'     \item{`payload`}{character(1): the response body.}
#'     \item{`file`}{character(1): path to a file to stream as the body.}
#'     \item{`content-type`}{character(1): MIME type (default
#'       `"text/html; charset=UTF-8"`).}
#'     \item{`status code`}{integer(1): HTTP status code (default 200).}
#'     \item{`header`}{Named character vector of extra response headers.}
#'   }
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it.  In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking, so additional apps can be registered afterwards.
#' @param host Passed to `httpd_start()`.  Only used when the server is not yet
#'   running, or when the user explicitly provides a value that differs from the
#'   running server's host.
#' @param ports Passed to `httpd_start()`.  Only used when the server is not yet
#'   running, or when the user explicitly provides values that differ from the
#'   currently bound port.
#' @return `new_app()` returns the app URL of the form
#'   `"http://host:port/name/"` (invisibly).  `stop_app()` returns nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', ports = 4321 + 1:10
) {
  # Restart the server when the user explicitly provides host/ports that differ
  # from the currently running server; otherwise start only if not yet running.
  needs_start = is.null(.httpd$port)
  if (!needs_start && (!missing(ports) || !missing(host))) {
    if (!identical(as.integer(ports), .httpd$port) ||
        !identical(host, .httpd$host)) {
      httpd_stop()
      needs_start = TRUE
    }
  }
  if (needs_start) httpd_start(host, ports)

  wd = getwd()
  .httpd$apps[[name]] = function(path, ...) {
    if (path == '') path = '.'
    in_dir(wd, handler(path, ...))
  }

  url_host = if (identical(.httpd$host, '0.0.0.0')) '127.0.0.1' else .httpd$host
  url = sprintf('http://%s:%d/%s/', url_host, .httpd$port, name)
  if (isTRUE(open)) open = getOption('viewer', browseURL)
  if (is.function(open)) open(url)

  # In non-interactive sessions, block until interrupted — but only when open
  # was not explicitly set to FALSE (which signals "more apps to come").
  if (!interactive() && !identical(open, FALSE)) {
    on.exit(stop_app(), add = TRUE)
    message('Serving at ', url, ' (press Ctrl+C to stop)')
    tryCatch(
      .Call(C_httpd_serve, names(.httpd$apps), unname(.httpd$apps)),
      interrupt = function(e) invisible(NULL)
    )
  }

  invisible(url)
}

#' @param name For `stop_app()`, a character vector of app names to stop;
#'   defaults to all running apps.
#' @rdname new_app
#' @export
stop_app = function(name = names(.httpd$apps)) {
  for (n in name) .httpd$apps[[n]] = NULL
  if (length(.httpd$apps) == 0L && !is.null(.httpd$port)) httpd_stop()
}

# internal state for the built-in HTTP server
.httpd = new.env(parent = emptyenv())
.httpd$apps = list()
.httpd$port = NULL
.httpd$host = NULL
#' Get data from a REST API
#'
#' Read data from a REST API and optionally with an authorization token in the
#' request header. The function `rest_api_raw()` returns the raw text of
#' the response, and `rest_api()` will parse the response with
#' `jsonlite::fromJSON()` (assuming that the response is in the JSON
#' format).
#'
#' These functions are simple wrappers based on [url()] and
#' [read_utf8()]. Specifically, the `headers` argument is
#' passed to `url()`, and `read_utf8()` will send a \samp{GET} request
#' to the API server. This means these functions only support the \samp{GET}
#' method. If you need to use other HTTP methods (such as \samp{POST}), you have
#' to use other packages such as \pkg{curl} and \pkg{httr}.
#' @param ... Arguments to be passed to `rest_api_raw()`.
#' @return A character vector (the raw JSON response) or an R object parsed from
#'   the JSON text.
#' @export
#' @examplesIf interactive()
#' # a normal GET request
#' xfun::rest_api('https://mockhttp.org', '/get')
#' xfun::rest_api_raw('https://mockhttp.org', '/get')
#'
#' # send the request with an auth header
#' xfun::rest_api('https://mockhttp.org', '/headers', 'OPEN SESAME!')
#'
#' # with query parameters
#' xfun::rest_api('https://mockhttp.org', '/response-headers', params = list(foo = 'bar'))
#'
#' # get the rate limit info from GitHub
#' xfun::github_api('/rate_limit')
rest_api = function(...) {
  res = rest_api_raw(...)
  jsonlite::fromJSON(res, simplifyVector = FALSE)
}

#' @param root The API root URL.
#' @param endpoint The API endpoint.
#' @param token A named character string (e.g., `c(token = "xxxx")`), which
#'   will be used to create an authorization header of the form
#'   \samp{Authorization: NAME TOKEN} for the API call, where \samp{NAME} is the
#'   name of the string and \samp{TOKEN} is the string. If the string does not
#'   have a name, \samp{Basic} will be used as the default name.
#' @param params A list of query parameters to be sent with the API call.
#' @param headers A named character vector of HTTP headers, e.g., `c(Accept
#'   = "application/vnd.github.v3+json")`.
#' @rdname rest_api
#' @export
rest_api_raw = function(root, endpoint, token = '', params = list(), headers = NULL)  {
  if (is.null(names(token))) names(token) = 'Basic'
  endpoint = sub('^/?', '/', endpoint)  # make sure it has a leading /
  url2 = if ('headers' %in% names(formals(url))) url else stop2(
    "The url() function does not support the 'headers' argument. Please upgrade R (>= 3.6.0)."
  )
  con = url2(
    paste0(root, endpoint, query_params(.list = params)), encoding = 'UTF-8',
    headers = c(
      headers, if (token != '') c(Authorization = sprintf('%s %s', names(token), token))
    )
  )
  on.exit(close(con), add = TRUE)
  raw_string(suppressWarnings(read_utf8(con)))
}
