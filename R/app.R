#' Create a local web application
#'
#' Create a local web application backed by a simple built-in HTTP server.
#' The server is started automatically on the first call to `new_app()` and
#' processes incoming requests via R's task-callback mechanism (see
#' [addTaskCallback()]), so it is intended for interactive use.
#' @param name The app name (a character string, and each app should have a
#'   unique name).
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
#'   it.
#' @param host The host/IP address to listen on.  Use `"127.0.0.1"` (default)
#'   to accept only local connections, or `"0.0.0.0"` to accept connections on
#'   all network interfaces.  This argument is only used when the server is
#'   first started; subsequent `new_app()` calls reuse the existing server.
#' @param ports A vector of candidate port numbers.  The first port that can be
#'   successfully bound is used.
#' @return The app URL of the form `http://host:port/name/` (invisibly).
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', ports = 4321 + 1:10
) {
  if (is.null(.httpd$port)) {
    port = .Call(httpd_start, as.integer(ports), as.character(host))
    if (port < 0L) stop2("Failed to start HTTP server on any of the specified ports.")
    .httpd$port = port
    .httpd$host = host
    .httpd$cb_id = addTaskCallback(function(...) {
      apps = .httpd$apps
      if (length(apps))
        .Call(httpd_poll, names(apps), unname(apps))
      TRUE
    }, name = 'xfun.httpd')
  }
  wd = getwd()
  .httpd$apps[[name]] = function(path, ...) {
    if (path == '') path = '.'
    in_dir(wd, handler(path, ...))
  }
  # use 127.0.0.1 in the URL when binding on all interfaces (0.0.0.0 is not
  # directly routable from a browser)
  url_host = if (identical(.httpd$host, '0.0.0.0')) '127.0.0.1' else .httpd$host
  url = sprintf('http://%s:%d/%s/', url_host, .httpd$port, name)
  if (isTRUE(open)) open = getOption('viewer', browseURL)
  if (is.function(open)) open(url)
  invisible(url)
}

# stop one or more apps (and shut down the server when all apps are removed)
stop_app = function(name = names(.httpd$apps)) {
  for (n in name) .httpd$apps[[n]] = NULL
  if (length(.httpd$apps) == 0L && !is.null(.httpd$port)) {
    .Call(httpd_stop)
    if (!is.null(.httpd$cb_id)) {
      removeTaskCallback(.httpd$cb_id)
      .httpd$cb_id = NULL
    }
    .httpd$port = NULL
    .httpd$host = NULL
  }
}

# internal state for the built-in HTTP server
.httpd = new.env(parent = emptyenv())
.httpd$apps  = list()
.httpd$port  = NULL
.httpd$host  = NULL
.httpd$cb_id = NULL

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
