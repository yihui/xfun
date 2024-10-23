#' Create a local web application
#'
#' An experimental function to create a local web application based on R's
#' internal `httpd` server (which is primarily for running R's dynamic help
#' system).
#' @param name The app name (a character string, and each app should have a
#'   unique name).
#' @param handler A function that takes the HTTP request information (the first
#'   argument is the requested path) and returns a response.
#' @param open Whether to open the app, or a function to open the app URL.
#' @param ports A vector of ports to try for starting the server.
#' @return The app URL of the form `http://127.0.0.1:port/custom/name/`.
#' @note This function is not based on base R's public API, and is possible to
#'   break in the future, which is also why the documentation here is terse.
#'   Please avoid creating public-facing web apps with it. You may consider
#'   packages like \pkg{httpuv} and \pkg{Rserve} for production web apps.
#' @export
new_app = function(name, handler, open = interactive(), ports = 4321 + 1:10) {
  if (is.null(getOption('help.ports'))) {
    options(help.ports = ports); on.exit(options(help.ports = NULL))
  }
  port = suppressMessages(tools::startDynamicHelp(NA))
  url  = sprintf('http://127.0.0.1:%d/custom/%s/', port, name)
  wd = getwd()  # always run handler under the original working directory
  h = function(path, ...) {
    path = sub(paste0('^/custom/', name, '/'), '', path)
    if (path == '') path = '.'
    in_dir(wd, handler(path, ...))
  }
  # to anyone who sees the dirty assign() here, please close your eyes and walk
  # away as quickly as possible; thanks!
  assign(name, h, envir = app_env())
  if (isTRUE(open)) open = getOption('viewer', browseURL)
  if (is.function(open)) open(url)
  invisible(url)
}

# remove apps registered in the app environment
stop_app = function(name = ls(app_env(), all.names = TRUE)) {
  rm(list = name, envir = app_env())
}

app_env = function() getFromNamespace('.httpd.handlers.env', 'tools')

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
#' xfun::rest_api('https://httpbin.org', '/get')
#' xfun::rest_api_raw('https://httpbin.org', '/get')
#'
#' # send the request with an auth header
#' xfun::rest_api('https://httpbin.org', '/headers', 'OPEN SESAME!')
#'
#' # with query parameters
#' xfun::rest_api('https://httpbin.org', '/response-headers', params = list(foo = 'bar'))
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
