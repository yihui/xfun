#' Create or stop a local web application
#'
#' `new_app()` registers a handler function for a URL path on the local HTTP
#' server and optionally opens the URL in a browser.  `stop_app()` deregisters
#' one or more apps.
#'
#' The server is R's built-in httpd (the same one that powers dynamic help).
#' A lightweight proxy started by xfun provides clean URLs of the form
#' `http://127.0.0.1:PORT/name/` instead of R's native
#' `http://127.0.0.1:PORT/custom/name/`.  The proxy runs in a background
#' thread and works immediately on all platforms without the user needing to
#' press Enter (no R event-loop involvement is required for the proxy itself).
#' @param name App name (a character string; each app needs a unique name).
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it.  In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking.
#' @param host Bind address for the proxy (`"127.0.0.1"` or `"0.0.0.0"`).
#' @param ports Candidate proxy ports; the first available port is used.
#' @return `new_app()` returns the app URL invisibly.  `stop_app()` returns
#'   nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', ports = 4321 + 1:10
) {
  # Start R's internal httpd (returns its port).
  backend = .httpd_port()
  if (backend <= 0L) stop2("Failed to start R's internal httpd.")

  # Register our wrapper in R's httpd environment.
  wd = getwd()
  h = function(path, query = NULL, body = NULL, headers = NULL) {
    path = sub(paste0('^/custom/', name, '/'), '', path)
    if (path == '') path = '.'
    # Reconstruct named, URL-decoded query params from the X-Xfun-Query
    # header added by our proxy (R's httpd discards parameter names).
    q = .parse_xfun_query(headers)
    # Normalise body/post and headers to raw vectors.
    post = if (is.null(body)    || length(body)    == 0L) raw(0) else
             if (is.raw(body))    body    else charToRaw(body)
    hdrs = if (is.null(headers) || length(headers) == 0L) raw(0) else
             if (is.raw(headers)) headers else charToRaw(headers[1L])
    in_dir(wd, handler(path, q, post, hdrs))
  }
  assign(name, h, envir = .httpd_env())

  # Start proxy if not running, or restart when user provides an explicit
  # different set of ports.
  if (is.null(.proxy$port) || !missing(ports)) {
    p = proxy_start(as.integer(ports), backend)
    if (p < 0L) stop2("Failed to start proxy on any of the given ports.")
    .proxy$port = p
  }
  .proxy$apps[[name]] = TRUE

  url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
  url = sprintf('http://%s:%d/%s/', url_host, .proxy$port, name)
  if (isTRUE(open)) open = getOption('viewer', browseURL)
  if (is.function(open)) open(url)

  if (!interactive() && !identical(open, FALSE)) {
    on.exit(stop_app(), add = TRUE)
    message('Serving at ', url, ' (press Ctrl+C to stop)')
    tryCatch(
      while (TRUE) Sys.sleep(1),
      interrupt = function(e) invisible(NULL)
    )
  }
  invisible(url)
}

#' @rdname new_app
#' @export
stop_app = function(name = names(.proxy$apps)) {
  e = .httpd_env()
  for (n in name) {
    if (exists(n, envir = e, inherits = FALSE)) rm(list = n, envir = e)
    .proxy$apps[[n]] = NULL
  }
  if (length(.proxy$apps) == 0L && !is.null(.proxy$port)) proxy_stop()
}

# ---- internal helpers -------------------------------------------------------

# Start R's internal httpd; return its port (or -1 on failure).
.httpd_port = function() suppressMessages(tools::startDynamicHelp(NA))

# Environment where R's httpd looks up /custom/* handlers.
.httpd_env = function() getFromNamespace('.httpd.handlers.env', 'tools')

# Internal proxy state.
.proxy = new.env(parent = emptyenv())
.proxy$port = NULL   # proxy listen port
.proxy$apps = list() # names of registered apps

# Start the proxy on the first available port from `ports`; return bound port or -1.
proxy_start = function(ports, backend_port) {
  .Call(C_proxy_start, as.integer(ports), as.integer(backend_port))
}

# Stop the proxy thread and close the listen socket.
proxy_stop = function() {
  .Call(C_proxy_stop)
  .proxy$port = NULL
}

# Parse the X-Xfun-Query header (added by the proxy) into a named, URL-decoded
# character vector.  Falls back to character(0) when absent.
.parse_xfun_query = function(headers) {
  if (is.null(headers) || length(headers) == 0L) return(character(0))
  hs = if (is.raw(headers)) rawToChar(headers) else paste(headers, collapse = '\n')
  # Find the X-Xfun-Query line.
  qs = ''
  for (line in strsplit(hs, '\n', fixed = TRUE)[[1L]]) {
    line = sub('\r$', '', line)
    if (startsWith(line, 'X-Xfun-Query: ')) {
      qs = substring(line, nchar('X-Xfun-Query: ') + 1L)
      break
    }
  }
  if (!nzchar(qs)) return(character(0))
  pairs = Filter(nzchar, strsplit(qs, '&', fixed = TRUE)[[1L]])
  if (!length(pairs)) return(character(0))
  keys = vals = character(length(pairs))
  for (i in seq_along(pairs)) {
    kv = strsplit(pairs[[i]], '=', fixed = TRUE)[[1L]]
    keys[[i]] = utils::URLdecode(kv[[1L]])
    vals[[i]] = if (length(kv) >= 2L)
      utils::URLdecode(paste(kv[-1L], collapse = '=')) else ''
  }
  setNames(vals, keys)
}

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
