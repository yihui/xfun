#' Create or stop a local web application
#'
#' `new_app()` registers a handler function for a URL path on the local HTTP
#' server and optionally opens the URL in a browser. `stop_app()` deregisters
#' one or more apps.
#'
#' The app URL is `http://127.0.0.1:PORT/name/`. Use `name = ''` to register a
#' catch-all handler at `http://127.0.0.1:PORT/`. R's internal httpd (the same
#' one that powers the help system) serves the requests.
#' @param name App name (a character string). Use `name = ''` to register a
#'   catch-all handler at the server root. Each non-empty name must be unique.
#'   For `stop_app()`, a character vector of app names to stop; defaults to all
#'   running apps.
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it. In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking.
#' @param host Bind address for the proxy (`"127.0.0.1"` or `"0.0.0.0"`).
#' @param ports Candidate proxy ports; the first available port is used. See
#'   [random_port()] to find a free port programmatically.
#' @return `new_app()` returns the app URL invisibly. `stop_app()` returns
#'   nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', ports = 4321 + 1:30
) {
  backend = .httpd_port()
  if (backend <= 0L) stop2("Failed to start R's internal httpd.")

  # Register the single xfun dispatcher in R's httpd environment (once).
  e = .httpd_env()
  if (!exists('xfun:', envir = e, inherits = FALSE))
    assign('xfun:', .xfun_dispatch, envir = e)

  # Store per-app handler and working directory.
  .proxy$apps[[name]] = list(fn = handler, wd = getwd())

  # Start proxy if not running, or restart when user provides explicit ports.
  if (is.null(.proxy$port) || !missing(ports)) {
    p = proxy_start(as.integer(ports), backend)
    if (p < 0L) stop2("Failed to start proxy on any of the given ports.")
    .proxy$port = p
  }

  url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
  url = sprintf('http://%s:%d/%s', url_host, .proxy$port,
                if (nzchar(name)) paste0(name, '/') else '')
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
  for (n in name) .proxy$apps[[n]] = NULL
  if (length(.proxy$apps) == 0L) {
    if (!is.null(.proxy$port)) proxy_stop()
    e = .httpd_env()
    if (exists('xfun:', envir = e, inherits = FALSE))
      rm(list = 'xfun:', envir = e)
  }
}

#' Find a random available TCP port
#'
#' Find an available TCP port, starting with `port`, then sampling from
#' 3000--8000 (excluding ports known to be blocked by Chrome).
#' @param port Default port to try first.
#' @param n Number of additional random ports to try.
#' @param exclude Integer vector of ports to exclude from the search.
#' @return An integer port number. Signals an error if no port is found.
#' @export
random_port = function(port = 4321L, n = 20L, exclude = NULL) {
  unsafe = c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)
  ports  = sample(setdiff(3000:8000, unsafe), n)
  ports  = setdiff(c(port, ports), exclude)
  for (p in ports) if (.port_available(p)) return(p)
  stop2("Cannot find an available TCP port")
}

# ---- internal helpers -------------------------------------------------------

# Start R's internal httpd; return its port (or -1 on failure).
.httpd_port = function() suppressMessages(tools::startDynamicHelp(NA))

# Environment where R's httpd looks up /custom/* handlers.
.httpd_env = function() getFromNamespace('.httpd.handlers.env', 'tools')

# Internal proxy state.
.proxy = new.env(parent = emptyenv())
.proxy$port = NULL   # proxy listen port
.proxy$apps = list() # name → list(fn, wd)

# Start the proxy on the first available port from `ports`; return bound port or -1.
proxy_start = function(ports, backend_port) {
  .Call(C_proxy_start, as.integer(ports), as.integer(backend_port))
}

# Stop the proxy thread and close the listen socket.
proxy_stop = function() {
  .Call(C_proxy_stop)
  .proxy$port = NULL
}

# Coerce body/headers-like argument to a raw vector.
.as_raw = function(x) {
  if (is.null(x) || length(x) == 0L) return(raw(0))
  if (is.raw(x)) return(x)
  charToRaw(x[[1L]])
}

# Check if a TCP port is available by attempting to bind a server socket.
.port_available = function(port) {
  s = tryCatch(serverSocket(port), error = function(e) NULL)
  if (is.null(s)) return(FALSE)
  close(s)
  TRUE
}

# Single R-level dispatcher for all xfun apps. Registered as 'xfun:' in
# R's httpd handler environment. Proxy rewrites /name/rest to
# /custom/xfun:/name/rest so R dispatches here for every xfun app request.
.xfun_dispatch = function(path, query = NULL, body = NULL, headers = NULL) {
  # Strip the /custom/xfun: prefix inserted by the proxy.
  real = sub('^/custom/xfun:', '', path)  # /name/rest  or  /rest
  q    = .parse_xfun_query(headers)
  post = .as_raw(body)
  hdrs = .as_raw(headers)

  # Try named apps first (non-empty names, longest first to avoid prefix conflicts).
  named = names(.proxy$apps)
  named = named[nzchar(named)]
  named = named[order(nchar(named), decreasing = TRUE)]
  for (n in named) {
    pfx = paste0('/', n)
    if (real == pfx || startsWith(real, paste0(pfx, '/'))) {
      sub_path = sub(paste0('^', pfx, '/?'), '', real)
      if (!nzchar(sub_path)) sub_path = '.'
      app = .proxy$apps[[n]]
      return(in_dir(app$wd, app$fn(sub_path, q, post, hdrs)))
    }
  }

  # Catch-all (name = '').
  if ('' %in% names(.proxy$apps)) {
    app      = .proxy$apps[['']]
    sub_path = sub('^/', '', real)
    if (!nzchar(sub_path)) sub_path = '.'
    return(in_dir(app$wd, app$fn(sub_path, q, post, hdrs)))
  }

  list(payload = 'Not found', 'content-type' = 'text/html', 'status code' = 404L)
}

# Parse the X-Xfun-Query header (added by the proxy) into a named, URL-decoded
# character vector. Falls back to character(0) when absent.
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
