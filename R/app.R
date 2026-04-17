#' Create or stop a local web application
#'
#' `new_app()` starts a dedicated local HTTP proxy for an app, registers a
#' handler, and optionally opens the URL in a browser. Each app gets its own
#' proxy port so there is no ambiguity in routing. `stop_app()` deregisters
#' one or more apps and stops their proxies.
#'
#' Requests are forwarded to R's internal httpd
#' (`http://127.0.0.1:PORT/custom/xfun:name/`).
#' @param name App name (a character string). Use `name = ''` for a nameless
#'   root app. Each name must be unique; calling `new_app()` with an existing
#'   name replaces that app. For `stop_app()`, a character vector of app names
#'   to stop; defaults to all running apps.
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it. In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking.
#' @param host Bind address for the proxy (`"127.0.0.1"` or `"0.0.0.0"`).
#' @param ports Candidate proxy ports; the first available port not already in
#'   use by another xfun app is chosen. Falls back to [random_port()] if all
#'   candidates are taken. See [random_port()] to pick a port programmatically.
#' @return `new_app()` returns the app URL invisibly. `stop_app()` returns
#'   nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', ports = 4321 + 1:30
) {
  backend = .httpd_port()
  if (backend <= 0L) stop2("Failed to start R's internal httpd.")

  # Replace any existing app with the same name.
  if (!is.null(.proxy$apps[[name]])) stop_app(name)

  # Pick a port: skip ports already used by running apps, fallback to random_port().
  used = unlist(lapply(.proxy$apps, function(a) a[['port']]))
  candidates = setdiff(as.integer(ports), used)
  port = -1L
  for (p in candidates) if (.port_available(p)) { port = p; break }
  if (port < 0L)
    port = tryCatch(random_port(exclude = used), error = function(e) -1L)
  if (port < 0L) stop2("No available port found.")

  # Start a dedicated proxy for this app; prefix encodes the R httpd handler key.
  prefix = paste0('/custom/xfun:', name)
  slot = proxy_start(as.integer(port), as.integer(backend), prefix)
  if (slot < 0L) stop2("Failed to start proxy on port ", port, ".")

  # Register per-app R handler under key 'xfun:name' (or 'xfun:' for name = '').
  assign(paste0('xfun:', name), .make_app_handler(name, handler, getwd()),
         envir = .httpd_env())

  # Record app state.
  .proxy$apps[[name]] = list(port = port, slot = slot)

  url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
  url = sprintf('http://%s:%d/', url_host, port)
  if (isTRUE(open)) open = getOption('viewer', browseURL)
  if (is.function(open)) open(url)

  if (!interactive() && !identical(open, FALSE)) {
    on.exit(stop_app(name), add = TRUE)
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
  for (n in name) {
    app = .proxy$apps[[n]]
    if (!is.null(app)) {
      proxy_stop(app$slot)
      e = .httpd_env()
      key = paste0('xfun:', n)
      if (exists(key, envir = e, inherits = FALSE)) rm(list = key, envir = e)
      .proxy$apps[[n]] = NULL
    }
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

# Internal proxy state: per-app list of list(port, slot).
.proxy = new.env(parent = emptyenv())
.proxy$apps = list()

# Start a proxy instance on `port` forwarding to `backend_port` with `prefix`.
# Returns the slot index (>= 0) or -1 on failure.
proxy_start = function(port, backend_port, prefix) {
  .Call(C_proxy_start, as.integer(port), as.integer(backend_port), as.character(prefix))
}

# Stop the proxy instance identified by its slot index.
proxy_stop = function(slot) {
  .Call(C_proxy_stop, as.integer(slot))
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

# Build a per-app R handler closure registered in R's httpd handler environment.
# The proxy rewrites /path → /custom/xfun:name/path so R routes here.
# The handler strips the /custom/xfun:name prefix and calls the user function.
.make_app_handler = function(name, fn, wd) {
  prefix = paste0('/custom/xfun:', name)
  function(path, query = NULL, body = NULL, headers = NULL) {
    real = if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
    real = sub('^/', '', real)
    if (!nzchar(real)) real = '.'
    q    = .parse_xfun_query(headers)
    post = .as_raw(body)
    hdrs = .as_raw(headers)
    in_dir(wd, fn(real, q, post, hdrs))
  }
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
