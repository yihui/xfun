#' Create or stop a local web application
#'
#' `new_app()` has two modes:
#'
#' - `name = ''`: start an app behind a lightweight proxy at
#'   `http://host:PORT/`, forwarding to R's internal httpd path
#'   `/custom/xfun:PORT/`.
#' - `name != ''`: register a legacy app directly on R's internal httpd at
#'   `http://127.0.0.1:BACKEND/custom/name/` (compatible with older versions).
#'
#' `stop_app()` deregisters one or more running apps.
#' @param name App name (a character string). Use `name = ''` to serve the app
#'   at the root URL `http://host:PORT/` via the proxy. If `name` is non-empty,
#'   the app is served directly by R's internal httpd at `/custom/name/`. Each
#'   name must be unique; calling `new_app()` with an existing name replaces
#'   that app. For `stop_app()`, a character vector of app names to stop;
#'   defaults to all running apps.
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it. In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking.
#' @param host Bind address for the proxy (`"127.0.0.1"` or `"0.0.0.0"`). Only
#'   used when `name = ''`.
#' @param port Candidate proxy ports when `name = ''`. The first available port
#'   is selected (falling back to [random_port()] if needed). Ignored when
#'   `name != ''`.
#' @return `new_app()` returns the app URL invisibly. `stop_app()` returns
#'   nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', port = NULL
) {
  backend = .httpd_port()

  # Replace any existing app with the same name.
  if (name %in% names(.proxy$apps)) stop_app(name)

  if (identical(name, '')) {
    port = .find_proxy_port(port, names(.proxy$help))
    slot = proxy_start(as.integer(port), as.integer(backend), host = host)
    if (slot < 0L) stop2("Failed to start proxy on port ", port, ".")
    key = paste0('xfun:', port)
    assign(key, .make_app_handler(paste0('/custom/xfun:', port), handler, getwd()), envir = .httpd_env())
    .proxy$apps[[name]] = list(type = 'proxy', slot = slot, key = key, port = port)
    url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
    url = sprintf('http://%s:%d/', url_host, port)
  } else {
    key = as.character(name)
    assign(key, .make_app_handler(paste0('/custom/', name), handler, getwd()), envir = .httpd_env())
    .proxy$apps[[name]] = list(type = 'httpd', key = key)
    url = sprintf('http://127.0.0.1:%d/custom/%s/', backend, name)
  }

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
  if (!missing(name)) name = intersect(name, names(.proxy$apps))
  for (n in name) {
    idx = match(n, names(.proxy$apps))
    app = .proxy$apps[[idx]]
    rm_vars(app$key, .httpd_env())
    .proxy$apps[[idx]] = NULL
    if (identical(app$type, 'proxy')) proxy_stop(app$slot)
  }
}

# start an internal passthrough proxy for R's help pages.
help_proxy = function(port = NULL, host = '0.0.0.0') {
  p1 = .httpd_port()
  p2 = .find_proxy_port(port)
  old = .proxy$help[[as.character(p2)]]
  if (!is.null(old)) proxy_stop(old)
  slot = proxy_start(as.integer(p2), as.integer(p1), TRUE, host)
  if (slot < 0L) stop2("Failed to start help proxy on port ", p2, ".")
  .proxy$help[[as.character(p2)]] = slot
  u = sprintf('http://127.0.0.1:%d/doc/html/index.html', p2)
  message('Proxy started at ', u)
  invisible(u)
}

#' Find a random available TCP port
#'
#' Find an available TCP port, starting with `port`, then sampling from
#' 3000--8000 (excluding ports known to be blocked by Chrome).
#' Port availability is checked on both `127.0.0.1` and `0.0.0.0`.
#' @param port Default port to try first.
#' @param n Number of additional random ports to try.
#' @param exclude Integer vector of ports to exclude from the search.
#' @param error Whether to signal an error (default) or return `NULL` when no
#'   port is found.
#' @return An integer port number. When `error = TRUE` (default), signals an
#'   error if no port is found; when `error = FALSE`, returns `NULL`.
#' @export
random_port = function(port = 4321L, n = 20L, exclude = NULL, error = TRUE) {
  # exclude ports considered unsafe by Chrome http://superuser.com/a/188070
  unsafe = c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)
  ports  = sample(setdiff(3000:8000, unsafe), n)
  ports  = setdiff(c(port, ports), exclude)
  for (p in ports) if (.port_available(p)) return(p)
  if (error) stop2("Cannot find an available TCP port")
}

# ---- internal helpers -------------------------------------------------------

# Start R's internal httpd; return its port (or -1 on failure).
.httpd_port = function() {
  if (is.null(getOption('help.ports'))) {
    options(help.ports = 4321 + 1:30); on.exit(options(help.ports = NULL))
  }
  p = suppressMessages(tools::startDynamicHelp(NA))
  if (p <= 0L) stop2("Failed to start R's internal httpd.")
  p
}

# Environment where R's httpd looks up /custom/* handlers.
.httpd_env = function() getFromNamespace('.httpd.handlers.env', 'tools')

# Internal proxy state.
.proxy = new.env(parent = emptyenv())
.proxy$apps = list()  # app name → list(type, key, slot?)
.proxy$help = list()  # help proxy port → slot index

# Find an available proxy port without starting the proxy.
.proxy_app_ports = function() {
  vapply(Filter(function(x) identical(x$type, 'proxy'), .proxy$apps), `[[`, integer(1), 'port')
}

.find_proxy_port = function(candidates = NULL, exclude = integer()) {
  if (is.null(candidates)) candidates = 4321 + 1:30
  exclude = unique(c(as.integer(exclude), .proxy_app_ports(), as.integer(names(.proxy$help))))
  candidates = setdiff(candidates, exclude)
  tried = c(candidates, random_port(error = FALSE, exclude = c(candidates, exclude)))
  for (p in tried) {
    if (!is.na(p) && .port_available(p)) return(as.integer(p))
  }
  stop2(
    "No available proxy port found (tried candidates ",
    paste(unique(candidates), collapse = ', '),
    " and random fallback ports)."
  )
}

# Start a proxy instance on `port` forwarding to `backend_port`.
# passthrough = TRUE: forward all request paths verbatim (use this to expose
#   the full R httpd server, e.g. on host = "0.0.0.0" for LAN access).
# passthrough = FALSE (default): rewrite /path to /custom/xfun:PORT/path.
# host: bind address; "0.0.0.0" to listen on all interfaces.
# Returns the slot index (>= 0) or -1 on failure.
proxy_start = function(port, backend_port, passthrough = FALSE, host = '127.0.0.1') {
  .Call(C_proxy_start, as.integer(port), as.integer(backend_port),
        isTRUE(passthrough), as.character(host))
}

# Stop the proxy instance identified by its slot index.
proxy_stop = function(slot) {
  .Call(C_proxy_stop, as.integer(slot))
}

# Coerce body/headers-like argument to a raw vector.
.as_raw = function(x) {
  if (is.null(x) || length(x) == 0L) raw(0) else {
    if (is.raw(x)) x else charToRaw(x[[1L]])
  }
}

# Check if a TCP port is available by attempting to bind a server socket.
# Uses a C-level function so it works on all R versions (serverSocket() is R >= 4.0).
.port_available = function(port) {
  isTRUE(.Call(C_port_available, as.integer(port)))
}

# Build an R httpd handler closure.
.make_app_handler = function(prefix, fn, wd) {
  function(path, query = NULL, body = NULL, headers = NULL) {
    real = if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
    real = sub('^/', '', real)
    if (real == '') real = '.'
    q = if (is.null(query)) .parse_xfun_query(headers) else query
    in_dir(wd, fn(real, q, .as_raw(body), .as_raw(headers)))
  }
}

.stop_help_proxy = function(port = names(.proxy$help)) {
  if (!missing(port)) port = intersect(as.character(port), names(.proxy$help))
  for (p in port) {
    proxy_stop(.proxy$help[[p]])
    .proxy$help[[p]] = NULL
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
  if (qs == '') return(character(0))
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
