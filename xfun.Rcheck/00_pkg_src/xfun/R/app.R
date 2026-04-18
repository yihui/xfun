#' Create or stop a local web application
#'
#' `new_app()` starts a local HTTP proxy for an app, registers a handler, and
#' optionally opens the URL in a browser. By default all apps share one proxy
#' port; pass explicit `ports` to use a different port. `stop_app()` deregisters
#' one or more apps (and shuts the proxy down when no apps remain on that port).
#'
#' Requests are forwarded to R's internal httpd at
#' `http://127.0.0.1:BACKEND/custom/xfun:name:PORT/`.
#' @param name App name (a character string). Use `name = ''` for a nameless
#'   root app served at `http://host:PORT/`; a non-empty name is served at
#'   `http://host:PORT/~name/`. Each name must be unique; calling `new_app()`
#'   with an existing name replaces that app. For `stop_app()`, a character
#'   vector of app names to stop; defaults to all running apps.
#' @param handler A function with signature `function(path, query, post,
#'   headers)` that handles HTTP requests and returns a response list.
#' @param open Whether to open the app URL in a browser, or a function to open
#'   it. In non-interactive sessions this also controls whether the call
#'   blocks: passing `open = FALSE` explicitly suppresses both browser-opening
#'   and blocking.
#' @param host Bind address for the proxy (`"127.0.0.1"` or `"0.0.0.0"`).
#' @param port Candidate proxy ports. When omitted, all apps share the same
#'   port (the first available one from `4321 + 1:30`). Pass an explicit value
#'   to select a different port for this app; falls back to [random_port()] if
#'   all candidates are in use by other processes. See [random_port()] to pick
#'   a port programmatically.
#' @return `new_app()` returns the app URL invisibly. `stop_app()` returns
#'   nothing.
#' @export
new_app = function(
  name, handler, open = interactive(), host = '127.0.0.1', port = NULL
) {
  backend = .httpd_port()

  # Replace any existing app with the same name.
  if (name %in% names(.proxy$apps)) stop_app(name)

  # Determine which proxy port/slot to use.
  ps = .find_proxy(port, backend, host)
  port = ps$port
  slot = ps$slot

  # Register per-app handler; key encodes both name and port for uniqueness.
  key = paste0('xfun:', name, ':', port)
  assign(key, .make_app_handler(name, port, handler, getwd()), envir = .httpd_env())

  # Record app state.
  .proxy$apps[[name]] = list(port = port, slot = slot)

  url_host = if (identical(host, '0.0.0.0')) '127.0.0.1' else host
  url = if (name == '') '' else sprintf('~%s/', name)
  url = sprintf('http://%s:%d/%s', url_host, port, url)
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
    key = paste0('xfun:', n, ':', app$port)
    rm_vars(key, .httpd_env())
    .proxy$apps[[idx]] = NULL
    # Stop the proxy only when no other apps remain on that port.
    still_on_port = Filter(function(a) a$port == app$port, .proxy$apps)
    if (length(still_on_port) == 0L) {
      proxy_stop(app$slot)
      .proxy$port_to_slot[[as.character(app$port)]] = NULL
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
  p = suppressMessages(tools::startDynamicHelp(NA))
  if (p <= 0L) stop2("Failed to start R's internal httpd.")
  p
}

# Environment where R's httpd looks up /custom/* handlers.
.httpd_env = function() getFromNamespace('.httpd.handlers.env', 'tools')

# Internal proxy state.
.proxy = new.env(parent = emptyenv())
.proxy$apps = list()          # name → list(port, slot)
.proxy$port_to_slot = list()  # as.character(port) → slot index

# Find or start a proxy for the given port candidates.
# When use_default is TRUE and a proxy already exists, reuse it (shared port).
.find_proxy = function(candidates, backend, host = '127.0.0.1') {
  if (use_default <- is.null(candidates)) candidates = 4321 + 1:30
  # Reuse any running proxy when using the default port selection.
  if (use_default && length(.proxy$port_to_slot)) {
    p = as.integer(names(.proxy$port_to_slot)[1L])
    return(list(port = p, slot = .proxy$port_to_slot[[1L]]))
  }
  # Reuse a running proxy if one of the explicit candidates is already active.
  for (p in candidates) {
    s = .proxy$port_to_slot[[as.character(p)]]
    if (!is.null(s)) return(list(port = p, slot = s))
  }
  # Try each candidate then fall back to a random port.
  for (p in c(candidates, random_port(error = FALSE))) {
    if (.port_available(p)) {
      slot = proxy_start(as.integer(p), as.integer(backend), host = host)
      if (slot >= 0L) {
        .proxy$port_to_slot[[as.character(p)]] = slot
        return(list(port = p, slot = slot))
      }
    }
  }
  stop2("No available port found.")
}

# Start a proxy instance on `port` forwarding to `backend_port`.
# passthrough = TRUE: forward all request paths verbatim (use this to expose
#   the full R httpd server, e.g. on host = "0.0.0.0" for LAN access).
# passthrough = FALSE (default): use ~name URL rewriting for new_app() apps.
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

# Build a per-app R handler closure registered in R's httpd handler environment.
# The proxy rewrites /~name/rest → /custom/xfun:name:PORT/rest so R routes here.
# The handler strips the /custom/xfun:name:PORT prefix and calls the user function.
.make_app_handler = function(name, port, fn, wd) {
  prefix = paste0('/custom/xfun:', name, ':', port)
  function(path, query = NULL, body = NULL, headers = NULL) {
    real = if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
    real = sub('^/', '', real)
    if (real == '') real = '.'
    in_dir(wd, fn(real, .parse_xfun_query(headers), .as_raw(body), .as_raw(headers)))
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
