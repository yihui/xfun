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
proxy_start = function(port, backend_port, passthrough = FALSE, host = '127.0.0.1') {
  port         = as.integer(port)
  backend_port = as.integer(backend_port)
  passthrough  = isTRUE(passthrough)
  host         = as.character(host)[1]

  bg = Rscript_bg(.proxy_run, list(
    port = port, backend_port = backend_port, passthrough = passthrough, host = host
  ))
  if (!.proxy_wait_ready(port, host)) {
    if (isTRUE(bg$is_alive())) try(proc_kill(bg$pid), silent = TRUE)
    stop2("Failed to start proxy on port ", port, ".")
  }
  paste0('r:', bg$pid)
}

# Stop the proxy instance identified by its slot.
proxy_stop = function(slot) {
  if (is.character(slot) && length(slot) == 1L && startsWith(slot, 'r:')) {
    pid = sub('^r:', '', slot)
    if (grepl('^[0-9]+$', pid)) try(proc_kill(as.integer(pid)), silent = TRUE)
  }
  invisible(NULL)
}

# Coerce body/headers-like argument to a raw vector.
.as_raw = function(x) {
  if (is.null(x) || length(x) == 0L) raw(0) else {
    if (is.raw(x)) x else charToRaw(x[[1L]])
  }
}

# Check if a TCP port is available by attempting to bind a server socket.
# Requires serverSocket() / socketAccept() (R >= 4.0.0).
.port_available = function(port) {
  server_socket = base::serverSocket
  s = tryCatch(server_socket(as.integer(port)), error = function(e) NULL)
  if (is.null(s)) return(FALSE)
  close(s)
  TRUE
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

.proxy_connect_host = function(host) {
  host = as.character(host)[1]
  # server may bind all interfaces (0.0.0.0), but local readiness checks should
  # use loopback for a deterministic self-connection target.
  if (identical(host, '0.0.0.0')) '127.0.0.1' else host
}

# Poll until the proxy port is accepting connections (or give up after timeout).
.proxy_wait_ready = function(port, host, tries = 100L, delay = 0.1) {
  host = .proxy_connect_host(host)
  for (i in seq_len(tries)) {
    con = tryCatch(
      socketConnection(host = host, port = port, open = 'r+b', blocking = TRUE, timeout = 1),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      close(con)
      return(TRUE)
    }
    Sys.sleep(delay)
  }
  FALSE
}

# Main proxy loop running in a background R process.
.proxy_run = function(port, backend_port, passthrough = FALSE, host = '127.0.0.1') {
  server_socket = base::serverSocket
  socket_accept = base::socketAccept

  listener = server_socket(port)
  on.exit(close(listener), add = TRUE)

  repeat {
    con = tryCatch(
      socket_accept(listener, blocking = FALSE, open = 'r+b', timeout = 1),
      error = function(e) NULL
    )
    if (is.null(con)) next
    try(.proxy_forward(con, port, backend_port, passthrough), silent = TRUE)
    try(close(con), silent = TRUE)
  }
}

# Read a client request, forward it to backend httpd, and relay the response.
.proxy_forward = function(client, port, backend_port, passthrough = FALSE) {
  req = .proxy_read_request(client)
  if (is.null(req)) return()

  path  = req$path
  path2 = if (isTRUE(passthrough)) path else sprintf('/custom/xfun:%d%s', port, path)
  q_pos = regexpr('\\?', path)[1L]
  query = if (q_pos > 0L) substring(path, q_pos + 1L) else ''

  backend = tryCatch(
    socketConnection(host = '127.0.0.1', port = backend_port, open = 'r+b', blocking = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(backend)) return()
  on.exit(try(close(backend), silent = TRUE), add = TRUE)

  headers = req$headers
  if (length(headers)) {
    keep = !grepl('^(connection|keep-alive|proxy-connection):', headers, ignore.case = TRUE)
    headers = headers[keep]
  }
  if (nzchar(query)) headers = c(headers, paste0('X-Xfun-Query: ', query))
  txt = paste(c(sprintf('%s %s HTTP/1.0', req$method, path2), headers, '', ''), collapse = '\r\n')
  writeBin(charToRaw(txt), backend)
  if (length(req$body)) writeBin(req$body, backend)
  flush(backend)

  buf = raw(0); clen = NA_integer_; sep_end = NA_integer_
  for (i in seq_len(200L)) {
    Sys.sleep(0.05)
    ready = tryCatch(socketSelect(list(backend), write = FALSE, timeout = 0), error = function(e) FALSE)
    if (!isTRUE(ready[1L])) next
    x = tryCatch(readBin(backend, raw(), 65536L), error = function(e) raw(0))
    if (!length(x)) break
    buf = c(buf, x)
    if (is.na(sep_end)) {
      s = tryCatch(rawToChar(buf), error = function(e) '')
      m = regexpr('\r\n\r\n|\n\n', s, perl = TRUE)
      if (m[1L] > 0L) {
        sep_end = m[1L] + attr(m, 'match.length') - 1L
        cl_m = regexpr('(?i)content-length:[ \t]*([0-9]+)', s, perl = TRUE)
        if (cl_m[1L] > 0L)
          clen = as.integer(trimws(sub('(?i).*:', '', regmatches(s, cl_m), perl = TRUE)))
      }
    }
    if (!is.na(sep_end) && !is.na(clen) && (length(buf) - sep_end) >= clen) break
  }
  if (length(buf)) { writeBin(buf, client); flush(client) }
}

# Parse one HTTP request from a non-blocking socket connection.
# Uses socketSelect() polling so readBin() returns immediately with whatever
# bytes are available without blocking for the full buffer size.
.proxy_read_request = function(con) {
  buf = raw(0); head_end = 0L; sep_len = 0L
  for (i in seq_len(200L)) {  # up to 200 * 0.05 s = 10 s
    Sys.sleep(0.05)
    ready = tryCatch(socketSelect(list(con), write = FALSE, timeout = 0), error = function(e) FALSE)
    if (!isTRUE(ready[1L])) next
    x = tryCatch(readBin(con, raw(), 65536L), error = function(e) raw(0))
    if (!length(x)) return(NULL)
    buf = c(buf, x)
    s = tryCatch(rawToChar(buf), error = function(e) '')
    m = regexpr('\r\n\r\n|\n\n', s, perl = TRUE)
    if (m[1L] > 0L) { head_end = m[1L]; sep_len = attr(m, 'match.length'); break }
    if (length(buf) >= 65536L) return(NULL)
  }
  if (head_end == 0L) return(NULL)

  hs = substring(rawToChar(buf), 1L, head_end + sep_len - 1L)
  hs = sub('\r\n\r\n$|\n\n$', '', hs, perl = TRUE)
  lines = strsplit(hs, '\r\n|\n', perl = TRUE)[[1L]]
  if (!length(lines)) return(NULL)

  req = strsplit(lines[[1L]], ' ', fixed = TRUE)[[1L]]
  req = req[nzchar(req)]
  if (length(req) < 2L) return(NULL)
  method = req[[1L]]; path = req[[2L]]; headers = lines[-1L]

  clen = 0L
  if (length(headers)) {
    i = grep('^content-length:', headers, ignore.case = TRUE)
    if (length(i)) {
      v = trimws(sub('^[^:]*:', '', headers[[i[1L]]]))
      if (grepl('^[0-9]+$', v)) clen = as.integer(v)
    }
    if (is.na(clen) || clen < 0L) clen = 0L
  }
  body_start = head_end + sep_len
  body0 = if (body_start <= length(buf)) buf[body_start:length(buf)] else raw(0)
  body = if (length(body0) < clen)
    c(body0, tryCatch(readBin(con, raw(), clen - length(body0)), error = function(e) raw(0)))
  else body0[seq_len(clen)]
  list(method = method, path = path, headers = headers, body = body)
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
