library(testit)

# Generate the exact response body for given request parameters.
# Used in both the test handler (to produce the response) and the assertions
# (to compute the expected value), so %==% catches any proxy/routing mismatch.
make_body = function(path, foo = '', post = '', method = '', x_custom = 'MISSING') {
  paste0(
    'path=', path, '\n',
    'foo=', foo, '\n',
    'post=', post, '\n',
    'method=', method, '\n',
    'x_custom=', x_custom
  )
}

# Extract the HTTP response body (strip status line + headers).
# Works on the raw bytes returned by http_request() (handles both \r\n\r\n and \n\n).
http_body = function(resp) {
  if (is.null(resp)) return(NULL)
  for (sep in c('\r\n\r\n', '\n\n')) {
    m = regexpr(sep, resp, fixed = TRUE)
    if (m[1L] > 0L) return(substring(resp, m[1L] + nchar(sep)))
  }
  resp
}

# Send a raw HTTP/1.0 request and return the full response as a string.
# Use non-blocking I/O plus Content-Length checks to avoid partial-response flakes.
http_request = function(host, port, method, path, body = NULL, extra_headers = '') {
  sock = tryCatch(
    socketConnection(host, port = port, open = 'r+b', blocking = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(sock)) return(NULL)
  on.exit(try(close(sock), silent = TRUE), add = TRUE)
  clen = if (is.null(body)) 0L else nchar(body, type = 'bytes')
  req = paste0(
    method, ' ', path, ' HTTP/1.0\r\n',
    'Host: ', host, '\r\n',
    if (clen > 0) paste0('Content-Length: ', clen, '\r\n') else '',
    extra_headers,
    '\r\n',
    if (!is.null(body)) body else ''
  )
  writeBin(req, sock)
  buf = raw(0L)
  # 120 * 0.1s = up to 12 seconds for slow CI environments.
  for (i in seq_len(120L)) {
    Sys.sleep(0.1)
    ready = tryCatch(socketSelect(list(sock), write = FALSE, timeout = 0), error = function(e) FALSE)
    if (!isTRUE(ready[1L])) next
    chunk = tryCatch(readBin(sock, raw(), n = 65536L), error = function(e) NULL)
    if (is.null(chunk)) next
    if (length(chunk) == 0L) break
    buf = c(buf, chunk)
    s = rawToChar(buf)
    sep = regexpr('\r\n\r\n|\n\n', s, perl = TRUE)
    if (sep[1L] < 0L) next
    body_start = sep[1L] + attr(sep, 'match.length')
    cl = regmatches(s, regexpr('(?i)Content-Length: *(\\d+)', s, perl = TRUE))
    if (length(cl) == 0L || !nzchar(cl)) next
    expected = as.integer(sub('(?i).*Content-Length: *', '', cl, perl = TRUE))
    if (nchar(s) - body_start + 1L >= expected) break
  }
  if (length(buf) == 0L) NULL else rawToChar(buf)
}

# Retry a request until a complete response with a non-empty body is received.
http_request_full = function(host, port, method, path, body = NULL, extra_headers = '', tries = 30L) {
  resp = NULL
  for (i in seq_len(tries)) {
    resp = http_request(host, port, method, path, body, extra_headers)
    has_sep = !is.null(resp) && grepl('\r\n\r\n|\n\n', resp, perl = TRUE)
    b = http_body(resp)
    if (has_sep && !is.null(b) && nzchar(b)) break
    Sys.sleep(0.1)
  }
  resp
}

if (!is.null(port <- random_port(error = FALSE))) {

  # Run the proxy-backed app in a separate R process so requests are sent from
  # this R session to reduce random timing issues in CI.
  pid_file = tempfile(fileext = '.rds')
  saveRDS(NULL, pid_file)
  Rscript_call(function(make_body, port, pid_file) {
    saveRDS(Sys.getpid(), pid_file)
    on.exit(unlink(pid_file), add = TRUE)
    xfun::new_app(
      '',
      function(path, query, post, headers) {
        h          = if (length(headers) > 0L) rawToChar(headers) else ''
        foo        = if ('foo' %in% names(query)) query[['foo']] else ''
        post_str   = if (length(post) > 0L) rawToChar(post) else ''
        method_str = {
          m = regmatches(h, regexpr('Request-Method: [^\r\n]+', h))
          if (length(m) > 0L) sub('Request-Method: ', '', m) else ''
        }
        x_custom_str = {
          m = regmatches(h, regexpr('X-Custom: [^\r\n]+', h))
          if (length(m) > 0L) sub('X-Custom: ', '', m) else 'MISSING'
        }
        list(payload = make_body(path, foo, post_str, method_str, x_custom_str),
             'content-type' = 'text/plain')
      },
      open = NA,
      port = port
    )
  }, list(make_body = make_body, port = port, pid_file = pid_file), wait = FALSE)
  pid = NULL
  for (i in seq_len(100L)) {
    Sys.sleep(0.1)
    if (!file.exists(pid_file)) break
    pid = tryCatch(readRDS(pid_file), error = function(e) NULL)
    if (length(pid) == 1L) break
  }
  on.exit(if (length(pid) == 1L) try(proc_kill(pid), silent = TRUE), add = TRUE)

  # Wait for the background app to become reachable.
  for (i in seq_len(50L)) {
    resp = http_request_full('127.0.0.1', port, 'GET', '/hello')
    if (!is.null(resp) && grepl('200 OK', resp, fixed = TRUE)) break
    Sys.sleep(0.1)
  }

  assert('proxy root app responds to a GET request', {
    (!is.null(resp))
    (grepl('200 OK', resp, fixed = TRUE))
    (http_body(resp) %==% make_body('hello', method = 'GET'))
  })

  assert('proxy query parameters are URL-decoded', {
    resp = http_request_full('127.0.0.1', port, 'GET', '/page?foo=bar%20baz')
    (!is.null(resp))
    (http_body(resp) %==% make_body('page', foo = 'bar baz', method = 'GET'))
  })

  assert('proxy POST body is forwarded correctly', {
    resp = http_request_full('127.0.0.1', port, 'POST', '/submit', body = 'hello=world')
    (!is.null(resp))
    (http_body(resp) %==% make_body('submit', post = 'hello=world', method = 'POST'))
  })

  assert('proxy request headers reach the handler', {
    resp = http_request_full('127.0.0.1', port, 'GET', '/hdr',
                             extra_headers = 'X-Custom: test-value\r\n')
    (!is.null(resp))
    (http_body(resp) %==% make_body('hdr', method = 'GET', x_custom = 'test-value'))
  })

  url = new_app(
    'test',
    function(path, query, post, headers) {
      h          = if (length(headers) > 0L) rawToChar(headers) else ''
      foo        = if ('foo' %in% names(query)) query[['foo']] else ''
      post_str   = if (length(post) > 0L) rawToChar(post) else ''
      method_str = {
        m = regmatches(h, regexpr('Request-Method: [^\r\n]+', h))
        if (length(m) > 0L) sub('Request-Method: ', '', m) else ''
      }
      list(payload = make_body(path, foo, post_str, method_str), 'content-type' = 'text/plain')
    },
    open = FALSE
  )
  on.exit(stop_app('test'), add = TRUE)

  assert('new_app(name != \"\") uses legacy /custom/name/ path', {
    m = regexec('^http://127\\.0\\.0\\.1:([0-9]+)/custom/test/$', url)
    cap = regmatches(url, m)[[1]]
    (length(cap) == 2L)
    (!is.null(get0('test', .httpd_env(), inherits = FALSE)))
  })

  assert('stop_app() deregisters legacy app handler', {
    stop_app('test')
    (is.null(.proxy$apps[['test']]))
    (is.null(get0('test', .httpd_env(), inherits = FALSE)))
  })
}
