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
# Works on the string returned by http_request() (readLines + paste).
http_body = function(resp) {
  if (is.null(resp)) return(NULL)
  idx = regexpr('\n\n', resp, fixed = TRUE)
  if (idx[1L] > 0L) substring(resp, idx[1L] + 2L) else resp
}

# Send a raw HTTP/1.0 request and return the full response as a string.
http_request = function(host, port, method, path, body = NULL, extra_headers = '') {
  sock = tryCatch(
    socketConnection(host, port = port, open = 'r+b', blocking = TRUE, timeout = 5),
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
  Sys.sleep(0.5)  # allow proxy + R httpd to process the request and send a full response
  tryCatch(paste(readLines(sock, warn = FALSE), collapse = '\n'),
           error = function(e) NULL)
}

port = tryCatch(random_port(), error = function(e) NULL)

if (!is.null(port)) {

  # Handler echoes request details via make_body() so the test can compare
  # the response body against make_body() called directly.
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
      x_custom_str = {
        m = regmatches(h, regexpr('X-Custom: [^\r\n]+', h))
        if (length(m) > 0L) sub('X-Custom: ', '', m) else 'MISSING'
      }
      list(payload = make_body(path, foo, post_str, method_str, x_custom_str),
           'content-type' = 'text/plain')
    },
    open = FALSE,
    ports = port
  )
  on.exit(stop_app('test'), add = TRUE)

  assert('new_app() returns the expected URL', {
    (url %==% sprintf('http://127.0.0.1:%d/~test/', port))
  })

  assert('proxy responds to a GET request', {
    resp = http_request('127.0.0.1', port, 'GET', '/~test/hello')
    (!is.null(resp))
    (grepl('200 OK', resp, fixed = TRUE))
    (http_body(resp) %==% make_body('hello', method = 'GET'))
  })

  assert('query parameters are URL-decoded', {
    resp = http_request('127.0.0.1', port, 'GET', '/~test/page?foo=bar%20baz')
    (!is.null(resp))
    (http_body(resp) %==% make_body('page', foo = 'bar baz', method = 'GET'))
  })

  assert('POST body is forwarded correctly', {
    resp = http_request('127.0.0.1', port, 'POST', '/~test/submit',
                        body = 'hello=world')
    (!is.null(resp))
    (http_body(resp) %==% make_body('submit', post = 'hello=world', method = 'POST'))
  })

  assert('request headers reach the handler', {
    resp = http_request('127.0.0.1', port, 'GET', '/~test/hdr',
                        extra_headers = 'X-Custom: test-value\r\n')
    (!is.null(resp))
    (http_body(resp) %==% make_body('hdr', method = 'GET', x_custom = 'test-value'))
  })

  assert('a nameless app shares the port and serves the root path', {
    new_app('', function(path, ...) {
      list(payload = paste0('nameless:', path), 'content-type' = 'text/plain')
    }, open = FALSE)  # no explicit ports → reuses 'test' port
    resp_named   = http_request('127.0.0.1', port, 'GET', '/~test/hi')
    resp_nameless = http_request('127.0.0.1', port, 'GET', '/hi')
    stop_app('')
    (!is.null(resp_named))
    (!is.null(resp_nameless))
    (http_body(resp_named)    %==% make_body('hi', method = 'GET'))
    (http_body(resp_nameless) %==% 'nameless:hi')
  })

  assert('stop_app() deregisters the app and stops the proxy', {
    stop_app('test')
    on.exit(NULL)  # cancel earlier on.exit so we don't double-stop
    (is.null(.proxy$apps[['test']]))
    (is.null(.proxy$port_to_slot[[as.character(port)]]))
    (is.null(tryCatch(
      socketConnection('127.0.0.1', port = port, open = 'r+b', timeout = 1),
      error = function(e) NULL
    )))
  })

}
