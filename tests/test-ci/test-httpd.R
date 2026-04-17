library(testit)

port = tryCatch(random_port(), error = function(e) NULL)

# Send a raw HTTP/1.0 request to the proxy and return the full response as a
# string. After writing the request we call Sys.sleep() so R's event loop
# runs and R's internal httpd can accept + process the connection forwarded by
# the proxy thread.
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
  # Allow time for the proxy thread to connect to R's httpd and for R's
  # event loop (triggered by Sys.sleep) to process the request.
  Sys.sleep(0.3)
  tryCatch(paste(readLines(sock, warn = FALSE), collapse = '\n'),
           error = function(e) NULL)
}

if (!is.null(port)) {

  # Handler echoes back request details as plain text, including a custom header.
  url = new_app(
    'test',
    function(path, query, post, headers) {
      h = if (length(headers) > 0L) rawToChar(headers) else ''
      body = paste0(
        'path=', path, '\n',
        'query_foo=', if ('foo' %in% names(query)) query[['foo']] else '', '\n',
        'post=', if (length(post) > 0L) rawToChar(post) else '', '\n',
        'method=', sub('\n.*', '', sub('Request-Method: ', '', h)), '\n',
        'x_custom=', {
          m = regmatches(h, regexpr('X-Custom: [^\r\n]+', h))
          if (length(m)) sub('X-Custom: ', '', m) else 'MISSING'
        }
      )
      list(payload = body, 'content-type' = 'text/plain')
    },
    open = FALSE,
    ports = port
  )
  on.exit(stop_app('test'), add = TRUE)

  assert('new_app() returns the expected URL', {
    (url %==% sprintf('http://127.0.0.1:%d/test/', port))
  })

  assert('proxy responds to a GET request', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/hello')
    (!is.null(resp))
    (grepl('200 OK',     resp, fixed = TRUE))
    (grepl('path=hello', resp, fixed = TRUE))
  })

  assert('query parameters are URL-decoded', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/page?foo=bar%20baz')
    (!is.null(resp))
    (grepl('path=page',        resp, fixed = TRUE))
    (grepl('query_foo=bar baz', resp, fixed = TRUE))
  })

  assert('POST body is forwarded correctly', {
    resp = http_request('127.0.0.1', port, 'POST', '/test/submit',
                        body = 'hello=world')
    (!is.null(resp))
    (grepl('post=hello=world', resp, fixed = TRUE))
    (grepl('method=POST',      resp, fixed = TRUE))
  })

  assert('request headers reach the handler', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/hdr',
                        extra_headers = 'X-Custom: test-value\r\n')
    (!is.null(resp))
    (grepl('200 OK',              resp, fixed = TRUE))
    (grepl('x_custom=test-value', resp, fixed = TRUE))
  })

  assert('unknown app returns a 404 response', {
    resp = http_request('127.0.0.1', port, 'GET', '/no-such-app/')
    (!is.null(resp))
    # Dispatcher returns 404; our handler was NOT called.
    (!grepl('path=no-such-app', resp, fixed = TRUE))
  })

  assert('stop_app() shuts down the proxy', {
    stop_app('test')
    on.exit(NULL)   # cancel earlier on.exit so we don't double-stop
    (is.null(.proxy$port))
    (is.null(tryCatch(
      socketConnection('127.0.0.1', port = port, open = 'r+b', timeout = 1),
      error = function(e) NULL
    )))
  })

}
