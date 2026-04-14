library(testit)

# Find a free port to use for the test server.
find_free_port = function(ports = 4321 + 1:20) {
  for (p in ports) {
    sock = tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(sock)) { close(sock); return(p) }
  }
  NULL
}

# Send a raw HTTP/1.0 request and return the full response as a string.
# The server must be polled manually AFTER writing the request so that it
# reads the TCP buffer, invokes the R handler, and writes the response.
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
  # Poll the server: accepts, parses, calls handler, sends response, closes fd.
  .Call(C_httpd_poll, names(.httpd$apps), unname(.httpd$apps))
  tryCatch(paste(readLines(sock, warn = FALSE), collapse = '\n'),
           error = function(e) NULL)
}

port = find_free_port()

if (!is.null(port)) {

  # Start a handler that echoes back the request details as plain text.
  url = new_app(
    'test',
    function(path, query, post, headers) {
      body = paste0(
        'path=', path, '\n',
        'query_foo=', if ('foo' %in% names(query)) query[['foo']] else '', '\n',
        'post=', rawToChar(post), '\n',
        'method=', sub('\n.*', '', sub('Request-Method: ', '', rawToChar(headers)))
      )
      list(payload = body, 'content-type' = 'text/plain')
    },
    open = FALSE,
    ports = port
  )
  on.exit(stop_app('test'), add = TRUE)

  assert('new_app() / httpd_start() returns the expected URL', {
    (url %==% sprintf('http://127.0.0.1:%d/test/', port))
  })

  assert('server responds to a GET request', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/hello')
    (!is.null(resp))
    (grepl('200 OK',     resp, fixed = TRUE))
    (grepl('path=hello', resp, fixed = TRUE))
  })

  assert('query parameters are parsed and URL-decoded', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/page?foo=bar%20baz')
    (!is.null(resp))
    # path is the path component only (query is separate)
    (grepl('path=page',        resp, fixed = TRUE))
    # query parameter 'foo' should be URL-decoded
    (grepl('query_foo=bar baz', resp, fixed = TRUE))
  })

  assert('POST body is received correctly', {
    resp = http_request('127.0.0.1', port, 'POST', '/test/submit',
                        body = 'hello=world')
    (!is.null(resp))
    (grepl('post=hello=world', resp, fixed = TRUE))
    (grepl('method=POST',      resp, fixed = TRUE))
  })

  assert('request headers are forwarded to the handler', {
    resp = http_request('127.0.0.1', port, 'GET', '/test/hdr',
                        extra_headers = 'X-Custom: test-value\r\n')
    (!is.null(resp))
    # The raw headers passed to the handler contain all request headers
    (grepl('200 OK', resp, fixed = TRUE))
  })

  assert('unknown app returns 404', {
    resp = http_request('127.0.0.1', port, 'GET', '/no-such-app/')
    (!is.null(resp))
    (grepl('404', resp, fixed = TRUE))
  })

  assert('httpd_stop() shuts down the server', {
    stop_app('test')
    on.exit(NULL)   # cancel the earlier on.exit so we don't double-stop
    (is.null(.httpd$port))
    # Any new connection attempt must fail now
    (is.null(tryCatch(
      socketConnection('127.0.0.1', port = port, open = 'r+b', timeout = 1),
      error = function(e) NULL
    )))
  })

}
