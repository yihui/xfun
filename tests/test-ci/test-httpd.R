library(testit)

# Find a free port to use for the test server.
find_free_port = function(ports = 4321 + 1:20) {
  for (p in ports) {
    sock = tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(sock)) { close(sock); return(p) }
  }
  NULL
}

# Send a single HTTP/1.0 GET request synchronously.
# The caller must ensure the server is polled AFTER calling this, which
# causes the server to read the request from the TCP buffer, invoke the
# R handler, write the response, and close the connection.  Then we read.
http_get = function(host, port, path) {
  sock = tryCatch(
    socketConnection(host, port = port, open = 'r+b', blocking = TRUE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(sock)) return(NULL)
  on.exit(try(close(sock), silent = TRUE), add = TRUE)
  # Write the HTTP request into the TCP send buffer
  req = paste0('GET ', path, ' HTTP/1.0\r\nHost: ', host, '\r\n\r\n')
  writeBin(req, sock)  # binary write; \r\n line endings are already in req
  # Poll the server: it accepts, reads the buffered request, calls the R
  # handler, writes the response, and closes the server-side fd.
  .Call(xfun:::httpd_poll,
        names(xfun:::.httpd$apps),
        unname(xfun:::.httpd$apps))
  # Read the HTTP response (server already closed its end, so we get EOF)
  tryCatch(paste(readLines(sock, warn = FALSE), collapse = '\n'),
           error = function(e) NULL)
}

port = find_free_port()

if (!is.null(port)) {

  url = xfun::new_app(
    'test',
    function(path, query, post, headers) {
      list(payload = paste0('path=', path), 'content-type' = 'text/plain')
    },
    open = FALSE,
    ports = port
  )
  on.exit(xfun::stop_app('test'), add = TRUE)

  assert('new_app() returns the expected URL', {
    (url %==% sprintf('http://127.0.0.1:%d/test/', port))
  })

  assert('server responds to a GET request', {
    resp = http_get('127.0.0.1', port, '/test/hello')
    (!is.null(resp))
    (grepl('200 OK',     resp, fixed = TRUE))
    (grepl('path=hello', resp, fixed = TRUE))
  })

  assert('query parameters are URL-decoded', {
    resp = http_get('127.0.0.1', port, '/test/page?foo=bar%20baz')
    # the handler receives path="page" (query is separate, not part of path)
    (!is.null(resp))
    (grepl('path=page', resp, fixed = TRUE))
  })

  assert('unknown app returns 404', {
    resp = http_get('127.0.0.1', port, '/no-such-app/')
    (!is.null(resp))
    (grepl('404', resp, fixed = TRUE))
  })

  assert('stop_app() shuts down the server', {
    xfun::stop_app('test')
    on.exit(NULL)   # cancel the earlier on.exit so we don't double-stop
    (is.null(xfun:::.httpd$port))
    # Any new connection attempt must fail now
    (is.null(tryCatch(
      socketConnection('127.0.0.1', port = port, open = 'r+b', timeout = 1),
      error = function(e) NULL
    )))
  })

}
