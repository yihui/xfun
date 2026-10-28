library(testit)

assert('header_value() and content_length() parse header lines', {
  h = c('HTTP/1.1 200 OK', 'Content-Length: 12', 'X-A:  b c ')
  (header_value(h, 'x-a') %==% 'b c')
  (header_value(h, 'x-b') %==% NULL)
  (content_length(h) %==% 12L)
  (is.na(content_length(h[1])))
  (is.na(content_length('content-length: abc')))
})

assert('read_headers() stops at the blank line, and is NULL when cut short', {
  con = textConnection(c('a: 1', 'b: 2', '', 'body')); on.exit(close(con))
  (read_headers(con) %==% c('a: 1', 'b: 2'))
  (read_headers(con) %==% 'body' || is.null(read_headers(con)))
  con2 = textConnection('a: 1'); on.exit(close(con2), add = TRUE)
  (read_headers(con2) %==% NULL)
})

assert('raw_to_utf8() marks the encoding', {
  (Encoding(raw_to_utf8(charToRaw('中'))) %==% 'UTF-8')
})

assert('.http_head() finds the end of the head of an HTTP message', {
  x = charToRaw('GET / HTTP/1.1\r\nHost: a\r\n\r\nbody')
  h = .http_head(x)
  (h$lines %==% c('GET / HTTP/1.1', 'Host: a'))
  (rawToChar(x[-seq_len(h$end)]) %==% 'body')
  (.http_head(charToRaw('GET / HTTP/1.1\r\nHost')) %==% NULL)
  (.http_head(charToRaw('GET /\n\nx'))$lines %==% 'GET /')
})

assert('port_open() detects a listening port', {
  (port_open(random_port(), timeout = .2) %==% FALSE)
})
