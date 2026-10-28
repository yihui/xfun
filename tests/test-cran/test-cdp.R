library(testit)

assert('browser_url() turns paths into file:// URLs and appends query params', {
  f = tempfile(fileext = '.html'); file.create(f); on.exit(unlink(f))
  u = browser_url(f)
  (startsWith(u, 'file://') %==% TRUE)
  (endsWith(u, basename(f)) %==% TRUE)
  (browser_url('https://example.com/a.html') %==% 'https://example.com/a.html')
  (browser_url('https://example.com/a.html', list(paged = 1)) %==%
    'https://example.com/a.html?paged=1')
  (browser_url('https://example.com/a?x=1#top', list(paged = 1, a = 'b c')) %==%
    'https://example.com/a?x=1&paged=1&a=b%20c#top')
  (browser_url('https://example.com/a.html', c(paged = '1')) %==%
    'https://example.com/a.html?paged=1')
  (endsWith(browser_url(f, list(paged = 1)), '.html?paged=1') %==% TRUE)
})

assert('ws_send() writes a masked text frame', {
  frame = function(text) {
    con = rawConnection(raw(), 'wb'); on.exit(close(con))
    ws_send(con, text)
    rawConnectionValue(con)
  }
  unmask = function(x, n) xor(x[-(1:n)], rep_len(x[n - 3:0 + 0], length(x) - n))
  x = frame('hello')
  (as.integer(x[1:2]) %==% c(129L, 128L + 5L))
  (rawToChar(unmask(x, 6)) %==% 'hello')
  # 16-bit length
  s = strrep('a', 300); x = frame(s)
  (as.integer(x[1:4]) %==% c(129L, 254L, 1L, 44L))
  (rawToChar(unmask(x, 8)) %==% s)
  # 64-bit length (the high bytes are zero)
  s = strrep('b', 70000); x = frame(s)
  (as.integer(x[1:2]) %==% c(129L, 255L))
  (rawToChar(unmask(x, 14)) %==% s)
})

assert('ws_recv() reads single and fragmented messages', {
  recv = function(x) {
    con = rawConnection(x, 'rb'); on.exit(close(con))
    ws_recv(con)
  }
  msg = function(s) charToRaw(s)
  (recv(c(as.raw(c(129, 5)), msg('hello'))) %==% 'hello')
  # a fragmented message (first frame not final, then a continuation frame),
  # with a ping frame in between that should be ignored
  x = c(
    as.raw(c(1, 3)), msg('hel'), as.raw(c(137, 0)), as.raw(c(128, 2)), msg('lo')
  )
  (recv(x) %==% 'hello')
  # 16-bit length
  s = strrep('x', 1000)
  (recv(c(as.raw(c(129, 126, 3, 232)), msg(s))) %==% s)
  # the connection is closed by the server
  (has_error(recv(as.raw(c(136, 0)))))
})

assert('cdp_data() decodes base64 data in a response', {
  x = sprintf('{"id":1,"result":{"data":"%s"}}', base64_encode(charToRaw('abc')))
  (rawToChar(cdp_data(x)) %==% 'abc')
  x = sprintf('{"id":2,"result":{"result":{"value":"%s"}},"x":1}', base64_encode(charToRaw('xyz')))
  (rawToChar(cdp_data(x, '"value":"')) %==% 'xyz')
  (has_error(cdp_data('{"id":1,"result":{}}')))
})

assert('wait_js() converts the argument wait to a JavaScript expression', {
  (wait_js(NULL) %==% 'true')
  (wait_js('window.ok') %==% 'window.ok')
  (wait_js(c('a', 'b')) %==% 'a\nb')
  (grepl('addEventListener("done"', wait_js(c(event = 'done')), fixed = TRUE) %==% TRUE)
  (grepl('dispatchEvent', wait_js(c(event = 'done')), fixed = TRUE) %==% FALSE)
  x = wait_js(list(event = 'a:after', trigger = 'a:start'))
  (grepl('addEventListener("a:after"', x, fixed = TRUE) %==% TRUE)
  (grepl('dispatchEvent(new Event("a:start"))', x, fixed = TRUE) %==% TRUE)
  (has_error(wait_js(c(foo = 'bar'))))
})
