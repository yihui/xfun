# A minimal client of the Chrome DevTools Protocol (CDP) in base R, which is
# enough to print a page to PDF, take a screenshot, or dump the DOM *after* a
# condition on the page becomes true (command-line flags can only guess how long
# to wait). CDP messages are JSON strings sent over a WebSocket; we only need to
# send commands, and find a few fields in the responses with regular expressions,
# so there is no need for a JSON parser.

# send a text message to the server as a single masked frame (the mask is
# required of clients but is meaningless for a local server, so it is fixed
# instead of consuming random numbers from the user's RNG stream)
ws_send = function(con, text) {
  x = charToRaw(enc2utf8(text)); n = length(x)
  len = if (n < 126) as.raw(128 + n) else if (n < 65536) {
    c(as.raw(254), writeBin(as.integer(n), raw(), size = 2, endian = 'big'))
  } else c(as.raw(255), raw(4), writeBin(as.integer(n), raw(), size = 4, endian = 'big'))
  mask = as.raw(c(55, 250, 33, 148))
  writeBin(c(as.raw(129), len, mask, xor(x, rep_len(mask, n))), con)
  flush(con)
}

# read exactly n bytes (readBin() may return fewer on a socket)
read_exact = function(con, n) {
  x = raw()
  while (length(x) < n) {
    y = readBin(con, 'raw', n - length(x))
    if (!length(y)) stop2('The connection to the browser was closed.')
    x = c(x, y)
  }
  x
}

# read a text message, which may consist of multiple frames
ws_recv = function(con) {
  msg = list()
  repeat {
    h = as.integer(read_exact(con, 2))
    op = bitwAnd(h[1], 15); n = bitwAnd(h[2], 127)
    if (n == 126) n = sum(as.integer(read_exact(con, 2)) * 256^(1:0))
    if (n == 127) n = sum(as.integer(read_exact(con, 8)) * 256^(7:0))
    p = read_exact(con, n)  # frames from the server are not masked
    if (op == 8) stop2('The connection to the browser was closed.')
    if (op > 1) next  # ignore ping/pong
    msg[[length(msg) + 1]] = p
    if (bitwAnd(h[1], 128) > 0) break  # the final frame
  }
  x = rawToChar(do.call(c, msg))
  Encoding(x) = 'UTF-8'
  x
}

# open a WebSocket connection to the DevTools server of the browser
ws_open = function(port, path, timeout) {
  con = socketConnection(
    '127.0.0.1', port, open = 'r+b', blocking = TRUE, timeout = timeout
  )
  writeBin(charToRaw(sprintf(paste0(
    'GET %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nUpgrade: websocket\r\n',
    'Connection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n'
  ), path, port, base64_encode(as.raw(1:16)))), con)
  repeat {  # skip the HTTP response headers
    h = readLines(con, 1, warn = FALSE)
    if (!length(h)) stop2('Failed to connect to the browser.')
    if (!nzchar(trimws(h))) break
  }
  con
}

# extract the base64 data from a CDP response and decode it (the data can be
# many MB, so avoid regular expressions on the whole string)
cdp_data = function(x, key = '"data":"') {
  i = regexpr(key, x, fixed = TRUE)
  if (i < 0) stop2('Failed to get the data from the browser.')
  x = substring(x, i + attr(i, 'match.length'))
  base64_decode(substr(x, 1, regexpr('"', x, fixed = TRUE) - 1))
}

# find the WebSocket path of a page via the HTTP API of the DevTools server (use
# a socket instead of url() so that proxy settings do not get in the way)
http_get_page = function(port) {
  con = socketConnection('127.0.0.1', port, open = 'r+b', blocking = TRUE, timeout = 10)
  on.exit(close(con))
  writeBin(charToRaw(paste0(
    'GET /json/list HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n'
  )), con)
  # the server keeps the connection open, so read the body by Content-Length
  n = 0
  repeat {
    h = readLines(con, 1, warn = FALSE)
    if (!length(h)) return()
    if (!nzchar(trimws(h))) break
    if (grepl('^content-length:', h, ignore.case = TRUE))
      n = as.integer(sub('^[^:]+:\\s*', '', h))
  }
  x = rawToChar(read_exact(con, n))
  r = '.*"type": *"page", *"url": *"[^"]*", *"webSocketDebuggerUrl": *"ws://[^/]+(/[^"]+)".*'
  if (grepl(r, gsub('\n', '', x))) sub(r, '\\1', gsub('\n', '', x))
}

# Start the browser with remote debugging, load `url`, wait until the page is
# ready (`wait` is a JavaScript expression), and get the result of `type`: 'pdf',
# 'png', 'jpeg' (raw vector), or 'dom' (the HTML of the page).
browser_cdp = function(...) with_browser_tmp(cdp_session(...))

cdp_session = function(browser, args, url, wait, timeout, size, type) {
  {
    dir = tempfile('profile'); err = tempfile()
    args = c(
      args, '--remote-debugging-port=0',
      shQuote(paste0('--user-data-dir=', dir)), 'about:blank'
    )
    t0 = Sys.time()
    timed_out = function() difftime(Sys.time(), t0, units = 'secs') > timeout
    system2(browser, args, wait = FALSE, stdout = FALSE, stderr = err)
    # the browser tells us the port of the DevTools server in this file
    f = file.path(dir, 'DevToolsActivePort')
    while (!(file.exists(f) && length(x <- readLines(f, warn = FALSE)) >= 2)) {
      if (timed_out()) browser_fail(
        browser, 'Failed to start the browser', err
      )
      Sys.sleep(.1)
    }
    port = as.integer(x[1]); bpath = x[2]
    on.exit({
      # close the whole browser (via its own endpoint) instead of just the page
      try({
        con2 = ws_open(port, bpath, timeout)
        ws_send(con2, '{"id":1,"method":"Browser.close"}')
        close(con2)
      }, silent = TRUE)
      Sys.sleep(.5)  # give the browser time to exit before its profile is deleted
    }, add = TRUE)
    # connect to the first page of the browser
    repeat {
      path = http_get_page(port)
      if (length(path)) break
      if (timed_out()) stop2('The browser did not open a page.')
      Sys.sleep(.1)
    }
    con = ws_open(port, path, timeout)
    id = 0
    call = function(method, params = '{}') {
      id <<- id + 1; i = id
      ws_send(con, sprintf('{"id":%d,"method":"%s","params":%s}', i, method, params))
      repeat {
        m = ws_recv(con)
        if (startsWith(m, sprintf('{"id":%d,', i))) break
      }
      if (grepl('^\\{"id":\\d+,"error":', m)) stop2('DevTools error: ', m)
      m
    }
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    if (!is.null(size)) call('Emulation.setDeviceMetricsOverride', sprintf(
      '{"width":%d,"height":%d,"deviceScaleFactor":1,"mobile":false}',
      size[1], size[2]
    ))
    call('Page.navigate', sprintf('{"url":%s}', tojson(url)))
    # `wait` can be a value (ready when it is true) or a promise (ready when it
    # is resolved)
    ready = tojson(sprintf(paste0(
      "(async () => {",
      "if (location.href === 'about:blank' || document.readyState !== 'complete') return false;",
      "const v = (%s); if (typeof v?.then !== 'function') return !!v; await v; return true;",
      "})()"
    ), wait %||% 'true'))
    repeat {
      ok = tryCatch(grepl('"value":true', call(
        'Runtime.evaluate',
        sprintf('{"expression":%s,"returnByValue":true,"awaitPromise":true}', ready)
      ), fixed = TRUE), error = function(e) FALSE)
      if (ok) break
      if (timed_out()) stop2(
        'The page was not ready in ', timeout, ' seconds. Please check the ',
        "argument 'wait', or increase 'timeout'."
      )
      Sys.sleep(.2)
    }
    switch(
      type,
      pdf = cdp_data(call(
        'Page.printToPDF', '{"printBackground":true,"preferCSSPageSize":true}'
      )),
      png = , jpeg = cdp_data(call(
        'Page.captureScreenshot', sprintf('{"format":"%s"}', type)
      )),
      dom = {
        # base64 avoids unescaping the JSON string
        m = call('Runtime.evaluate', sprintf('{"expression":%s}', tojson(paste0(
          'btoa(unescape(encodeURIComponent(',
          'document.documentElement.outerHTML)))'
        ))))
        x = rawToChar(cdp_data(m, '"value":"'))
        Encoding(x) = 'UTF-8'
        x
      }
    )
  }
}

# turn a path into a file:// URL and append query parameters (a named list or
# vector) to the URL
browser_url = function(input, query = NULL) {
  if (!grepl('^[a-zA-Z][a-zA-Z0-9+.-]*:(//|[^\\\\/])', input)) {  # not a URL
    input = paste0(
      'file://', if (is_windows()) '/', URLencode(normalize_path(input))
    )
  }
  if (length(query)) {
    q = paste0(
      URLencode(names(query), TRUE), '=',
      URLencode(vapply(query, as.character, ''), TRUE), collapse = '&'
    )
    x = strsplit(input, '#', fixed = TRUE)[[1]]
    input = paste0(
      x[1], if (grepl('?', x[1], fixed = TRUE)) '&' else '?', q,
      if (length(x) > 1) paste0('#', paste(x[-1], collapse = '#'))
    )
  }
  input
}
