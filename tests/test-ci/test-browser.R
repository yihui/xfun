library(testit)

# skip tests that need a browser (e.g., on Alpine) when none is found
has_browser = !has_error(find_browser())

if (has_browser) assert('browser_dom() executes JS, returns rendered DOM, and strips scripts', {
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    '<html><body><div id="out"></div>',
    '<script>document.getElementById("out").textContent = 1 + 1;</script>',
    '</body></html>'
  ), f)
  (unclass(browser_dom(f)) %==% '<html><head></head><body><div id="out">2</div></body></html>')
})

if (has_browser) assert('browser_dom() saves to output file', {
  f = tempfile(fileext = '.html')
  out = tempfile(fileext = '.html')
  on.exit(unlink(c(f, out)), add = TRUE)
  writeLines(c(
    '<html><body><p>static</p>',
    '<script>\ndocument.body.insertAdjacentHTML("beforeend", "<p>" + (2+3) + "</p>")</script>',
    '</body></html>'
  ), f)
  res = browser_dom(f, out)
  (res %==% out)
  (unclass(file_string(out)) %==% '<html><head></head><body><p>static</p><p>5</p>\n\n</body></html>')
})

assert('with_browser_tmp() isolates and cleans the browser temp dir', {
  old = Sys.getenv('TMPDIR', unset = NA)
  # during evaluation TMPDIR points at a fresh dir created for the browser
  seen = with_browser_tmp(Sys.getenv('TMPDIR'))
  (startsWith(basename(seen), 'browser-') %==% TRUE)
  # afterward that dir is removed and TMPDIR restored to its prior value
  (dir.exists(seen) %==% FALSE)
  (identical(Sys.getenv('TMPDIR', unset = NA), old) %==% TRUE)
})

if (has_browser) assert('browser_dom(fragment = TRUE) returns body content only', {
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines('<html><head><title>test</title></head>\n<body>\n <p>content\n</p>\t\n</body></html>', f)
  res = browser_dom(f, fragment = TRUE)
  (unclass(res) %==% '<p>content\n</p>')
})

if (has_browser) assert('json_unpack unpack() decodes packed ints and passes arrays/typed arrays through', {
  x = rep(1:3, 50)
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    '<html><body><div id="out"></div><script>', json_unpack,
    sprintf('const p = unpack(%s), r = [p.length, p[1], unpack(p) === p, unpack([7, 8]).join(",")];',
            as.character(tojson(x, pack = 0, mode = 'json'))),
    'document.getElementById("out").textContent = r.join(";");',
    '</script></body></html>'
  ), f)
  (has_match(browser_dom(f), I('<div id="out">150;2;true;7,8</div>')))
})

if (has_browser) assert('browser_dom() waits for a condition and accepts query params', {
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    '<html><body><div id="out"></div><script>',
    'var q = location.search;',
    'setTimeout(() => {',
    '  document.getElementById("out").textContent = q + "|héllo";',
    '  window.done = true;',
    '}, 1000);',
    '</script></body></html>'
  ), f, useBytes = FALSE)
  res = browser_dom(f, query = list(paged = 1, a = 'b c'), wait = 'window.done')
  (unclass(res) %==% '<html><head></head><body><div id="out">?paged=1&amp;a=b%20c|héllo</div></body></html>')
  # a promise is awaited (its value is not used)
  res2 = browser_dom(f, wait = 'new Promise(r => setTimeout(() => r(), 1500))')
  (has_match(res2, 'héllo'))
  # wait for an event after triggering it
  g = tempfile(fileext = '.html'); on.exit(unlink(g), add = TRUE)
  writeLines(c(
    '<html><body><div id="out"></div><script>',
    'addEventListener("go", () => setTimeout(() => {',
    '  document.getElementById("out").textContent = "ok"; dispatchEvent(new Event("gone"));',
    '}, 500));',
    '</script></body></html>'
  ), g)
  (has_match(browser_dom(g, wait = c(event = 'gone', trigger = 'go')), '>ok<'))
  # time out if the condition is never true
  (has_error(browser_dom(f, wait = 'window.never', timeout = 3)))
})

if (has_browser) assert('browser_print() prints and takes screenshots via DevTools', {
  f = tempfile(fileext = '.html')
  out = tempfile(fileext = c('.pdf', '.png', '.jpeg'))
  on.exit(unlink(c(f, out)), add = TRUE)
  writeLines(c(
    '<html><body><p id="out">waiting</p><script>',
    'setTimeout(() => { document.getElementById("out").textContent = "ready"; window.ok = 1 }, 500);',
    '</script></body></html>'
  ), f)
  for (o in out) (browser_print(f, o, wait = 'window.ok') %==% o)
  (file.size(out) > 1000)

  (readBin(out[1], 'raw', 4) %==% charToRaw('%PDF'))
  (readBin(out[2], 'raw', 4)[2:4] %==% charToRaw('PNG'))
})

if (has_browser) assert('browser_session() reuses one browser for several pages', {
  f1 = tempfile(fileext = '.html'); f2 = tempfile(fileext = '.html')
  on.exit(unlink(c(f1, f2)), add = TRUE)
  writeLines('<p id="a">one</p><script>document.body.dataset.r = 1</script>', f1)
  writeLines('<p id="a">two</p>', f2)
  s = browser_session()
  on.exit(s$close(), add = TRUE)
  (s$open(f1, wait = 'document.body.dataset.r === "1"') %==% s)
  (s$eval('document.getElementById("a").textContent') %==% 'one')
  s$open(f2)
  (s$eval('document.getElementById("a").textContent') %==% 'two')
  (s$eval('var x = 1; x + 1') %==% '2')  # multiple statements
  (s$dom() %==% '<html><head></head><body><p id="a">two</p>\n</body></html>')
})
