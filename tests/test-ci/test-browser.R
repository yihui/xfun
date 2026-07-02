library(testit)

assert('browser_dom() executes JS, returns rendered DOM, and strips scripts', {
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines(c(
    '<html><body><div id="out"></div>',
    '<script>document.getElementById("out").textContent = 1 + 1;</script>',
    '</body></html>'
  ), f)
  (unclass(browser_dom(f)) %==% '<html><head></head><body><div id="out">2</div></body></html>')
})

assert('browser_dom() saves to output file', {
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

assert('browser_dom(fragment = TRUE) returns body content only', {
  f = tempfile(fileext = '.html')
  on.exit(unlink(f), add = TRUE)
  writeLines('<html><head><title>test</title></head>\n<body>\n <p>content\n</p>\t\n</body></html>', f)
  res = browser_dom(f, fragment = TRUE)
  (unclass(res) %==% '<p>content\n</p>')
})
