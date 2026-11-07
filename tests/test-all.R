library(testit)
test_pkg('xfun', 'test-cran')
if (tolower(Sys.getenv('CI')) == 'true') {
  test_pkg('xfun', 'test-ci')
  # browser tests need Chromium, Chrome, or Edge (not available on Alpine)
  library(xfun)
  f = tempfile(fileext = '.html'); writeLines('<p>x</p>', f)
  if (!has_error(browser_dom(f), 'Cannot find')) test_pkg('xfun', 'test-browser')
  unlink(f)
}
