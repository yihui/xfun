library(testit)
test_pkg('xfun', 'test-cran')
if (tolower(Sys.getenv('CI')) == 'true') {
  test_pkg('xfun', 'test-ci')
  # browser tests need a browser
  if (!has_error(xfun:::find_browser())) test_pkg('xfun', 'test-browser')
}
