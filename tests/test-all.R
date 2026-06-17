library(testit)
test_pkg('xfun', 'test-cran')
if (tolower(Sys.getenv('CI')) == 'true') test_pkg('xfun', 'test-ci')
