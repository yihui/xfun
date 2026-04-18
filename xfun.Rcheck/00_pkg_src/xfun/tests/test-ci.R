# tests to run on CI servers (e.g. GitHub Actions)
if (tolower(Sys.getenv('CI')) == 'true') testit::test_pkg('xfun', 'test-ci')
