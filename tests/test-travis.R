# tests needing an internet connection
if (!is.na(Sys.getenv('CI', NA))) testit::test_pkg('xfun', 'test-travis')
