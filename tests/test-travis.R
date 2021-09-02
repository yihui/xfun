# tests needing an internet connection
options(repos = c(CRAN = "https://cran.rstudio.com"))
if (!is.na(Sys.getenv('CI', NA))) testit::test_pkg('xfun', 'test-travis')
