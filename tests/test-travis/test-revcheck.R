library(testit)

assert("check_deps() works", {
  options(repos = c(CRAN = "https://cran.rstudio.com"))
  x <- check_deps("xfun")
  (is.character(x$check))
  (is.character(x$install))
})

