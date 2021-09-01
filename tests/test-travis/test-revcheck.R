library(testit)

assert("check_deps() works", {
  x <- check_deps("xfun")
  (is.character(x$check))
  (is.character(x$install))
})

