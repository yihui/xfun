library(testit)

assert("check_deps() works", {
  has_error(check_deps())

  x <- check_deps("xfun")
  (is.character(x$check))
  (is.character(x$install))
})

assert("rev_check() needs a package name", {
  has_error(rev_check())
})
