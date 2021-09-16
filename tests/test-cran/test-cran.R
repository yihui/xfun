library(testit)

assert("pkg_maintainers() works", {
  x = pkg_maintainers("xfun")
  (length(x) > 0)
  # %==% not working for some reason
  (x %==% c(xfun = packageDescription('xfun', fields = 'Maintainer')))
})

