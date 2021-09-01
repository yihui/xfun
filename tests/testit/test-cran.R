library(testit)

assert("pkg_maintainers() works", {
  x <- pkg_maintainers("xfun")
  (length(x) > 0)
  # %==% not working for some reason
  (x[1] == "Yihui Xie <xie@yihui.name>")
})

