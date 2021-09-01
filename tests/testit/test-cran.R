library(testit)

assert("cran_package_dates() needs the name of a maintainer", {
  has_error(cran_pkg_dates(maintainer = NULL))
})

assert("cran_package_dates() returns the good class", {
  x <- cran_pkg_dates()
  lapply(x, function(y) {
    class(y) %==% "Date"
  })

  # several dates per package, hence nested lapply
  x <- cran_pkg_dates(full = TRUE)
  lapply(x, function(y) {
    lapply(y, function(z) {
      class(z) %==% "Date"
    })
  })

})

assert("cran_updatable() returns nothing if no duration specified", {
  x <- cran_updatable(days = NULL)
  length(x) %==% 0L
})

assert("cran_updatable() returns a character vector", {
  x <- cran_updatable()
  (is.character(x))
})

assert("pkg_maintainers() needs a package name", {
  has_error(pkg_maintainers())
})

assert("pkg_maintainers() works", {
  x <- pkg_maintainers("xfun")
  (length(x) > 0)
  # %==% not working for some reason
  (x[1] == "Yihui Xie <xie@yihui.name>")
})

