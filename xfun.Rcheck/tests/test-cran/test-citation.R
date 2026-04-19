library(testit)

pkgs = c('KernSmooth', 'xfun', 'knitr')
pkg_bib(pkgs, tempfile(), tweak = FALSE)

assert('& is escaped in title when pkg_bib(tweak = TRUE)', {
  (length(grep(' & ', grep(
    '^\\s*title =', capture.output(pkg_bib(pkgs, tweak = TRUE)), value = TRUE
  ))) %==% 0L)
})
