library(testit)

pkgs = c('KernSmooth', 'xfun', 'knitr')
pkg_bib(pkgs, tempfile(), tweak = FALSE)

assert('& is escaped in title when pkg_bib(tweak = TRUE)', {
  (length(grep(' & ', grep(
    '^\\s*title =', capture.output(pkg_bib(pkgs, tweak = TRUE)), value = TRUE
  ))) %==% 0L)
})

assert('pkg_bib() warns on missing packages', {
  has_warning(pkg_bib(c('xfun', '__nonexistent_pkg__'), tempfile()))
})

assert('make_unique() converts repeated values to unique suffixes', {
  # already unique → returned unchanged
  (make_unique(c('a', 'b', 'c')) %==% c('a', 'b', 'c'))
  # empty input
  (make_unique(character(0)) %==% character(0))
  # duplicate values get letter suffixes (a, b, c, ...)
  res = make_unique(c('2020', '2020', '2020'))
  (res %==% c('2020a', '2020b', '2020c'))
  # more than 26 duplicates: numeric suffix fallback
  res2 = make_unique(rep('x', 28))
  (res2[27] %==% 'x27')
  (res2[28] %==% 'x28')
})
