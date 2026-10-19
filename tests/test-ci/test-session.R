library(testit)

assert('session_info() strips BLAS/LAPACK/matrix-products and base-packages noise', {
  si = session_info()
  (has_match(si, !I('BLAS')))
  (has_match(si, !I('LAPACK')))
  (has_match(si, !I('Matrix products')))
  (has_match(si, !'(?i)attached base packages'))
  # no consecutive blank lines
  blank = !nzchar(trimws(si))
  (!any(blank[-1] & blank[-length(blank)]))
})

assert('session_info() renames "other attached packages" and filters by package name', {
  si_all = session_info()
  (has_match(si_all, !'(?i)other attached packages'))
  (has_match(si_all, I('Package version:')))
  # packages= restricts output to the named package
  si = session_info('xfun', dependencies = FALSE)
  ver = as.character(packageVersion('xfun'))
  (has_match(si, I(paste0('xfun_', ver))))
  # empty strings in packages= are silently ignored
  si2 = session_info(c('xfun', ''), dependencies = FALSE)
  (identical(si, si2))
})
