library(testit)

assert('session_info() strips BLAS/LAPACK/matrix-products and base-packages noise', {
  si = session_info()
  (!any(grepl('BLAS', si, fixed = TRUE)))
  (!any(grepl('LAPACK', si, fixed = TRUE)))
  (!any(grepl('Matrix products', si, fixed = TRUE)))
  (!any(grepl('attached base packages', si, ignore.case = TRUE)))
  # no consecutive blank lines
  blank = !nzchar(trimws(si))
  (!any(blank[-1] & blank[-length(blank)]))
})

assert('session_info() renames "other attached packages" and filters by package name', {
  si_all = session_info()
  (!any(grepl('other attached packages', si_all, ignore.case = TRUE)))
  (any(grepl('Package version:', si_all, fixed = TRUE)))
  # packages= restricts output to the named package
  si = session_info('xfun', dependencies = FALSE)
  ver = as.character(packageVersion('xfun'))
  (any(grepl(paste0('xfun_', ver), si, fixed = TRUE)))
  # empty strings in packages= are silently ignored
  si2 = session_info(c('xfun', ''), dependencies = FALSE)
  (identical(si, si2))
})
