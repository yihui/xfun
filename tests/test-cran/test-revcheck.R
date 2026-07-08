library(testit)

assert('clean_log() removes OK lines, current time, and other noise', {
  d = tempfile(); dir.create(d)
  owd = setwd(d); on.exit({ setwd(owd); unlink(d, recursive = TRUE) }, add = TRUE)

  writeLines(c(
    'log dir line',
    'R version line',
    '* using R version 4.4.0 ... OK',
    '* current time: 2026-07-07 14:49:46 UTC',
    '* checking foo ... WARNING',
    'some detail'
  ), '00check.log')

  x = clean_log()
  # first 2 lines removed, OK line removed, current time line removed
  (!any(grepl('OK$', x)))
  (!any(grepl('current time', x)))
  (x %==% c('* checking foo ... WARNING', 'some detail'))
})
