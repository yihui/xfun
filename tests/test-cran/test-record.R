library(testit)

record2 = function(...) unlist(record(...))

assert('record() works', {
  (record2('quote(x)') %==% c('quote(x)', 'x'))
  (record2('quote(x+y)') %==% c('quote(x+y)', 'x + y'))
})

assert('record() handles raw_string objects', {
  s = raw_string(c('line 1', 'line 2\nwith newline', 'line 3 "with quotes"'))
  rec = record('s')
  (as.character(rec[[2]]) %==% as.character(s))
})

if (loadable('data.table')) assert('record() works with data.table', {
  x = c('d = data.table::as.data.table(mtcars)', 'd[, mpg := 99]')
  rec = record(x, envir = globalenv())
  (length(rec) %==% 1L)  # only source code, no output printed
  (as.character(rec[[1]]) %==% x)
})

assert('record() avoids recording empty plots', {
  rec = record('library(grid); g <- pointsGrob(); x <- convertUnit(g$x, "mm")')
  (length(rec) %==% 1L)  # only source code, no plot generated
})

assert('record() can selectively keep plots', {
  f = function(i = TRUE) record(c('plot(1)', 'plot(2)'), dev.keep = i)
  cl = function(x) sub('^record_', '', sapply(x, class))
  (cl(f()) %==% rep(c('source', 'plot'), 2))  # keep all plots
  (cl(f(-1)) %==% c('source', 'plot'))  # remove 1st plot
  (cl(f(c(FALSE, TRUE))) %==% c('source', 'plot'))  # ditto
  (cl(f('last')) %==% c('source', 'plot'))  # keep the last plot only
  (cl(f(-2)) %==% c('source', 'plot', 'source'))  # remove 2nd plot
  (cl(f(FALSE)) %==% 'source')  # remove all plots
})
