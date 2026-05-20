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

# the option try.outFile was introduced in R 3.4.0
opts = options(try.outFile = NULL)
if (getRversion() >= '3.4.0') assert('record() can capture try() messages', {
  rec = record('try(stop("asdf qwer zxcv"))')
  (length(rec) %==% 2L)
  (grepl('asdf qwer zxcv', rec[[2]]))
})
options(opts)

assert('record() returns empty result for NULL code', {
  res = record(NULL)
  (inherits(res, 'xfun_record_results'))
  (length(res) %==% 0L)
})

assert('record() captures messages', {
  res = record('message("hello")', message = TRUE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('message' %in% cls)
  (any(grepl('hello', unlist(res))))
})

assert('record() suppresses messages when message = FALSE', {
  res = record('message("hidden")', message = FALSE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  (!('message' %in% cls))
})

assert('record() captures warnings', {
  res = record('warning("oops")', warning = TRUE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('warning' %in% cls)
  (any(grepl('oops', unlist(res))))
})

assert('record() captures errors when error = TRUE', {
  res = record('stop("boom")', error = TRUE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('error' %in% cls)
  (any(grepl('boom', unlist(res))))
})

assert('record() handles syntax errors gracefully', {
  res = record('1 +', error = TRUE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('source' %in% cls)
  ('error' %in% cls)
})

assert('record() with verbose = 2 prints invisible values', {
  res = record('invisible(42)', verbose = 2)
  (any(grepl('42', unlist(res))))
})

assert('record() with verbose = 1 prints last invisible value', {
  res = record(c('invisible(1)', 'invisible(2)'), verbose = 1)
  (any(grepl('2', unlist(res))))
})

assert('record() captures text output', {
  res = record('cat("hello world")')
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('output' %in% cls)
  (any(grepl('hello world', unlist(res))))
})

assert('format.xfun_record_results() works for text output', {
  res = record(c('1 + 1', 'cat("hi")'))
  txt = format(res, to = 'text')
  (is.character(txt))
  (any(grepl('1 \\+ 1', txt)))
  (any(grepl('hi', txt)))
})

assert('format.xfun_record_results() works for markdown output', {
  res = record(c('1 + 1'))
  md = format(res, to = 'markdown')
  (is.character(md))
  (any(grepl('```', md)))
})

assert('format.xfun_record_results() works for html output', {
  res = record(c('1 + 1'))
  html = format(res, to = 'html')
  (is.character(html))
  (any(grepl('<pre', html)))
  (any(grepl('</code></pre>', html)))
})

assert('format.xfun_record_results() html with template', {
  res = record('1 + 1')
  html = format(res, to = 'html', template = TRUE)
  (any(grepl('<html', html) | grepl('<!DOCTYPE', html)))
})

assert('print.xfun_record_results() works in non-interactive mode', {
  res = record('1 + 1')
  out = capture.output(print(res, browse = FALSE))
  (length(out) > 0L)
})

assert('record() with multiple messages in one expression', {
  res = record('{message("a"); message("b")}', message = TRUE)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  ('message' %in% cls)
})

assert('new_record() creates record objects with correct class', {
  r = new_record('hello', 'output')
  (inherits(r, 'record_output'))
  (r %==% structure('hello', class = 'record_output'))
})

assert('record_print.default() captures print output', {
  out = record_print(1:3)
  (is.character(out))
  (any(grepl('1 2 3', out)))
})

assert('format() markdown output includes fenced blocks for messages/warnings', {
  res = record(c('message("hi")', 'warning("yo")'), message = TRUE, warning = TRUE)
  md = format(res, to = 'markdown')
  (any(grepl('```', md)))
  (any(grepl('[.]message', md)))
  (any(grepl('[.]warning', md)))
})

assert('format() html output for messages and errors', {
  res = record('stop("boom")', error = TRUE)
  html = format(res, to = 'html')
  (any(grepl('class="error"', html)))
  (any(grepl('boom', html)))
})

assert('record() with error = NA halts on error', {
  (has_error(record('stop("fail")', error = NA)))
})

assert('record() with message = NA does not capture messages', {
  res = record('message("pass through")', message = NA)
  cls = sapply(res, function(x) sub('record_', '', class(x)))
  (!('message' %in% cls))
})
