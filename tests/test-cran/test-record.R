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
