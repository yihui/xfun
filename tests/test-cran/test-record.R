library(testit)

record2 = function(...) unlist(record(...))

assert('record() works', {
  (record2('quote(x)') %==% c('quote(x)', 'x'))
  (record2('quote(x+y)') %==% c('quote(x+y)', 'x + y'))
})
