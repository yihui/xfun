library(testit)

assert('attr2() is strict', {
  z = structure(list(a = 1), foo = 2)
  (attr2(z, 'foo') %==% 2)
  (attr2(z, 'f') %==% NULL)
})

assert('in_dir() preserves the working directory', {
  owd = getwd()
  in_dir('.', setwd(tempdir()))
  (same_path(owd, getwd()))
})
