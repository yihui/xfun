library(testit)

assert('attr() is strict', {
  z = structure(list(a = 1), foo = 2)
  (attr(z, 'foo') %==% 2)
  (attr(z, 'f') %==% NULL)
})

assert('in_dir() preserves the working directory', {
  owd = getwd()
  in_dir('.', setwd(tempdir()))
  (same_path(owd, getwd()))
})

assert('stringsAsFactors() makes sure strings are not converted to factors', {
  f = function() {
    strings_please()
    data.frame(x = letters[1:4], y = factor(letters[1:4]))
  }
  is.character(f()[, 1])
})
