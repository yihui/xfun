library(testit)

assert('find_globals() identifies global variables', {
  env = list2env(
    list(qwer = 1, g = 2, y = 3, d = 4, z = 5), parent = emptyenv()
  )
  # nothing from outside environment
  (find_globals('x=1', env) %==% character(0))
  # qwer must be created outside somewhere
  (find_globals('a=1; b=a; d=qwer', env) %==% 'qwer')
  (find_globals('a=function(){f=2;g}', env) %==% 'g')
  # y was assigned locally in z, but there is another y outside from nowhere
  (find_globals('z=function(){y=1};y', env) %==% 'y')
  # more complicated cases: operators, subscripts, ...
  (find_globals(c('a=1%*%1%o%2 %in% d', 'b=d%%10+3%/%2-z[1:3]'), env) %==% c('d', 'z'))
})

assert('global_vars() returns values of global variables in code', {
  e = new.env(parent = emptyenv())
  e$x = 42
  res = global_vars('y = x + 1', e)
  (is.list(res))
  ('x' %in% names(res))
  (res$x %==% 42)
})

assert('md5_obj() computes md5 hash for a character vector or object', {
  h1 = md5_obj(c('a', 'b'))
  h2 = md5_obj(c('a', 'b'))
  h3 = md5_obj(c('a', 'c'))
  (h1 %==% h2)
  (h1 != h3)
  (nchar(h1) %==% 32L)
})

assert('clean_cache() removes old cache files with same base name', {
  d = tempfile()
  dir.create(d)
  base = 'mycode'
  ext = 'rds'
  old_hash = paste(rep('a', 32), collapse = '')
  new_hash = paste(rep('b', 32), collapse = '')
  old_file = file.path(d, sprintf('%s_%s.%s', base, old_hash, ext))
  new_file = file.path(d, sprintf('%s_%s.%s', base, new_hash, ext))
  writeLines('old', old_file)
  clean_cache(new_file, ext)  # should remove old_file
  (!file.exists(old_file))
  unlink(d, recursive = TRUE)
})

assert('cache_rds() caches an expression to disk', {
  d = tempfile()
  dir.create(d)
  f = 'mycode.rds'
  # first call evaluates and caches
  r1 = cache_rds(1 + 1, file = f, dir = paste0(d, '/'))
  (r1 %==% 2)
  # second call loads from cache
  r2 = cache_rds(1 + 1, file = f, dir = paste0(d, '/'))
  (r2 %==% 2)
  # rerun = TRUE forces re-evaluation
  r3 = cache_rds(1 + 1, file = f, dir = paste0(d, '/'), rerun = TRUE)
  (r3 %==% 2)
  unlink(d, recursive = TRUE)
})
