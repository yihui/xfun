library(testit)

assert('cache_exec() with disk path caches and retrieves', {
  d = tempfile(); dir.create(d)
  path = paste0(d, '/')
  r1 = cache_exec({Sys.time()}, path = path, id = 'disk-test')
  Sys.sleep(0.01)
  r2 = cache_exec({Sys.time()}, path = path, id = 'disk-test')
  (r1 %==% r2)
  unlink(d, recursive = TRUE)
})

assert('cache_exec() invalidates when hash changes', {
  x = 1
  r1 = cache_exec({x + 1}, path = ':memory:', id = 'hash-change-test')
  (r1 %==% 2)
  x = 10
  r2 = cache_exec({x + 1}, path = ':memory:', id = 'hash-change-test')
  (r2 %==% 11)
})

assert('cache_exec() with keep = FALSE always re-evaluates', {
  r1 = cache_exec({runif(1)}, path = ':memory:', id = 'keep-false-test', keep = FALSE)
  r2 = cache_exec({runif(1)}, path = ':memory:', id = 'keep-false-test', keep = FALSE)
  (r1 != r2)
})

assert('cache_rds() with hash = "auto" detects global variables', {
  d = tempfile(); dir.create(d)
  x = 100
  r1 = cache_rds(x * 2, file = 'auto-hash.rds', dir = paste0(d, '/'), hash = 'auto')
  (r1 %==% 200)
  r2 = cache_rds(x * 2, file = 'auto-hash.rds', dir = paste0(d, '/'), hash = 'auto')
  (r2 %==% 200)
  unlink(d, recursive = TRUE)
})

assert('cache_rds() with hash as list invalidates on change', {
  d = tempfile(); dir.create(d)
  r1 = cache_rds(1 + 1, file = 'hash-list.rds', dir = paste0(d, '/'), hash = list(a = 1))
  (r1 %==% 2)
  r2 = cache_rds(1 + 1, file = 'hash-list.rds', dir = paste0(d, '/'), hash = list(a = 2))
  (r2 %==% 2)
  # clean = TRUE removes old cache, so only one file exists
  (length(list.files(d, pattern = '[.]rds$')) %==% 1L)
  unlink(d, recursive = TRUE)
})

assert('cache_rds() errors on unsupported file extension', {
  d = tempfile(); dir.create(d)
  (has_error(cache_rds(1, file = 'test.xyz', dir = paste0(d, '/'))))
  unlink(d, recursive = TRUE)
})

assert('cache_dir() returns a character string', {
  d = cache_dir()
  (is.character(d))
  (nchar(d) > 0)
})

assert('find_locals() finds assigned variables', {
  (find_locals('x = 1; y = 2') %==% c('x', 'y'))
  (find_locals('for (i in 1:10) {}') %==% 'i')
  (find_locals('x = 1 +') %==% character(0))
})

assert('find_globals() handles language input', {
  e = new.env(parent = emptyenv())
  e$z = 99
  (find_globals(quote(y <- z + 1), e) %==% 'z')
})
