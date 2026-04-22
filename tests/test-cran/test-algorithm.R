library(testit)

assert('rand_unit() returns values in [0, 1)', {
  x = rand_unit(100, seed = 42)
  (length(x) %==% 100L)
  (x >= 0)
  (x < 1)
})

assert('rand_unit() produces reproducible results with the same seed', {
  x1 = rand_unit(10, seed = 0)
  x2 = rand_unit(10, seed = 0)
  (x1 %==% x2)
})

assert('rand_unit() returns 0 values for n = 0', {
  (length(rand_unit(0, seed = 1)) %==% 0L)
})

assert('rand_n() returns integers in [1, M]', {
  x = rand_n(100, 6)
  (length(x) %==% 100L)
  (x >= 1)
  (x <= 6)
})

assert('md5() computes checksums', {
  (is.character(md5(1:10)))
  (nchar(md5(1:10)) %==% 32L)
  # x2 and x3 should be identical since they are the same integer sequence
  x2 = 1:10; x3 = seq(1, 10)
  (md5(x2) %==% md5(x3))
  # named arguments produce named results
  res = md5(a = 1, b = 'hello')
  (names(res) %==% c('a', 'b'))
})

assert('find_locals() identifies locally created variables', {
  (find_locals('y = x + 1') %==% 'y')
  ("y" %in% find_locals("assign('y', x + 1)"))
})

assert('cache_exec() returns correct result', {
  r = cache_exec({2 + 2}, path = ':memory:', id = 'xfun-test-result')
  (r %==% 4)
})

assert('cache_exec() uses cache on second call', {
  id = 'xfun-test-cache-hit'
  r1 = cache_exec({Sys.time()}, path = ':memory:', id = id)
  Sys.sleep(0.01)  # ensure time would differ if re-evaluated
  r2 = cache_exec({Sys.time()}, path = ':memory:', id = id)
  # r2 should come from cache, so they should be identical
  (r1 %==% r2)
})
