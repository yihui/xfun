library(testit)

assert('strict_list() is really strict', {
  s_list = strict_list(aaa = 1:3, bbb = c('hey', 'mom'))
  # the class name better not be changed in the future
  (inherits(s_list, 'xfun_strict_list'))
  # `$` returns the expected value if the name provided is complete
  (s_list$aaa %==% 1:3)
  # but partial match is prohibited
  (s_list$a %==% NULL)
  # `[` will return a list - will it be better if returns a strict list?
  (inherits(s_list['aaa'], 'list'))
  # and add an element won't change the class by accident
  s_list$ccc = 4
  s_list$ddd = 'abcd'
  (inherits(s_list, 'xfun_strict_list'))
})

assert('as_strict_list() converts a list to a strict list', {
  normal_list = list(aaa = 1:3, bbb = c('hey', 'mom'))
  s_list = as_strict_list(normal_list)
  # does the strict list have the same length as the normal list?
  (length(normal_list) %==% length(s_list))
  # is the converted strict list equal to the same object created by `strict_list()`?
  s_list = strict_list(aaa = 1:3, bbb = c('hey', 'mom'))
  (as_strict_list(normal_list) %==% s_list)
})

assert('print() returns the same output for strict and normal list', {
  normal_list = list(aaa = 1:2, bbb = c('hey', 'dad'))
  s_list = as_strict_list(normal_list)
  (capture.output(print(normal_list)) %==% capture.output(print(s_list)))
})

assert('raw_string() prints as expected', {
  rs = raw_string(c('a "b"', 'hello\tworld!'))
  (inherits(rs, 'xfun_raw_string'))
  output = capture.output(rs)
  (output %==% c('a "b"', 'hello\tworld!'))
})

assert('raw_string() returns 0-length xfun_raw_string when input is NULL', {
  rs1 = raw_string(NULL)
  (inherits(rs1, 'xfun_raw_string'))
  (length(rs1) == 0)
  (capture.output(rs1) %==% character(0))

  # string 'NULL' is treated appropriately
  rs2 = raw_string('NULL')
  (inherits(rs2, 'xfun_raw_string'))
  (length(rs2) == 1)
  (capture.output(rs2) %==% 'NULL')
})

assert('raw_string() inherits from character', {
  rs = raw_string(c('a "b"', 'hello\tworld!'))
  (inherits(rs, 'character'))
})

assert('radix_id() encodes combinations as numeric codes', {
  (radix_id(c('b', 'a', 'b', 'a', 'c')) %==% c(2, 1, 2, 1, 3))
  # two vectors: combinations matched row-wise
  (radix_id(c(1, 1, 2), c('x', 'x', 'x')) %==% c(3, 3, 5))
  # NA maps to code 0 (its own group)
  (radix_id(c(1, NA, 1, NA)) %==% c(1, 0, 1, 0))
})

assert('radix_group() returns 1-based group index in first-appearance order', {
  # single vector
  (radix_group(c('b', 'a', 'b', 'a', 'c')) %==% c(1L, 2L, 1L, 2L, 3L))
  # two vectors
  (radix_group(c(1, 1, 2, 2), c('x', 'y', 'x', 'y')) %==% 1:4)
  (radix_group(c(1, 1, 1, 2), c('x', 'x', 'y', 'x')) %==% c(1L, 1L, 2L, 3L))
  # NA is its own group; factors work
  (radix_group(c(1, NA, 1, NA)) %==% c(1L, 2L, 1L, 2L))
  (radix_group(c(NA, NA)) %==% c(1L, 1L))
  (radix_group('a') %==% 1L)
  f = factor(c('b', 'a', 'b'), levels = c('a', 'b'))
  (radix_group(f) %==% c(1L, 2L, 1L))
  # expand a per-group result back to the full row order
  x = c(3, 1, 3, 2, 1)
  gid = radix_group(x)
  (x[!duplicated(gid)][gid] %==% x)
})
