library(testit)

assert('n2w converts numbers to words', {
  (n2w(0) %==% 'zero')
  # cap capitalizes the first letter
  (n2w(0, cap = TRUE) %==% 'Zero')
  # hyphen adds '-' for 21-99 (except 30, 40, ...)
  (n2w(21, cap = TRUE, hyphen = TRUE) %==% 'Twenty-one')
  (n2w(21, cap = TRUE, hyphen = FALSE) %==% 'Twenty one')
  # x can be negative integers
  (n2w(-21, cap = TRUE, hyphen = TRUE) %==% 'Minus twenty-one')
  (n2w(-21, cap = TRUE, hyphen = FALSE) %==% 'Minus twenty one')
  # and controls whether to have 'add' between hundreds and double digits
  (n2w(121, and = FALSE) %==% 'one hundred twenty-one')
  (n2w(121, and = TRUE) %==% 'one hundred and twenty-one')
  # x can be a vector with length > 1
  (n2w(c(10, 13, 99, 1e6)) %==% c('ten', 'thirteen', 'ninety-nine', 'one million'))
  # the number should be less than 1e15
  (has_error(n2w(1e15)))
})

assert('split_lines() splits a character vector into lines', {
  (split_lines('a') %==% 'a')
  (split_lines('') %==% '')
  (split_lines(NULL) %==% NULL)
  (split_lines('a\n') %==% c('a', ''))
  (split_lines(c('a', 'b\nc')) %==% c('a', 'b', 'c'))
  (split_lines(c('', '\n')) %==% c('', '', ''))
  (split_lines('a\nb') %==% c('a', 'b'))
  (split_lines('a\nb\n\n') %==% c('a', 'b', '', ''))
  (split_lines(c('a\nb', '', ' ', 'c')) %==% c('a', 'b', '', ' ', 'c'))
})
