library(testit)

assert('is_blank() tests if a vector is blank', {
  (is_blank(''))
  (is_blank(NULL) %==% logical(0))
  (is_blank(c('', '   ', '\n', '\t')))
  (!is_blank('abc'))
  (is_blank(c('', 'abc')) %==% c(TRUE, FALSE))
})

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

assert('split_source() puts lines of the same expression into a list element', {
  (split_source('1+1') %==% list('1+1'))
  (split_source(c('1+1+', '1')) %==% list(c('1+1+', '1')))
  (split_source(c('1+1+', '1', 'TRUE')) %==% list(c('1+1+', '1'), 'TRUE'))
  x = c('# a', '# b', '1', '# c', 'if (T)', 'F')
  (split_source(x) %==% c(as.list(x[1:4]), list(x[5:6])))
  (split_source(x, merge_comments = TRUE) %==% list(x[1:3], x[4:6]))
  x = c(x, '1+1')
  (split_source(x, TRUE, line_number = TRUE)[2:3] %==% list(
    structure(x[4:6], lines = c(4L, 6L)),
    structure(x[7], lines = c(7L, 7L))
  ))
})

assert('split_source() should signal an error for incomplete code', {
  (has_error(split_source('1+1+')))
  (has_error(split_source(c('1+1', '1+1+'))))
})

assert('valid_syntax() tells if a code fragment is syntactically valid', {
  (valid_syntax('1+1'))
  (!valid_syntax('1+1+'))
  (valid_syntax('if(TRUE)1'))
  (!valid_syntax(c('if(T){', 'F')))
  (valid_syntax(c('if(T){', 'F}')))
})

assert('alnum_id() generates ID strings', {
  x = c('Hello world 123!', 'a  &b*^##c 456')
  (alnum_id(x) %==% c('hello-world-123', 'a-b-c-456'))
  (alnum_id(x, '[^[:alpha:]]+') %==% c('hello-world', 'a-b-c'))
})
