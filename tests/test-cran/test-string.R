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

assert('join_words() joins multiple words into a single string', {
  jw = function(...) unclass(join_words(...))
  (jw(NULL) %==% character())
  (jw(c('a')) %==% 'a')
  (jw(c('a', 'b')) %==% 'a and b')
  (jw(c('a', 'b'), and = "") %==% 'a, b')
  (jw(c('a', 'b', 'c')) %==% 'a, b, and c')
  (jw(c('a', 'b', 'c'), and = '') %==% 'a, b, c')
  (jw(c('a', 'b', 'c'), ' / ', '') %==% 'a / b / c')
  (jw(c('a', 'b', 'c'), before = '"') %==% '"a", "b", and "c"')
  (jw(c('a', 'b', 'c'), before = '``', after = "''") %==% "``a'', ``b'', and ``c''")
  (jw(c('a', 'b', 'c'), before = '``', after = "''", oxford_comma = FALSE) %==% "``a'', ``b'' and ``c''")
  rm(list = 'jw')
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

assert('split_source() returns list with input for empty input', {
  # empty input (length < 1) should return list(x) immediately
  (split_source(character(0)) %==% list(character(0)))
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

assert('str_wrap() wraps text and returns same-length output', {
  x = c('hello world foo bar baz', 'another long string here')
  res = str_wrap(x, width = 10)
  (length(res) %==% length(x))
  (is.character(res))
  # each element should contain newlines when wrapping was needed
  (grepl('\n', res[1]))
})

assert('decimal_dot() forces dot as the decimal separator', {
  old = options(OutDec = ',')
  r = decimal_dot(as.character(1.234))
  options(old)  # restore OutDec
  (r %==% '1.234')
})

assert('strip_blank() removes blank elements from both ends', {
  (strip_blank(character(0)) %==% character(0))
  (strip_blank(c('', 'a', 'b', '')) %==% c('a', 'b'))
  (strip_blank(c('', '  ', 'a', '', 'b', '', '')) %==% c('a', '', 'b'))
  (strip_blank(c('a', 'b')) %==% c('a', 'b'))
  (strip_blank(c('', '')) %==% character(0))
})

assert('query_params() creates URL query strings', {
  (query_params() %==% '')
  (query_params(a = 1, b = 'foo') %==% '?a=1&b=foo')
  (query_params(.list = list(x = 'bar')) %==% '?x=bar')
})

assert('pair_chars() checks balanced paired characters', {
  # balanced quotes
  x = c('He said \u201chello\u201d.', 'No quotes here.')
  (pair_chars(x) %==% x)
  # unbalanced quotes produce a warning
  (has_warning(pair_chars(c('\u201chello.'))))
  # wrong chars length errors
  (has_error(pair_chars('text', chars = c('\u201c'))))
  # file mode: file content unchanged (quotes are already balanced)
  f = tempfile()
  writeLines(c('\u201chello\u201d', '\u201cworld\u201d'), f)
  pair_chars(file = f)
  (read_utf8(f) %==% c('\u201chello\u201d', '\u201cworld\u201d'))
  # file mode: content changed (mismatched quotes get replaced)
  f2 = tempfile()
  writeLines(c('\u201chello\u201d', '\u2018world\u2018'), f2)  # second line has wrong closing quote
  has_warning(pair_chars(file = f2))
  unlink(c(f, f2))
})

assert('html_content() resolves HTML content recursively', {
  # list with mixed html and plain text
  result = html_content(list('hello', html_value('<b>world</b>')))
  (result %==% c(html_escape('hello'), '<b>world</b>'))
})

assert('numbers_to_words() handles more cases', {
  # non-numeric input errors
  (has_error(n2w('abc')))
  # value >= 1e15 errors
  (has_error(n2w(1e15)))
  # 10-19 range (x_cs[1] == 1)
  (n2w(10) %==% 'ten')
  (n2w(15) %==% 'fifteen')
  (n2w(19) %==% 'nineteen')
  # x00 pattern (100, 200, ...) and x0x (101, 201, ...)
  (n2w(100) %==% 'one hundred')
  (n2w(200) %==% 'two hundred')
  (n2w(101) %==% 'one hundred one')
  # float with decimal part
  (n2w(1.5) %==% 'one point five')
  (n2w(3.14) %==% 'three point one four')
  # round tens like 30, 40, 50 (x_cs[2] == 0 branch in convert_2)
  (n2w(30) %==% 'thirty')
  (n2w(40) %==% 'forty')
  (n2w(50) %==% 'fifty')
})

assert('encrypt() and decrypt() are inverses', {
  key = 'fedcba9876543210'
  x = 'hello'
  (decrypt(encrypt(x, key), key) %==% x)
})
