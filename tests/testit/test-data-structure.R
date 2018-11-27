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

assert('raw_string() prints as expected', {
  rs = raw_string(c('a "b"', 'hello\tworld!'))
  (inherits(rs, 'xfun_raw_string'))
  output = capture.output(rs)
  (output %==% c('a "b"', 'hello\tworld!'))
})
