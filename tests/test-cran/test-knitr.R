library(testit)

assert('csv_options() parses chunk options to a list', {
  (csv_options('a-s-d,b=TRUE,c=def') %==% alist(label = 'a-s-d', b = TRUE, c = def))
  (csv_options('a,opt=c(1,3,5)') %==% alist(label = 'a', opt = c(1, 3, 5)))
  (csv_options('label="xx",opt=zz') %==% alist(label = 'xx', opt = zz))
  (csv_options('label=foo') %==% alist(label = 'foo'))
  (csv_options('a,b=2,c="qwer",asdf="efg"') %==%
      alist(label = 'a', b = 2, c = 'qwer', asdf = 'efg'))
  (csv_options('2a') %==% alist(label = '2a'))
  (csv_options('abc-function,fig.path="foo/bar-"') %==%
      alist(label = 'abc-function', fig.path = "foo/bar-"))
  (has_error(csv_options('a,b')))
  (has_error(csv_options('a,b,c=qwer')))
  # empty options string: returns an empty list (no label)
  (length(csv_options('')) %==% 0L)
  # trailing comma: empty option is removed, only named args kept
  res = csv_options('a, b=1,')
  (res$label %==% 'a')
  (res$b %==% 1)
  (length(res) %==% 2L)
  # invalid R syntax in options triggers error with a message
  (has_error(csv_options('a=1+')))
})

assert('divide_chunk() parses YAML-style chunk options', {
  yaml_like = c('#| label: mine', '#| echo: true', '#| fig.width: 8', '1 + 1')
  res = divide_chunk('r', yaml_like, use_yaml = FALSE)
  (res$options$label %==% 'mine')
  (isTRUE(res$options$echo))
  (res$options$fig.width %==% 8L)
  (res$code %==% '1 + 1')
})

assert('divide_chunk() parses CSV-style chunk options', {
  csv_like = c("#| mine, echo = TRUE, fig.width = 8", "1 + 1")
  res = divide_chunk('r', csv_like)
  (res$options$label %==% 'mine')
  (isTRUE(res$options$echo))
  (res$options$fig.width %==% 8)
  (res$code %==% '1 + 1')
})

assert('divide_chunk() returns empty options for empty code', {
  res = divide_chunk('r', character(0))
  (is.null(res$options))
  (length(res$code) %==% 0L)
})

assert('divide_chunk() returns code unchanged when no option comments', {
  code = c('x = 1', 'y = 2')
  res = divide_chunk('r', code)
  (is.null(res$options))
  (res$code %==% code)
})

assert('divide_chunk() falls back to #| for non-R engines', {
  # js uses //| but falls back to #| when strict=FALSE
  js_like = c('#| echo: true', 'console.log(1)')
  res = divide_chunk('js', js_like, use_yaml = FALSE)
  (isTRUE(res$options$echo))
  (res$code %==% 'console.log(1)')
  # with strict=TRUE, #| for a non-# engine is an error
  (has_error(divide_chunk('js', js_like, strict = TRUE, use_yaml = FALSE)))
})

assert('divide_chunk() handles block-comment engines (s2 != empty)', {
  # C engine uses /*| */ style: s2 != ''
  c_like = c('/*| echo: true */', 'int x = 1;')
  res = divide_chunk('c', c_like, use_yaml = FALSE)
  (isTRUE(res$options$echo))
  (res$code %==% 'int x = 1;')
})

assert('divide_chunk() strips leading blank line from code', {
  # a blank line immediately after the options block is removed from code
  yaml_like = c('#| echo: true', '', '1 + 1')
  res = divide_chunk('r', yaml_like, use_yaml = FALSE)
  (isTRUE(res$options$echo))
  (res$code %==% '1 + 1')  # blank line stripped
  (length(res$src) %==% 2L)  # blank line moved to src
})
