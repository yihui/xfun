library(testit)

assert('prose_index() works', {
  x = c('a', '```', 'b', '```', 'c')
  out = c(1L, 5L)
  (prose_index(x) %==% out)

  x = c('a', '````', '```r', '1+1', '```', '````', 'c')
  out = c(1L, 7L)
  (prose_index(x) %==% out)

  x = c('a', '``', 'b', '``', 'c')
  out = seq_along(x)
  (prose_index(x) %==% out)

  # a character vector of length zero
  x = character()
  out = integer()
  (prose_index(x) %==% out)

  # one backbrick
  x = c('`', 'a', '`')
  out = seq_along(x)
  (prose_index(x) %==% out)

  # two backbrick
  x = c('``', 'a', '``')
  out = seq_along(x)
  (prose_index(x) %==% out)

  # no code fences
  x = c('a', 'b')
  out = c(1L, 2L)
  (prose_index(x) %==% out)

  # two code fences
  x = c('```', 'b', '```', '```', 'd', '```')
  out = integer()
  (prose_index(x) %==% out)

  # if the code fences are not balanced
  x = c('a', '```', 'b', '``', 'c')
  out = seq_along(x)
  (has_warning(prose_index(x)))
  (prose_index(x) %==% out)
})


assert('protect_math() puts inline math expressions in backticks', {
  (protect_math('$x$') %==% '`\\(x\\)`')
  (protect_math('hi $x$ a') %==% 'hi `\\(x\\)` a')
  (protect_math('$ a $') %==% '$ a $')
  (protect_math(' $a$') %==% ' `\\(a\\)`')
  (protect_math('$ x$') %==% '$ x$')
  (protect_math('$x $') %==% '$x $')
  (protect_math('b$a$') %==% 'b$a$')  # no space before $; unlikely to be math
  (protect_math('`$a$`') %==% '`$a$`')
  (protect_math('hi $x$9') %==% 'hi $x$9')
  (protect_math('$500 $600') %==% '$500 $600')

  (protect_math('$$a$$') %==% '`$$a$$`')
  (protect_math('$$a$') %==% '$$a$')
  (protect_math('hi $$\alpha$$') %==% 'hi `$$\alpha$$`')
  (protect_math('hi $$\alpha $$') %==% 'hi $$\alpha $$')
  (protect_math('hi $$ \alpha$$') %==% 'hi $$ \alpha$$')
  (protect_math('hi $$\alpha$$ and $$ \alpha$$') %==% 'hi `$$\alpha$$` and $$ \alpha$$')
})

