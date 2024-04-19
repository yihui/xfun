library(testit)

assert('prose_index() works', {
  x = c('a', '```', 'b', '```', 'c')
  (prose_index(x) %==% c(1L, 5L))

  x = c('a', '````', '```r', '1+1', '```', '````', 'c')
  (prose_index(x) %==% c(1L, 7L))

  x = c('a', '``', 'b', '``', 'c')
  (prose_index(x) %==% seq_along(x))

  # a character vector of length zero
  (prose_index(character()) %==% integer())

  # one backbrick
  x = c('`', 'a', '`')
  (prose_index(x) %==% seq_along(x))

  # two backbrick
  x = c('``', 'a', '``')
  (prose_index(x) %==% seq_along(x))

  # no code fences
  x = c('a', 'b')
  (prose_index(x) %==% seq_along(x))

  # two code fences
  x = c('```', 'b', '```', '```', 'd', '```')
  (prose_index(x) %==% integer())

  # code fences commented out
  x = c('```', 'b', '```', '<!--```', 'd', '```-->')
  (prose_index(x) %==% 4:6)

  # if the code fences are not balanced
  x = c('a', '```', 'b', '``', 'c')
  (has_warning(prose_index(x)))
  (prose_index(x) %==% seq_along(x))

  # recognize ``` ``text`` ```
  x = c('a', '``` md', 'b', '```', '``` ``text`` ```')
  (prose_index(x) %==% c(1L, 5L))

  # <pre> should also be treated as code blocks
  x = c('<pre><code>```', '```', '```', '</code></pre>')
  (prose_index(x) %==% integer())
})


assert('protect_math() puts inline math expressions in backticks', {
  (protect_math('$x$') %==% '`\\(x\\)`')
  (protect_math('$x$', '===') %==% '`===\\(x\\)===`')
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


assert('block_attr(x) turns a character vector into Pandoc attributes', {
  (block_attr(NULL) %==% '')
  (block_attr('.a') %==% ' {.a}')
  (block_attr('.a b="11"') %==% ' {.a b="11"}')
  (block_attr(c('.a', 'b="11"')) %==% ' {.a b="11"}')
})

assert('make_fence() uses the right number of fence characters', {
  (make_fence('1+1') %==% '```')
  (make_fence(c('1+1', '`````')) %==% '``````')
  (make_fence(':::') %==% '```')
  (make_fence(':::', ':') %==% '::::')
})

assert('fenced_block() wraps content inside fences', {
  (fenced_block('1+1') %==% c('', '```', '1+1', '```'))
  (fenced_block('<i>info</i>', '=html') %==% c('', '``` {=html}', '<i>info</i>', '```'))
  (fenced_block('1+1', c('.lang', '#id', 'foo="BAR"')) %==% c('', '``` {.lang #id foo="BAR"}', '1+1', '```'))
})
