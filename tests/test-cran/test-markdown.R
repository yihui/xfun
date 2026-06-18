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
  (protect_math('($x$)') %==% '(`\\(x\\)`)')
  (protect_math(' $x$') %==% ' `\\(x\\)`')
  (protect_math('.$x$') %==% '.$x$')
  (protect_math('$`x`$') %==% '$`x`$')
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
  (protect_math('($$a$$)') %==% '(`$$a$$`)')
  (protect_math(' $$a$$') %==% ' `$$a$$`')
  (protect_math('.$$a$$') %==% '.$$a$$')
  (protect_math('hi $$\alpha$$') %==% 'hi `$$\alpha$$`')
  (protect_math('hi $$\alpha $$') %==% 'hi $$\alpha $$')
  (protect_math('hi $$ \alpha$$') %==% 'hi $$ \alpha$$')
  (protect_math('hi $$\alpha$$ and $$ \alpha$$') %==% 'hi `$$\alpha$$` and $$ \alpha$$')
  (protect_math(c('$$a', 'b$$')) %==% c('`$$a', 'b$$`'))
  (protect_math(c('$$a', 'b$$'), 'asdf') %==% c('`asdf$$a', 'b$$asdf`'))
  (protect_math(c('$$a', 'b$$'), use_block = TRUE) %==% c('```{.md-math}\n$$a', 'b$$\n```'))
  (protect_math(c('$$a', 'b$$'), 'asdf', use_block = TRUE) %==% c('```{.md-math .asdf}\n$$a', 'b$$\n```'))
  (protect_math(c('  $$a', '  b$$'), use_block = TRUE) %==% c('  ```{.md-math}\n  $$a', '  b$$\n  ```'))
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

assert('fenced_div() uses colons as fence character', {
  res = fenced_div('content')
  (res %==% c('', ':::', 'content', ':::'))
})

assert('md_table() generates a Markdown pipe table', {
  # basic usage
  x = data.frame(a = 1:3, b = c('x', 'y', 'z'), stringsAsFactors = FALSE)
  res = md_table(x)
  (length(res) %==% 5L)  # header + separator + 3 rows
  (grepl('^\\|', res[1]))  # starts with pipe
  (grepl('\\|$', res[1]))  # ends with pipe
  # numeric column is right-aligned
  (grepl('--:', res[2]))

  # empty columns
  (md_table(data.frame()) %==% character())

  # error for non-2D objects
  (has_error(md_table(1:5)))

  # NA values in table
  x2 = data.frame(a = c(1, NA, 3), b = c('x', 'y', NA))
  res2 = md_table(x2)
  (any(grepl('NA', res2) | grepl('', res2)))  # NA shown as empty by default

  # row names (non-numeric) are included as first column
  x3 = data.frame(a = 1:2, row.names = c('r1', 'r2'))
  res3 = md_table(x3)
  (grepl('r1', res3[3]))  # first data row (after header + separator) contains r1

  # limit parameter
  x4 = data.frame(a = 1:10)
  res4 = md_table(x4, limit = 4)
  (length(res4) < 12L)  # fewer rows than full table
})

assert('fenced_div() uses colons and supports attributes', {
  (fenced_div('text', '.note') %==% c('', '::: {.note}', 'text', ':::'))
})

assert('md_table() with newlines, NA, digits, and pipes', {
  x = data.frame(a = 'line1\nline2', stringsAsFactors = FALSE)
  (!any(grepl('\n', md_table(x))))
  x = data.frame(a = c(1, NA))
  (any(grepl('N/A', md_table(x, na = 'N/A'))))
  x = data.frame(a = 3.14159265)
  res = md_table(x, digits = 2)
  (any(grepl('3.14', res)))
  (!any(grepl('3.1415', res)))
  x = data.frame(a = 'a|b', stringsAsFactors = FALSE)
  (any(grepl('\\\\[|]', md_table(x))))
})

assert('md_table() escapes special Markdown characters', {
  x = data.frame(a = '<TaskRegr:mtcars[0]>', stringsAsFactors = FALSE)
  res = md_table(x, escape = TRUE)
  (any(grepl('\\\\<', res)))
  (any(grepl('\\\\\\[', res)))
  # escape by column name
  x = data.frame(a = '*bold*', b = '*bold*', stringsAsFactors = FALSE)
  res = md_table(x, escape = 'a')
  (any(grepl('\\\\[*]', res)))
  # column b should not be escaped
  (grepl('\\*bold\\*\\|$', res[3]))
  # escape = FALSE (default) does not escape
  (!any(grepl('\\\\<', md_table(data.frame(a = '<x>'), escape = FALSE))))
})

assert('md_table() with column limit', {
  x = as.data.frame(matrix(1:20, nrow = 2))
  (any(grepl('[.][.][.]', md_table(x, limit = c(0, 4)))))
})

assert('md_table() with row limit shows ellipsis', {
  (any(grepl('vellip', md_table(data.frame(a = 1:20), limit = 4))))
})

assert('protect_math() does not touch code blocks', {
  (protect_math(c('```', '$x$', '```')) %==% c('```', '$x$', '```'))
})

assert('protect_math() handles \\begin/\\end environments', {
  x = c('\\begin{equation}', 'x = y', '\\end{equation}')
  (protect_math(x) %==% c('`\\begin{equation}', 'x = y', '\\end{equation}`'))

  # nested environments: only outermost should be protected (#57)
  x = c('\\begin{equation}', '\\begin{cases}', 'x = y', '\\end{cases}', '\\end{equation}')
  (protect_math(x) %==% c('`\\begin{equation}', '\\begin{cases}', 'x = y', '\\end{cases}', '\\end{equation}`'))
})

assert('block_attr() works', {
  (block_attr('#myid') %==% ' {#myid}')
  (block_attr('lang') %==% ' lang')
})

assert('embed_file() produces an anchor tag with base64 data', {
  f = tempfile(fileext = '.txt'); writeLines('hello', f)
  link = embed_file(f)
  (inherits(link, 'xfun_html'))
  (grepl('<a ', link) && grepl('download=', link) && grepl('data:text/plain;base64,', link))
  unlink(f)
})

assert('embed_files() compresses multiple files into a zip link', {
  d = tempfile(); dir.create(d)
  f1 = file.path(d, 'a.txt'); writeLines('aaa', f1)
  f2 = file.path(d, 'b.txt'); writeLines('bbb', f2)
  link = embed_files(c(f1, f2))
  (inherits(link, 'xfun_html'))
  (grepl('\\.zip', link))
  unlink(d, recursive = TRUE)
})

assert('embed_dir() compresses a directory into a zip link', {
  d = tempfile(); dir.create(d)
  writeLines('aaa', file.path(d, 'file.txt'))
  link = embed_dir(d)
  (inherits(link, 'xfun_html'))
  (grepl('\\.zip', link))
  unlink(d, recursive = TRUE)
})
