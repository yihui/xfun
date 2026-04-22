library(testit)

yaml = '
a: 1
b: [1, 2, 3]
c: true
d:
  e: !expr 1+1
  f: null
'

yaml_long_indent = '
a: 1
b: [1, 2, 3]
c: true
d:
    e: !expr 1+1
    f: null
'


yaml_tabs = '
a: 1
b: [1, 2, 3]
c: true
d:
\te: !expr 1+1
\tf: null
'

# see https://github.com/yihui/xfun/issues/94
yaml_mre = '
output:
    latex:
        latex_engine: pdflatex
        options:
            toc: true
'


if (loadable('yaml')) assert('yaml_load() works with the yaml package', {
  (yaml_load(yaml) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (yaml_load(yaml, envir = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
  f = function() {
    foo = 1:10
    yaml_load('a: !expr head(foo, 4)')
  }
  (f() %==% list(a = 1:4))
})

assert('taml_load() works', {
  (taml_load(yaml) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (taml_load(yaml, envir = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
  f = function() {
    foo = 1:10
    taml_load('a: !expr head(foo, 4)')
  }
  (f() %==% list(a = 1:4))
  res = list(a = c('A', 'B', 'C D'))
  (taml_load('a: [A, B, C D]') %==% res)
  (taml_load('a: ["A", \'B\', "C D"]') %==% res)
  (taml_load("a: ['A', 'B', 'C D']") %==% res)
})

assert('taml_load() works with variable indent', {
  (taml_load(yaml_long_indent) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (taml_load(yaml_long_indent, envir = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
})

assert('taml_load() works with tabs indent', {
  (taml_load(yaml_tabs) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (taml_load(yaml_tabs, envir = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
})

assert('taml_load() works with non-uniform indent (varying indent sizes)', {
  x = 'runs:
  steps:
      name: Install R
      with:
        use-public-rspm: true'
  res = taml_load(x)
  (res %==% list(runs = list(steps = list(
    name = 'Install R', with = list(`use-public-rspm` = TRUE)
  ))))
})

assert('yaml_load() works with a complex output', {
  expected = list(
    output = list(
      latex = list(
        latex_engine = "pdflatex", options = list(toc = TRUE)
      )
    )
  )

  (yaml_load(yaml_mre) %==% expected)
  (yaml_load(yaml_mre, use_yaml = FALSE) %==% expected)
})

assert('taml_load() handles empty input', {
  (taml_load('') %==% list())
  (taml_load(character(0)) %==% list())
  (taml_load('# comment only') %==% list())
})

assert('taml_load() handles null value in nested key', {
  # tests null value handling in nested keys
  x = 'a:\n  b: null\n  c: 1'
  res = taml_load(x)
  (is.null(res[['a']][['b']]))
  (res[['a']][['c']] %==% 1L)
  # top-level null
  (taml_load('a: null') %==% list(a = NULL))
})

assert('yaml_value() returns float for non-integer numerics', {
  res = taml_load('a: 1.5')
  (res %==% list(a = 1.5))
  (is.double(res$a))
})

assert('taml_file() reads YAML from a file', {
  f = tempfile()
  writeLines('a: 1\nb: foo', f)
  res = taml_file(f)
  (res %==% list(a = 1L, b = 'foo'))
  unlink(f)
})

assert('taml_save() converts a list to YAML', {
  res = list(a = 1L, b = 'foo', c = TRUE, d = list(e = 2L, f = NULL))
  yaml_str = as.character(taml_save(res))
  (any(grepl('a: 1', yaml_str)))
  (any(grepl('b: "foo"', yaml_str)))
  (any(grepl('c: true', yaml_str)))
  (any(grepl('f: null', yaml_str)))
  # round-trip
  (taml_load(yaml_str) %==% res)
})

assert('taml_save() supports custom indent', {
  res = list(a = list(b = 1L))
  yaml_str = as.character(taml_save(res, indent = '\t'))
  (grepl('\t', yaml_str))
})

assert('taml_save() errors on unsupported types', {
  (has_error(taml_save(list(a = list(1, 2)))))  # unnamed sub-list
  (has_error(taml_save(list(a = raw(1)))))  # unsupported raw type
})

assert('taml_save() handles expressions and arrays', {
  # expression value
  yaml_expr = as.character(taml_save(list(a = expression(1 + 1))))
  (any(grepl('!expr', yaml_expr)))
  # vector value -> array syntax
  yaml_arr = as.character(taml_save(list(a = c(1L, 2L, 3L))))
  (any(grepl('^a: \\[', yaml_arr)))
  # AsIs scalar -> still array
  yaml_asis = as.character(taml_save(list(a = I(1L))))
  (any(grepl('^a: \\[', yaml_asis)))
})

assert('taml_save() writes to a file when path is provided', {
  res = list(a = 1L, b = 'foo')
  f = tempfile()
  taml_save(res, path = f)
  (file.exists(f))
  (taml_load(read_utf8(f)) %==% res)
  unlink(f)
})

assert('yaml_body() splits YAML front matter from body', {
  doc = c('---', 'title: Hello', 'output: html', '---', '', 'Content here.')
  res = yaml_body(doc)
  (res$yaml %==% list(title = 'Hello', output = 'html'))
  (res$body %==% c('', '', '', '', '', 'Content here.'))
  (res$lines %==% c(1L, 4L))
  # no YAML
  doc2 = c('# heading', 'text')
  res2 = yaml_body(doc2)
  (is.null(res2$yaml))
  (res2$body %==% doc2)
  # yaml_body with parse=FALSE
  res3 = yaml_body(doc, parse = FALSE)
  (res3$yaml %==% c('title: Hello', 'output: html'))
})
