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
