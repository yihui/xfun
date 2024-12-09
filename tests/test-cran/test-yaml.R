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

if (loadable('yaml')) assert('yaml_load() works with the yaml package', {
  (yaml_load(yaml) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (yaml_load(yaml, envir = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
  # this test won't work for yaml::yaml.load() but works for yaml_load()
  f = function() {
    foo = 1:10
    yaml_load('a: !expr head(foo, 4)')
  }
  (f() %==% list(a = 1:4))
})

assert('yaml_load() works without the yaml package', {
  (yaml_load(yaml, use_yaml = FALSE) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (yaml_load(yaml, envir = FALSE, use_yaml = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
  # this test won't work for yaml::yaml.load() but works for yaml_load()
  f = function() {
    foo = 1:10
    yaml_load('a: !expr head(foo, 4)', use_yaml = FALSE)
  }
  (f() %==% list(a = 1:4))
})


assert('yaml_load() works with variable indent', {
  (yaml_load(yaml_long_indent, use_yaml = FALSE) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (yaml_load(yaml_long_indent, envir = FALSE, use_yaml = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
})


assert('yaml_load() works with tabs indent', {
  (yaml_load(yaml_tabs, use_yaml = FALSE) %==% list(a = 1L, b = 1:3, c = TRUE, d = list(e = 2, f = NULL)))
  (yaml_load(yaml_tabs, envir = FALSE, use_yaml = FALSE)[[c('d', 'e')]] %==% expression(1 + 1))
})
