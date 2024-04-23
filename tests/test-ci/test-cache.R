library(testit)

assert('find_globals() identifies global variables', {
  # nothing from outside environment
  (find_globals('x=1') %==% character(0))
  # qwer must be created outside somewhere
  (find_globals('a=1; b=a; d=qwer') %==% 'qwer')
  (find_globals('a=function(){f=2;g}') %==% 'g')
  # y was assigned locally in z, but there is another y outside from nowhere
  (find_globals('z=function(){y=1};y') %==% 'y')
  # more complicated cases: operators, subscripts, ...
  (find_globals(c('a=1%*%1%o%2 %in% d', 'b=d%%10+3%/%2-z[1:3]')) %==% c('d', 'z'))
})
