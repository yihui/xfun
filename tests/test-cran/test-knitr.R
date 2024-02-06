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
})
