library(testit)

assert('with_ext works for corner cases', {
  (with_ext(character(), 'abc') %==% character())
  (with_ext('abc', character()) %==% 'abc')
  (with_ext(NA_character_, 'abc') %==% NA_character_)
  (has_error(with_ext('abc', NA_character_)))
  (with_ext('abc', c('d', 'e')) %==% c('abc.d', 'abc.e'))
  (has_error(with_ext(c('a', 'b'), c('d', 'e', 'f'))))
  (with_ext(c('a', 'b'), c('d', 'e')) %==% c('a.d', 'b.e'))
  (with_ext(c('a', 'b'), c('d')) %==% c('a.d', 'b.d'))
  (with_ext(c('a', 'b', 'c'), c('', '.d', 'e.e')) %==% c('a', 'b.d', 'c.e.e'))
})

assert('same_path works', {
  (is.na(same_path('~/foo', NA_character_)))
  (is.na(same_path(NA_character_, '~/foo')))
  (same_path('~/foo', file.path(Sys.getenv('HOME'), 'foo')))
  (!same_path(tempdir(), 'foo'))
})

assert('url_filename() returns the file names in URLs', {
  (url_filename('https://yihui.org/images/logo.png') %==% 'logo.png')
  (url_filename(c(
    'https://yihui.org/index.html',
    'https://yihui.org/index.html?foo=bar',
    'https://yihui.org/index.html#about'
  )) %==% rep('index.html', 3))
})
