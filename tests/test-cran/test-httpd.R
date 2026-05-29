library(testit)

assert('redirect() builds a redirect response', {
  r = redirect('/foo/')
  (r$`status code` %==% 301L)
  (r$header %==% 'Location: /foo/')
  (r$`content-type` %==% 'text/html')
  r2 = redirect('/404.html', 302L)
  (r2$`status code` %==% 302L)
  (r2$header %==% 'Location: /404.html')
})

assert('html_doc() wraps content in an HTML document', {
  x = html_doc('<p>hi</p>', 'My Title')
  (any(grepl('<!DOCTYPE html>', x)))
  (any(grepl('<title>My Title</title>', x)))
  (any(grepl('<p>hi</p>', x)))
})

assert('fileinfo_table() builds a directory listing table', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines('a', file.path(d, 'a.txt'))
  dir.create(file.path(d, 'sub'))
  info = file.info(list.files(d, all.files = TRUE, full.names = TRUE))
  tbl = fileinfo_table(info)
  (any(grepl('<table>', tbl)))
  (any(grepl('a.txt', tbl)))
  # directories get a trailing slash
  (any(grepl('sub/', tbl)))
})

assert('.serve_path() serves files, dirs, and 404s', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines('hello', file.path(d, 'a.txt'))
  writeLines('<h1>Index</h1>', file.path(d, 'index.html'))
  dir.create(file.path(d, 'sub'))
  writeLines('sub-content', file.path(d, 'sub', 'b.txt'))

  in_dir(d, {
    # root with index.html
    r = .serve_path('.')
    (basename(r$file) %==% 'index.html')

    # explicit file
    r = .serve_path('a.txt')
    (basename(r$file) %==% 'a.txt')

    # directory without trailing slash redirects
    r = .serve_path('sub')
    (r$`status code` %==% 301L)
    (r$header %==% 'Location: sub/')

    # directory listing (trailing slash, no index.html)
    r = .serve_path('sub/')
    (r$`content-type` %==% 'text/html')
    (grepl('Index of', r$payload))
    (grepl('b.txt', r$payload))

    # missing file gives 404
    r = .serve_path('missing.txt')
    (r$`status code` %==% 404L)
    (grepl('Not found', r$payload))

    # favicon.ico falls back to R's built-in
    r = .serve_path('favicon.ico')
    (basename(r$file) %==% 'favicon.ico')
    (r$`content-type` %==% 'image/x-icon')

    # custom 404.html redirect for page-like requests
    writeLines('<h1>404</h1>', '404.html')
    r = .serve_path('nope.html')
    (r$`status code` %==% 302L)
    (r$header %==% 'Location: /404.html')
  })
})

assert('.serve_dir_handler() resolves paths under the served dir', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines('hi', file.path(d, 'a.txt'))
  h = .serve_dir_handler(normalizePath(d))
  r = h('a.txt')
  (basename(r$file) %==% 'a.txt')
  # query/post/headers are accepted but ignored for static serving
  r = h('a.txt', query = c(foo = 'bar'), post = NULL, headers = NULL)
  (basename(r$file) %==% 'a.txt')
})
