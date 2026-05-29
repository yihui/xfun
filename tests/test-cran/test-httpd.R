library(testit)

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

assert('resolve_path() classifies files, dirs, redirects, and 404s', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines('hello', file.path(d, 'a.txt'))
  writeLines('<h1>Index</h1>', file.path(d, 'index.html'))
  dir.create(file.path(d, 'sub'))
  writeLines('sub-content', file.path(d, 'sub', 'b.txt'))

  in_dir(d, {
    # root with index.html
    r = resolve_path('.')
    (r$action %==% 'file')
    (basename(r$path) %==% 'index.html')

    # explicit file
    r = resolve_path('a.txt')
    (r$action %==% 'file')
    (basename(r$path) %==% 'a.txt')

    # directory without trailing slash → caller redirects
    r = resolve_path('sub')
    (r$action %==% 'add-slash')

    # directory listing (trailing slash, no index.html)
    r = resolve_path('sub/')
    (r$action %==% 'payload')
    (r$mime %==% 'text/html')
    (grepl('Index of', r$body))
    (grepl('b.txt', r$body))

    # missing file gives 404 payload
    r = resolve_path('missing.txt')
    (r$action %==% 'payload')
    (r$status %==% 404L)
    (grepl('Not found', r$body))

    # favicon.ico falls back to R's built-in
    r = resolve_path('favicon.ico')
    (r$action %==% 'file')
    (basename(r$path) %==% 'favicon.ico')
    (r$mime %==% 'image/x-icon')

    # custom 404.html → caller redirects
    writeLines('<h1>404</h1>', '404.html')
    r = resolve_path('nope.html')
    (r$action %==% '404-redirect')
  })
})

assert('.resolve_path_to_httpd() maps to R httpd response shape', {
  d = tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines('hi', file.path(d, 'a.txt'))
  dir.create(file.path(d, 'sub'))

  in_dir(d, {
    # file response
    r = .resolve_path_to_httpd('a.txt')
    (basename(r$file) %==% 'a.txt')

    # add-slash → 301 with Location header
    r = .resolve_path_to_httpd('sub')
    (r$`status code` %==% 301L)
    (grepl('^Location: /sub/$', r$header))

    # 404 payload
    r = .resolve_path_to_httpd('missing.txt')
    (r$`status code` %==% 404L)
    (r$`content-type` %==% 'text/plain')

    # 404.html redirect
    writeLines('x', '404.html')
    r = .resolve_path_to_httpd('nope.html')
    (r$`status code` %==% 302L)
    (r$header %==% 'Location: /404.html')
  })
})
