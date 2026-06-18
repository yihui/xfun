#' Serve a static directory
#'
#' Serve a directory of files via [new_app()]. If `index.html` exists under the
#' directory, it is shown; otherwise a directory listing is shown. This is
#' similar to `servr::httd()` and Python's `http.server` module.
#' @param dir The directory to serve.
#' @param name The app name passed to [new_app()]. Use `''` to serve the
#'   directory at the root URL via the proxy.
#' @param ... Other arguments passed to [new_app()].
#' @return The app URL, returned invisibly.
#' @export
#' @examplesIf interactive()
#' xfun::serve_dir(tempdir())
serve_dir = function(dir = '.', name = 'xfun-dir', ...) {
  dir = normalizePath(dir, mustWork = TRUE)
  handler = function(path, query = NULL, post = NULL, headers = NULL) {
    in_dir(dir, .resolve_path_to_httpd(path))
  }
  new_app(name, handler, ...)
}

# Resolve a request path under the current working directory and return a
# neutral intermediate response. This is the shared core used by both
# `xfun::serve_dir()` (mapped to R's internal httpd shape) and
# `servr:::serve_dir()` (mapped to the httpuv shape). The `action` field is
# one of:
#   'file'         — list(action, path = <abs>, mime)
#   'payload'      — list(action, body = <text>, mime, status)
#   'add-slash'    — caller redirects to its own request URL + '/'
#   '404-redirect' — caller redirects to '/404.html' (if such a file exists)
resolve_path = function(path) {
  if (path == '' || path == '/') path = '.'
  if (file_test('-d', path)) {
    # ensure a trailing slash so relative links inside the dir resolve correctly
    if (path != '.' && !grepl('/$', path)) return(list(action = 'add-slash'))
    idx = file.path(path, 'index.html')
    if (file_exists(idx))
      return(list(action = 'file', path = normalizePath(idx), mime = mime_type(idx)))
    title = html_escape(path)
    info = file.info(list.files(path, all.files = TRUE, full.names = TRUE))
    body = one_string(html_doc(
      c(sprintf('<h1>Index of %s</h1>', title), fileinfo_table(info)), title
    ))
    return(list(action = 'payload', body = body, mime = 'text/html', status = 200L))
  }
  if (!file_exists(path)) {
    # callers may pass the request path with a leading './' (e.g. servr's
    # serve_dir()), so match on the basename instead of the exact string
    if (basename(path) == 'favicon.ico') return(list(
      action = 'file', path = file.path(R.home('doc'), 'html', 'favicon.ico'),
      mime = 'image/x-icon'
    ))
    # redirect to a custom 404.html if it exists and the request looks like a page
    if (file_exists('404.html') && grepl('(/|[.]html)$', path, ignore.case = TRUE))
      return(list(action = '404-redirect'))
    return(list(
      action = 'payload', body = paste('Not found:', path),
      mime = 'text/plain', status = 404L
    ))
  }
  list(action = 'file', path = normalizePath(path), mime = mime_type(path))
}

# Map the neutral response to R's internal httpd response shape.
.resolve_path_to_httpd = function(path) {
  r = resolve_path(path)
  redir = function(dest, status) list(
    payload = sprintf('Redirect to <a href="%s">%s</a>', dest, dest),
    `content-type` = 'text/html', header = paste0('Location: ', dest),
    `status code` = as.integer(status)
  )
  switch(
    r$action,
    file = list(file = r$path, `content-type` = r$mime),
    payload = list(payload = r$body, `content-type` = r$mime, `status code` = r$status),
    `add-slash` = redir(sprintf('%s/', sub('^/?', '/', path)), 301L),
    `404-redirect` = redir('/404.html', 302L)
  )
}

# Render a file.info() data frame as an HTML directory listing.
fileinfo_table = function(info) {
  info = info[order(info$isdir, decreasing = TRUE), ]
  d = info$isdir; i = !is.na(d)
  x1 = paste0(basename(rownames(info)), ifelse(d & i, '/', ''))
  x1 = html_escape(x1)
  x1[i] = sprintf('<a href="%s">%s</a>', x1[i], x1[i])
  x2 = paste(format(info$size, scientific = FALSE, big.mark = ','), 'B')
  x2[is.na(info$size) | d] = ''
  x3 = as.character(info$mtime); x3[is.na(x3)] = ''
  c('<table>', '<thead><tr>',
    sprintf('<th>%s</th>', c('Name', 'Size', 'Date Modified')),
    '</tr></thead>',
    apply(cbind(
      '<tr>', sprintf('<td>%s</td>', x1),
      sprintf('<td align="right">%s</td>', x2),
      sprintf('<td>%s</td>', x3), '</tr>'
    ), 1, paste, collapse = ''),
    '</table>')
}

# Wrap an HTML body in a minimal HTML document with the given title.
html_doc = function(body, title = NULL) {
  c('<!DOCTYPE html>', '<html>',
    '<head>', sprintf('<title>%s</title>', title), '</head>',
    '<body>', body, '</body>', '</html>')
}
