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
  new_app(name, .serve_dir_handler(dir), ...)
}

# Build a handler for new_app() that serves files under `dir`. Returns a
# function with the new_app() handler signature: function(path, query, post,
# headers). The response shape is R's internal httpd shape (a list with
# `payload` or `file`, `content-type`, and optionally `header`/`status code`).
.serve_dir_handler = function(dir) {
  function(path, query = NULL, post = NULL, headers = NULL) {
    in_dir(dir, .serve_path(path))
  }
}

# Resolve a request path under the current working directory and build a
# response. `path` is the request path relative to the served directory; the
# caller must have already chdir-ed to that directory.
.serve_path = function(path) {
  if (path == '' || path == '/') path = '.'
  if (file_test('-d', path)) {
    # ensure a trailing slash so relative links inside resolve correctly
    if (path != '.' && !grepl('/$', path)) return(redirect(sprintf('%s/', path)))
    idx = file.path(path, 'index.html')
    if (file_exists(idx)) return(list(
      file = normalizePath(idx), `content-type` = mime_type(idx)
    ))
    title = html_escape(path)
    info = file.info(list.files(path, all.files = TRUE, full.names = TRUE))
    body = c(sprintf('<h1>Index of %s</h1>', title), fileinfo_table(info))
    return(list(
      payload = one_string(html_doc(body, title)), `content-type` = 'text/html'
    ))
  }
  if (!file_exists(path)) {
    if (path == 'favicon.ico') return(list(
      file = file.path(R.home('doc'), 'html', 'favicon.ico'),
      `content-type` = 'image/x-icon'
    ))
    # redirect to a custom 404.html if it exists and request looks like a page
    if (file_exists('404.html') && grepl('(/|[.]html)$', path, ignore.case = TRUE))
      return(redirect('/404.html', 302L))
    return(list(
      payload = paste('Not found:', path), `content-type` = 'text/plain',
      `status code` = 404L
    ))
  }
  list(file = normalizePath(path), `content-type` = mime_type(path))
}

# Build a response that redirects to `dest` with the given HTTP status.
redirect = function(dest, status = 301L) {
  list(
    payload = sprintf('Redirect to <a href="%s">%s</a>', dest, dest),
    `content-type` = 'text/html', header = paste0('Location: ', dest),
    `status code` = as.integer(status)
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
