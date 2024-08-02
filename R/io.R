#' Read / write files encoded in UTF-8
#'
#' Read or write files, assuming they are encoded in UTF-8. `read_utf8()` is
#' roughly `readLines(encoding = 'UTF-8')` (a warning will be issued if non-UTF8
#' lines are found), and `write_utf8()` calls `writeLines(enc2utf8(text),
#' useBytes = TRUE)`.
#'
#' The function `append_utf8()` appends UTF-8 content to a file or connection
#' based on `read_utf8()` and `write_utf8()`, and optionally sort the content.
#' The function `append_unique()` appends unique lines to a file or connection.
#' @param con A connection or a file path.
#' @param error Whether to signal an error when non-UTF8 characters are detected
#'   (if `FALSE`, only a warning message is issued).
#' @param text A character vector (will be converted to UTF-8 via [enc2utf8()]).
#' @param ... Other arguments passed to [writeLines()] (except `useBytes`, which
#'   is `TRUE` in `write_utf8()`).
#' @return `read_utf8()` returns a character vector of the file content;
#'   `write_utf8()` returns the `con` argument (invisibly).
#' @export
read_utf8 = function(con, error = FALSE) {
  # users may have set options(encoding = 'UTF-8'), which usually won't help but
  # will bring more trouble than good, so we reset this option temporarily
  opts = options(encoding = 'native.enc'); on.exit(options(opts), add = TRUE)
  x = readLines(con, encoding = 'UTF-8', warn = FALSE)
  i = invalid_utf8(x)
  n = length(i)
  if (n > 0) (if (error) stop else warning)(
    if (is.character(con)) c('The file ', con, ' is not encoded in UTF-8. '),
    'These lines contain invalid UTF-8 characters: ',
    paste(c(head(i), if (n > 6) '...'), collapse = ', ')
  )
  x
}

#' @rdname read_utf8
#' @export
write_utf8 = function(text, con, ...) {
  if (is.null(text)) text = character(0)
  if (identical(con, '')) {
    cat(text, sep = '\n', file = con)
  } else {
    # prevent re-encoding the text in the file() connection in writeLines()
    # https://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
    opts = options(encoding = 'native.enc'); on.exit(options(opts), add = TRUE)
    writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
  }
  invisible(con)
}

#' @param sort Logical (`FALSE` means not to sort the content) or a
#'   function to sort the content; `TRUE` is equivalent to
#'   `base::sort`.
#' @rdname read_utf8
#' @export
append_utf8 = function(text, con, sort = TRUE) {
  x = read_utf8(con, error = TRUE)
  x = c(x, text)
  if (is.logical(sort)) sort = if (sort) base::sort else identity
  if (is.function(sort)) x = sort(x)
  write_utf8(x, con)
}

#' @rdname read_utf8
#' @export
append_unique = function(text, con, sort = function(x) base::sort(unique(x))) {
  append_utf8(text, con, sort)
}

# which lines are invalid UTF-8
invalid_utf8 = function(x) {
  which(!is_utf8(x))
}

test_utf8 = function(x) {
  is.na(x) | !is.na(iconv(x, 'UTF-8', 'UTF-8'))
}

# validUTF8() was added to base R 3.3.0
is_utf8 = function(x) {
  if ('validUTF8' %in% ls(baseenv())) validUTF8(x) else test_utf8(x)
}

#' Read a text file and concatenate the lines by `'\n'`
#'
#' The source code of this function should be self-explanatory.
#' @param file Path to a text file (should be encoded in UTF-8).
#' @return A character string of text lines concatenated by `'\n'`.
#' @export
#' @examples
#' xfun::file_string(system.file('DESCRIPTION', package = 'xfun'))
file_string = function(file) {
  x = read_utf8(file)
  # paste converts 0-length character() into 1-length ""
  if (length(x)) x = paste(x, collapse = '\n')
  raw_string(x)
}

#' Read all records of a binary file as a raw vector by default
#'
#' This is a wrapper function of [readBin()] with default arguments
#' `what = "raw"` and `n = `[`file.size`]`(file)`, which means it
#' will read the full content of a binary file as a raw vector by default.
#' @param file,what,n,... Arguments to be passed to `readBin()`.
#' @return A vector returned from `readBin()`.
#' @export
#' @examples
#' f = tempfile()
#' cat('abc', file = f)
#' xfun::read_bin(f)
#' unlink(f)
read_bin = function(file, what = 'raw', n = file.info(file)$size, ...) {
  readBin(file, what, n, ...)
}

#' Read all text files and concatenate their content
#'
#' Read files one by one, and optionally add text before/after the content. Then
#' combine all content into one character vector.
#' @param files A vector of file paths.
#' @param before,after A function that takes one file path as the input and
#'   returns values to be added before or after the content of the file.
#'   Alternatively, they can be constant values to be added.
#' @return A character vector.
#' @export
#' @examples
#' # two files in this package
#' fs = system.file('scripts', c('call-fun.R', 'child-pids.sh'), package = 'xfun')
#' xfun::read_all(fs)
#'
#' # add file paths before file content and an empty line after content
#' xfun::read_all(fs, before = function(f) paste('#-----', f, '-----'), after = '')
#'
#' # add constants
#' xfun::read_all(fs, before = '/*', after = c('*/', ''))
read_all = function(files, before = function(f) NULL, after = function(f) NULL) {
  b = before; a = after
  x = unlist(lapply(files, function(f) {
    c(if (is.function(b)) b(f) else b, read_utf8(f), if (is.function(a)) a(f) else a)
  }))
  raw_string(x)
}

#' Read a text file, process the text with a function, and write the text back
#'
#' Read a text file with the UTF-8 encoding, apply a function to the text, and
#' write back to the original file if the processed text is different with the
#' original input.
#'
#' `sort_file()` is an application of `process_file()`, with the processing
#' function being [sort()], i.e., it sorts the text lines in a file and write
#' back the sorted text.
#' @param file Path to a text file.
#' @param fun A function to process the text.
#' @param x The content of the file.
#' @param ... Arguments to be passed to `process_file()`.
#' @return If `file` is provided, invisible `NULL` (the file is updated as a
#'   side effect), otherwise the processed content (as a character vector).
#' @export
#' @examples f = tempfile()
#' xfun::write_utf8('Hello World', f)
#' xfun::process_file(f, function(x) gsub('World', 'woRld', x))
#' xfun::read_utf8(f)  # see if it has been updated
#' file.remove(f)
process_file = function(file, fun = identity, x = read_utf8(file)) {
  x2 = fun(x)
  if (missing(file)) x2 else {
    if ((length(x2) != length(x)) || !all(x2 == x)) write_utf8(x2, file)
  }
}

#' @rdname process_file
#' @export
sort_file = function(..., fun = sort) {
  process_file(fun = fun, ...)
}

#' Search and replace strings in files
#'
#' These functions provide the "file" version of [gsub()], i.e.,
#' they perform searching and replacement in files via `gsub()`.
#' @param file Path of a single file.
#' @param ... For `gsub_file()`, arguments passed to `gsub()`. For
#'   other functions, arguments passed to `gsub_file()`. Note that the
#'   argument `x` of `gsub()` is the content of the file.
#' @param rw_error Whether to signal an error if the file cannot be read or
#'   written. If `FALSE`, the file will be ignored (with a warning).
#' @param files A vector of file paths.
#' @param dir Path to a directory (all files under this directory will be
#'   replaced).
#' @param recursive Whether to find files recursively under a directory.
#' @param ext A vector of filename extensions (without the leading periods).
#' @param mimetype A regular expression to filter files based on their MIME
#'   types, e.g., `'^text/'` for plain text files. This requires the
#'   \pkg{mime} package.
#' @note These functions perform in-place replacement, i.e., the files will be
#'   overwritten. Make sure you backup your files in advance, or use version
#'   control!
#' @export
#' @examples library(xfun)
#' f = tempfile()
#' writeLines(c('hello', 'world'), f)
#' gsub_file(f, 'world', 'woRld', fixed = TRUE)
#' readLines(f)
gsub_file = function(file, ..., rw_error = TRUE) {
  if (!(file.access(file, 2) == 0 && file.access(file, 4) == 0)) {
    (if (rw_error) stop else warning)('Unable to read or write to ', file)
    if (!rw_error) return(invisible())
  }
  x1 = tryCatch(read_utf8(file, error = TRUE), error = function(e) if (rw_error) stop(e))
  if (is.null(x1)) return(invisible())
  x2 = gsub(x = x1, ...)
  if (!identical(x1, x2)) write_utf8(x2, file)
}

#' @rdname gsub_file
#' @export
gsub_files = function(files, ...) {
  for (f in files) gsub_file(f, ...)
}

#' @rdname gsub_file
#' @export
gsub_dir = function(..., dir = '.', recursive = TRUE, ext = NULL, mimetype = '.*') {
  files = list.files(dir, full.names = TRUE, recursive = recursive)
  if (length(ext)) files = files[file_ext(files) %in% ext]
  if (mimetype != '.*') files = files[grep(mimetype, mime::guess_type(files))]
  gsub_files(files, ...)
}

#' @rdname gsub_file
#' @export
gsub_ext = function(ext, ..., dir = '.', recursive = TRUE) {
  gsub_dir(..., dir = dir, recursive = recursive, ext = ext)
}

#' Perform replacement with `gsub()` on elements matched from `grep()`
#'
#' This function is a shorthand of `gsub(pattern, replacement,
#' grep(pattern, x, value = TRUE))`.
#' @param pattern,replacement,x,... Passed to [grep()] and
#'   `gsub()`.
#' @return A character vector.
#' @export
#' @examples # find elements that matches 'a[b]+c' and capitalize 'b' with perl regex
#' xfun::grep_sub('a([b]+)c', 'a\\U\\1c', c('abc', 'abbbc', 'addc', '123'), perl = TRUE)
grep_sub = function(pattern, replacement, x, ...) {
  x = grep(pattern, x, value = TRUE, ...)
  gsub(pattern, replacement, x, ...)
}

#' Try various methods to download a file
#'
#' Try all possible methods in [download.file()] (e.g.,
#' `libcurl`, `curl`, `wget`, and `wininet`) and see if any
#' method can succeed. The reason to enumerate all methods is that sometimes the
#' default method does not work, e.g.,
#' <https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html>.
#' @param url The URL of the file.
#' @param output Path to the output file. By default, it is determined by
#'   [url_filename()].
#' @param ... Other arguments to be passed to [download.file()]
#'   (except `method`).
#' @param .error An error message to signal when the download fails.
#' @note To allow downloading large files, the `timeout` option in
#'   [options()] will be temporarily set to one hour (3600 seconds)
#'   inside this function when this option has the default value of 60 seconds.
#'   If you want a different `timeout` value, you may set it via
#'   `options(timeout = N)`, where `N` is the number of seconds (not
#'   60).
#' @return The integer code `0` for success, or an error if none of the
#'   methods work.
#' @export
download_file = function(
  url, output = url_filename(url), ...,
  .error = 'No download method works (auto/wininet/wget/curl/lynx)'
) {
  if (getOption('timeout') == 60L) {
    opts = options(timeout = 3600)  # one hour
    on.exit(options(opts), add = TRUE)
  }
  download = function(method = 'auto') suppressWarnings({
    tryCatch(download.file(url, output, ..., method = method), error = function(e) 1L)
  })
  for (method in c(if (is_windows()) 'wininet', 'libcurl', 'auto')) {
    if (download(method = method) == 0) return(0L)
  }

  # check for libcurl/curl/wget/lynx, call download.file with appropriate method
  if (Sys.which('curl') != '') {
    # curl needs to add a -L option to follow redirects
    opts2 = if (is.null(getOption('download.file.extra')))
      options(download.file.extra = c('-L', '--fail'))
    res = download(method = 'curl')
    options(opts2)
    if (res == 0) return(res)
  }
  if (Sys.which('wget') != '') {
    if ((res <- download(method = 'wget')) == 0) return(res)
  }
  if (Sys.which('lynx') != '') {
    if ((res <- download(method = 'lynx')) == 0) return(res)
  }

  stop(.error)
}

#' Test if a URL is accessible
#'
#' Try to send a `HEAD` request to a URL using
#' [curlGetHeaders()] or the \pkg{curl} package, and see if it
#' returns a successful status code.
#' @param x A URL as a character string.
#' @param use_curl Whether to use the \pkg{curl} package or the
#'   `curlGetHeaders()` function in base R to send the request to the URL.
#'   By default, \pkg{curl} will be used when base R does not have the
#'   \command{libcurl} capability (which should be rare).
#' @param ... Arguments to be passed to `curlGetHeaders()`.
#' @return `TRUE` or `FALSE`.
#' @export
#' @examples xfun::url_accessible('https://yihui.org')
url_accessible = function(x, use_curl = !capabilities('libcurl'), ...) {
  try_status = function(code) tryCatch(code < 400, error = function(e) FALSE)
  if (use_curl) {
    h = curl::new_handle()
    curl::handle_setopt(h, customrequest = 'HEAD', nobody = TRUE)
    try_status(curl::curl_fetch_memory(x, h)$status_code)
  } else {
    # use curlGetHeaders() instead
    try_status(attr(curlGetHeaders(x, ...), 'status'))
  }
}

#' Generate a message with `cat()`
#'
#' This function is similar to [message()], and the difference is
#' that `msg_cat()` uses [cat()] to write out the message,
#' which is sent to [stdout()] instead of [stderr()]. The
#' message can be suppressed by [suppressMessages()].
#' @param ... Character strings of messages, which will be concatenated into one
#'   string via `paste(c(...), collapse = '')`.
#' @note By default, a newline will not be appended to the message. If you need
#'   a newline, you have to explicitly add it to the message (see
#'   \sQuote{Examples}).
#' @return Invisible `NULL`, with the side-effect of printing the message.
#' @seealso This function was inspired by `rlang::inform()`.
#' @export
#' @examples
#' {
#' # a message without a newline at the end
#' xfun::msg_cat('Hello world!')
#' # add a newline at the end
#' xfun::msg_cat(' This message appears right after the previous one.\n')
#' }
#' suppressMessages(xfun::msg_cat('Hello world!'))
msg_cat = function(...) {
  x = paste(c(...), collapse = '')
  withRestarts({
    signalCondition(simpleMessage(x))
    cat(x)
  }, muffleMessage = function() invisible(NULL))
}

#' Save objects to files and lazy-load them
#'
#' The function [lazy_save()] saves objects to files with incremental integer
#' names (e.g., the first object is saved to `1.rds`, and the second object is
#' saved to `2.rds`, etc.). The function [lazy_load()] lazy-load objects from
#' files saved via [lazy_save()], i.e., a file will not be read until the object
#' is used.
#' @param list A character vector of object names. This list will be written to
#'   an index file with `0` as the base name (e.g., `0.rds`).
#' @param path The path to write files to / read files from.
#' @param method The file save/load method. It can be a string (e.g., `rds`,
#'   `raw`, or `qs`) or a list. See the `rw` argument of [cache_exec()]. By
#'   default, it is automatically detected by checking the existence of the
#'   index file (e.g., `0.rds`, `0.raw`, or `0.qs`).
#' @param envir The environment to [get] or [assign] objects.
#' @return [lazy_save()] returns invisible `NULL`; [lazy_load()] returns the
#'   object names invisibly.
#' @seealso [delayedAssign()]
#' @export
lazy_save = function(list = NULL, path = './', method = 'auto', envir = parent.frame()) {
  m = io_method(method, path)
  idx = lazy_idx(path, m$name)
  m$save(list, idx)
  for (i in seq_along(list)) {
    v = get(list[[i]], envir, inherits = FALSE)
    m$save(v, sprintf('%s%d.%s', path, i, m$name))
  }
}

#' @rdname lazy_save
#' @export
lazy_load = function(path = './', method = 'auto', envir = parent.frame()) {
  m = io_method(method, path)
  idx = lazy_idx(path, m$name)
  vars = m$load(idx)
  .mapply(function(path, name) {
    delayedAssign(name, m$load(path), assign.env = envir)
  }, list(sprintf('%s%d.%s', path, seq_along(vars), m$name), vars), NULL)
  invisible(vars)
}

# use 0.ext as the index file to store the object names
lazy_idx = function(path, ext) paste0(path, '0.', ext)

io_method = function(method, path) {
  if (is.character(method)) {
    if (method == 'auto') {
      for (m in io_methods) if (file_exists(lazy_idx(path, m$name))) {
        method = m$name; break
      }
    }
    if (method == 'auto') method = 'rds'
  }
  if (is.list(method)) method else io_methods[[method]]
}
