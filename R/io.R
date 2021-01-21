#' Read / write files encoded in UTF-8
#'
#' Read or write files, assuming they are encoded in UTF-8. \code{read_utf8()}
#' is roughly \code{readLines(encoding = 'UTF-8')} (a warning will be issued if
#' non-UTF8 lines are found), and \code{write_utf8()} calls
#' \code{writeLines(enc2utf8(text), useBytes = TRUE)}.
#' @param con A connection or a file path.
#' @param error Whether to signal an error when non-UTF8 characters are detected
#'   (if \code{FALSE}, only a warning message is issued).
#' @param text A character vector (will be converted to UTF-8 via
#'   \code{\link{enc2utf8}()}).
#' @param ... Other arguments passed to \code{\link{writeLines}()} (except
#'   \code{useBytes}, which is \code{TRUE} in \code{write_utf8()}).
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
}

# which lines are invalid UTF-8
invalid_utf8 = function(x) {
  which(!is.na(x) & is.na(iconv(x, 'UTF-8', 'UTF-8')))
}

#' Read a text file and concatenate the lines by \code{'\n'}
#'
#' The source code of this function should be self-explanatory.
#' @param file Path to a text file (should be encoded in UTF-8).
#' @return A character string of text lines concatenated by \code{'\n'}.
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
#' This is a wrapper function of \code{\link{readBin}()} with default arguments
#' \code{what = "raw"} and \code{n = \link{file.size}(file)}, which means it
#' will read the full content of a binary file as a raw vector by default.
#' @param file,what,n,... Arguments to be passed to \code{readBin()}.
#' @return A vector returned from \code{readBin()}.
#' @export
#' @examples
#' f = tempfile()
#' cat('abc', file = f)
#' xfun::read_bin(f)
#' unlink(f)
read_bin = function(file, what = 'raw', n = file.info(file)$size, ...) {
  readBin(file, what, n, ...)
}

#' Read a text file, process the text with a function, and write the text back
#'
#' Read a text file with the UTF-8 encoding, apply a function to the text, and
#' write back to the original file.
#' @param file Path to a text file.
#' @param FUN A function to process the text.
#' @param x The content of the file.
#' @return If \code{file} is provided, invisible \code{NULL} (the file is
#'   updated as a side effect), otherwise the processed content (as a character
#'   vector).
#' @export
#' @examples f = tempfile()
#' xfun::write_utf8('Hello World', f)
#' xfun::process_file(f, function(x) gsub('World', 'woRld', x))
#' xfun::read_utf8(f)  # see if it has been updated
#' file.remove(f)
process_file = function(file, FUN = identity, x = read_utf8(file)) {
  x = FUN(x)
  if (missing(file)) x else write_utf8(x, file)
}

#' Search and replace strings in files
#'
#' These functions provide the "file" version of \code{\link{gsub}()}, i.e.,
#' they perform searching and replacement in files via \code{gsub()}.
#' @param file Path of a single file.
#' @param ... For \code{gsub_file()}, arguments passed to \code{gsub()}. For
#'   other functions, arguments passed to \code{gsub_file()}. Note that the
#'   argument \code{x} of \code{gsub()} is the content of the file.
#' @param rw_error Whether to signal an error if the file cannot be read or
#'   written. If \code{FALSE}, the file will be ignored (with a warning).
#' @param files A vector of file paths.
#' @param dir Path to a directory (all files under this directory will be
#'   replaced).
#' @param recursive Whether to find files recursively under a directory.
#' @param ext A vector of filename extensions (without the leading periods).
#' @param mimetype A regular expression to filter files based on their MIME
#'   types, e.g., \code{'^text/'} for plain text files. This requires the
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

#' Perform replacement with \code{gsub()} on elements matched from \code{grep()}
#'
#' This function is a shorthand of \code{gsub(pattern, replacement,
#' grep(pattern, x, value = TRUE))}.
#' @param pattern,replacement,x,... Passed to \code{\link{grep}()} and
#'   \code{gsub()}.
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
#' Try all possible methods in \code{\link{download.file}()} (e.g.,
#' \code{libcurl}, \code{curl}, \code{wget}, and \code{wininet}) and see if any
#' method can succeed. The reason to enumerate all methods is that sometimes the
#' default method does not work, e.g.,
#' \url{https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html}.
#' @param url The URL of the file.
#' @param output Path to the output file. By default, it is determined by
#'   \code{\link{url_filename}()}.
#' @param ... Other arguments to be passed to \code{\link{download.file}()}
#'   (except \code{method}).
#' @return The integer code \code{0} for success, or an error if none of the
#'   methods work.
#' @export
download_file = function(url, output = url_filename(url), ...) {
  download = function(method = 'auto') download.file(url, output, ..., method = method)
  for (method in c(if (is_windows()) 'wininet', 'libcurl', 'auto')) {
    if (!inherits(try_silent(res <- download(method = method)), 'try-error') && res == 0)
      return(res)
  }

  # check for libcurl/curl/wget/lynx, call download.file with appropriate method
  res = NA
  if (Sys.which('curl') != '') {
    # curl needs to add a -L option to follow redirects
    opts = if (is.null(getOption('download.file.extra'))) options(download.file.extra = '-L')
    res = download(method = 'curl'); options(opts)
    if (res == 0) return(res)
  }
  if (Sys.which('wget') != '') {
    if ((res <- download(method = 'wget')) == 0) return(res)
  }
  if (Sys.which('lynx') != '') {
    if ((res <- download(method = 'lynx')) == 0) return(res)
  }
  if (is.na(res)) stop('No download method works (auto/wininet/wget/curl/lynx)')

  res
}

#' Get the tags of Github releases of a repository
#'
#' Read the HTML source of the release page and parse the tags of the releases.
#' @param repo The repository name of the form \code{user/repo}, e.g.,
#'   \code{"yihui/xfun"}.
#' @param subpath A character string to be appended to the URL of Github
#'   releases (i.e., \verb{https://github.com/user/repo/releases/}). For
#'   example, you may use \code{subpath = "latest"} to get the tag of the latest
#'   release.
#' @param pattern A regular expression to extract the tags from the HTML source.
#'   It must contain a group (i.e., must have a pair of parentheses).
#' @export
#' @return A character vector of (GIT) tags.
#' @examples if (interactive()) xfun::github_releases('yihui/xfun')
github_releases = function(repo, subpath = '', pattern = '(v[0-9.]+)') {
  h = readLines(sprintf('https://github.com/%s/releases/%s', repo, subpath), warn = FALSE)
  r = sprintf('^.*?releases/tag/%s".*', pattern)
  v = gsub(r, '\\1', grep(r, h, value = TRUE))
  unique(v)
}

#' Generate a message with \code{cat()}
#'
#' This function is similar to \code{\link{message}()}, and the difference is
#' that \code{msg_cat()} uses \code{\link{cat}()} to write out the message,
#' which is sent to \code{\link{stdout}} instead of \code{\link{stderr}}. The
#' message can be suppressed by \code{\link{suppressMessages}()}.
#' @param ... Character strings of messages, which will be concatenated into one
#'   string via \code{paste(c(...), collapse = '')}.
#' @note By default, a newline will not be appended to the message. If you need
#'   a newline, you have to explicitly add it to the message (see
#'   \sQuote{Examples}).
#' @return Invisible \code{NULL}, with the side-effect of printing the message.
#' @seealso This function was inspired by \code{rlang::inform()}.
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
