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
  which(is.na(iconv(x, 'UTF-8', 'UTF-8')))
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
  paste(read_utf8(file), collapse = '\n')
}

#' Search and replace strings in files
#'
#' These functions provide the "file" version of \code{\link{gsub}()}, i.e.,
#' they perform searching and replacement in files via \code{gsub()}.
#' @param file Path of a single file.
#' @param ... Arguments passed to \code{gsub()}. Note that the argument \code{x}
#'   of \code{gsub()} is the content of the file.
#' @param files A vector of file paths.
#' @param dir Path to a directory (all files under this directory will be
#'   replaced).
#' @param ext A filename extension (without the period).
#' @note These functions perform in-place replacement, i.e., the files will be
#'   overwritten. Make sure you backup your files in advance, or use version
#'   control!
#' @export
#' @examples library(xfun)
#' f = tempfile()
#' writeLines(c('hello', 'world'), f)
#' gsub_file(f, 'world', 'woRld', fixed = TRUE)
#' readLines(f)
gsub_file = function(file, ...) {
  x = gsub(x = read_utf8(file), ...)
  write_utf8(x, file)
}

#' @rdname gsub_file
#' @export
gsub_files = function(files, ...) {
  for (f in files) gsub_file(f, ...)
}

#' @rdname gsub_file
#' @export
gsub_dir = function(dir = '.', ...) {
  gsub_files(list.files(dir, full.names = TRUE), ...)
}

#' @rdname gsub_file
#' @export
gsub_ext = function(ext, ..., dir = '.') {
  files = list.files(dir, full.names = TRUE)
  gsub_files(files[file_ext(files) == ext], ...)
}
