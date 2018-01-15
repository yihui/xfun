#' Read / write files encoded in UTF-8
#'
#' Read or write files, assuming they are encoded in UTF-8. \code{read_utf8()}
#' is roughly \code{readLines(encoding = 'UTF-8')} (a warning will be issued if
#' non-UTF8 lines are found), and \code{write_utf8()} calls
#' \code{writeLines(enc2utf8(text), useBytes = TRUE)}.
#' @param con A connection or a file path.
#' @param text A character vector (will be converted to UTF-8 via
#'   \code{\link{enc2utf8}()}).
#' @param ... Other arguments passed to \code{\link{writeLines}()} (except
#'   \code{useBytes}, which is \code{TRUE} in \code{write_utf8()}).
#' @export
read_utf8 = function(con) {
  x = readLines(con, encoding = 'UTF-8', warn = FALSE)
  i = invalid_utf8(x)
  n = length(i)
  if (n > 0) warning(
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
    writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
  }
}

# which lines are invalid UTF-8
invalid_utf8 = function(x) {
  which(is.na(iconv(x, 'UTF-8', 'UTF-8')))
}
