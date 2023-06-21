#' Try to use the system native encoding to represent a character vector
#'
#' Apply `enc2native()` to the character vector, and check if `enc2utf8()` can
#' convert it back without a loss. If it does, return `enc2native(x)`, otherwise
#' return the original vector with a warning.
#' @param x A character vector.
#' @note On platforms that supports UTF-8 as the native encoding
#'   ([l10n_info()]`[['UTF-8']]` returns `TRUE`), the conversion will be
#'   skipped.
#' @export
#' @examples
#' library(xfun)
#' s = intToUtf8(c(20320, 22909))
#' Encoding(s)
#'
#' s2 = native_encode(s)
#' Encoding(s2)
native_encode = function(x) {
  if (isTRUE(l10n_info()[['UTF-8']])) return(x)
  if (identical(enc2utf8(x2 <- enc2native(x)), x)) return(x2)
  warning('The character vector cannot be represented in the native encoding')
  x
}

#' Check if a character vector consists of entirely ASCII characters
#'
#' Converts the encoding of a character vector to `'ascii'`, and check if
#' the result is `NA`.
#' @param x A character vector.
#' @return A logical vector indicating whether each element of the character
#'   vector is ASCII.
#' @export
#' @examples library(xfun)
#' is_ascii(letters)  # yes
#' is_ascii(intToUtf8(8212))  # no
is_ascii = function(x) {
  out = iconv(x, to = 'ascii') == x
  out[is.na(out)] = FALSE
  out[is.na(x)] = NA
  out
}
