#' Try to use the system native encoding to represent a character vector
#'
#' Apply \code{enc2native()} to the character vector, and check if
#' \code{enc2utf8()} can convert it back without a loss. If it does, return
#' \code{enc2native(x)}, otherwise return the original vector with a warning.
#' @param x A character vector.
#' @param windows_only Whether to make the attempt on Windows only. On Unix,
#'   characters are typically encoded in the native encoding (UTF-8), so there
#'   is no need to do the conversion.
#' @export
#' @examples
#' library(xfun)
#' s = intToUtf8(c(20320, 22909))
#' Encoding(s)
#'
#' s2 = native_encode(s)
#' Encoding(s2)
native_encode = function(x, windows_only = is_windows()) {
  if (!windows_only) return(x)
  if (identical(enc2utf8(x2 <- enc2native(x)), x)) return(x2)
  warning('The character vector cannot be represented in the native encoding')
  x
}
