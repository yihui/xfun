#' Encode data into the base64 encoding.
#'
#' Encode a file or a raw vector into the base64 encoding.
#' @param x A raw vector. If not raw, it is assumed to be a file or a connection
#'   to be read as raw via \code{readBin()}.
#' @return A character string.
#' @useDynLib xfun, .registration = TRUE
#' @export
#' @examples xfun::base64_encode(as.raw(1:10))
#' logo = file.path(R.home('doc'), 'html', 'logo.jpg')
#' xfun::base64_encode(logo)
base64_encode = function(x) {
  if (!is.raw(x)) x = read_bin(x)
  .Call('base64_enc', x)
}
