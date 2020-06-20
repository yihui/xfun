#' @useDynLib xfun, .registration = TRUE
#' @export
base64_encode = function(x) {
  if (!is.raw(x)) x = read_raw(x)
  .Call(.base64_encode, x)
}
