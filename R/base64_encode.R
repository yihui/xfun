
#'@useDynLib xfun, .base64_encode=base64_encode
#'@export
base64_encode <- function(input) {
  if (!is.raw(input)) {
    stop("input should be a raw vector")
  }
  .Call(.base64_encode, input)
}
