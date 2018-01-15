#' Run OptiPNG on all PNG files under a directory
#'
#' Calls the command \command{optipng} to optimize all PNG files under a
#' directory.
#' @param dir Path to a directory.
#' @references OptiPNG: \url{http://optipng.sourceforge.net}.
#' @export
optipng = function(dir = '.') {
  files = list.files(dir, '[.]png$', recursive = TRUE, full.names = TRUE)
  for (f in files) system2('optipng', shQuote(f))
}
