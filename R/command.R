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

#' Run the commands \command{Rscript} and \command{R CMD}
#'
#' Wrapper functions to run the commands \command{Rscript} and \command{R CMD}.
#' @param args A character vector of command-line arguments.
#' @param ... Other arguments to be passed to \code{\link{system2}()}.
#' @export
#' @return A value returned by \code{system2()}.
#' @examples library(xfun)
#' Rscript(c('-e', '1+1'))
#' Rcmd(c('build', '--help'))
Rscript = function(args, ...) {
  system2(file.path(R.home('bin'), 'Rscript'), args, ...)
}

#' @rdname Rscript
#' @export
Rcmd = function(args, ...) {
  system2(file.path(R.home('bin'), 'R'), c('CMD', args), ...)
}
