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

#' Call a function in a new R session via \code{Rscript()}
#'
#' Save the argument values of a function in a temporary RDS file, open a new R
#' session via \code{\link{Rscript}()}, read the argument values, call the
#' function, and read the returned value back to the current R session.
#' @param fun A function, or a character string that can be parsed and evaluated
#'   to a function.
#' @param args A list of argument values.
#' @export
#' @return The returned value of the function in the new R session.
#' @examples factorial(10)
#' # should return the same value
#' xfun::Rscript_call('factorial', list(10))
#'
#' # the first argument can be either a character string or a function
#' xfun::Rscript_call(factorial, list(10))
Rscript_call = function(fun, args = list()) {
  f = replicate(2, tempfile(fileext = '.rds'))
  on.exit(unlink(f), add = TRUE)
  saveRDS(list(fun, args), f[1])
  Rscript(shQuote(c(system.file('scripts', 'call-fun.R', package = 'xfun'), f)))
  readRDS(f[2])
}

#' Upload to an FTP server via \command{curl}
#'
#' Run the command \command{curl -T file server} to upload a file to an FTP
#' server. These functions require the system package (\emph{not the R package})
#' \command{curl} to be installed (which should be available on macOS by
#' default). The function \code{upload_win_builder()} uses \code{upload_ftp()}
#' to upload packages to the win-builder server.
#'
#' These functions were written mainly to save package developers the trouble of
#' going to the win-builder web page and uploading packages there manually. You
#' may also consider using \code{devtools::check_win_*}, which currently only
#' allows you to upload a package to one folder on win-builder each time, and
#' \code{xfun::upload_win_builder()} uploads to all three folders, which is more
#' likely to be what you need.
#' @param file Path to a local file.
#' @param server The address of the FTP server.
#' @param dir The remote directory to which the file should be uploaded.
#' @param version The R version(s) on win-builder.
#' @return Status code returned from \code{\link{system2}}.
#' @export
upload_ftp = function(file, server, dir = '') {
  if (dir != '') dir = gsub('/*$', '/', dir)
  system2('curl', shQuote(c('-T', file, paste0(server, dir))))
}

#' @rdname upload_ftp
#' @export
upload_win_builder = function(
  file, version = c("R-devel", "R-release", "R-oldrelease", "R-devel_gcc8"),
  server = 'ftp://win-builder.r-project.org/'
) {
  res = unlist(lapply(version, upload_ftp, file = file, server = server))
  setNames(res, version)
}
