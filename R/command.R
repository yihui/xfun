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
  file, version = c("R-devel", "R-release", "R-oldrelease"),
  server = 'ftp://win-builder.r-project.org/'
) {
  res = unlist(lapply(version, upload_ftp, file = file, server = server))
  setNames(res, version)
}
