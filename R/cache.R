#' Cache the value of an R expression to an RDS file
#'
#' Save the value of an expression to a cache file (of the RDS format). Next
#' time the value is loaded from the file if it exists. To invalidate the cache,
#' you can either delete the file, or use the argument \code{rerun = TRUE}.
#' @param expr An R expression.
#' @param rerun Whether to delete the RDS file, rerun the expression, and save
#'   the result again (i.e., invalidate the cache).
#' @param file The cache filename under the directory specified by the
#'   \code{dir} argument. If not specified and this function is called inside a
#'   code chunk of a \pkg{knitr} document (e.g., an R Markdown document), the
#'   filename is the current chunk label with the extension \file{.rds}.
#' @param dir The path of the RDS file is determined by \code{paste0(dir,
#'   file)}. If not specified and the \pkg{knitr} package is available, the
#'   default value of \code{dir} is the \pkg{knitr} chunk option
#'   \code{cache.path} (so if you are compiling a \pkg{knitr} document, you do
#'   not need to provide this \code{dir} argument explicitly), otherwise the
#'   default is \file{cache/}. If you do not want to provide a \code{dir} but
#'   simply a valid path to the \code{file} argument, you may use \code{dir =
#'   ""}.
#' @param ... Other arguments to be passed to \code{\link{saveRDS}()}.
#' @note When this function is called in a code chunk of a \pkg{knitr} document,
#'   you may not want to provide the filename or directory of the cache file,
#'   because they have reasonable defaults.
#' @return If the cache file does not exist, run the expression and save the
#'   result to the file, otherwise read the cache file and return the value.
#' @export
#' @examples
#' f = tempfile()  # the cache file
#' compute = function(...) {
#'   res = xfun::cache_rds({
#'     Sys.sleep(2)
#'     1:10
#'   }, file = f, dir = '', ...)
#'   res
#' }
#' compute()  # takes two seconds
#' compute()  # returns 1:10 immediately
#' compute()  # fast again
#' compute(rerun = TRUE)  # two seconds to rerun
#' compute()
#' file.remove(f)
cache_rds = function(expr = {}, rerun = FALSE, file = 'cache.rds', dir = 'cache/', ...) {
  if (loadable('knitr')) {
    if (missing(file) && !is.null(lab <- knitr::opts_current$get('label')))
      file = paste0(lab, '.rds')
    if (missing(dir)) dir = knitr::opts_chunk$get('cache.path')
  }
  path = paste0(dir, file)
  if (rerun) unlink(path)
  if (file.exists(path)) readRDS(path) else {
    obj = expr  # lazy evaluation
    saveRDS(obj, path, ...)
    obj
  }
}
