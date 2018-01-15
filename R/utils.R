#' Obtain an attribute of an object without partial matching
#'
#' An abbreviation of \code{base::\link[base]{attr}(exact = TRUE)}.
#' @param ... Passed to \code{base::\link[base]{attr}()} (without the
#'   \code{exact} argument).
#' @export
#' @examples
#' z = structure(list(a = 1), foo = 2)
#' base::attr(z, 'f')  # 2
#' xfun::attr(z, 'f')  # NULL
#' xfun::attr(z, 'foo')  # 2
attr = function(...) base::attr(..., exact = TRUE)

#' Evaluate an expression under a specified working directory
#'
#' Change the working directory, evaluate the expression, and restore the
#' working directory.
#' @param dir Path to a directory.
#' @param expr An R expression.
#' @export
#' @examples
#' library(xfun)
#' in_dir(tempdir(), {print(getwd()); list.files()})
in_dir = function(dir, expr) {
  owd = setwd(dir); on.exit(setwd(owd))
  expr
}

#' Test if an object is identical to \code{FALSE}
#'
#' A simple abbreviation of \code{identical(x, FALSE)}.
#' @param x An R object.
#' @export
#' @examples
#' library(xfun)
#' isFALSE(TRUE)  # false
#' isFALSE(FALSE)  # true
#' isFALSE(c(FALSE, FALSE))  # false
isFALSE = function(x) identical(x, FALSE)

#' Parse R code and do not keep the source
#'
#' An abbreviation of \code{parse(keep.source = FALSE)}.
#' @param code A character vector of the R source code.
#' @export
#' @return R \code{\link{expression}}s.
#' @examples library(xfun)
#' parse_only('1+1'); parse_only(c('y~x', '1:5 # a comment'))
#' parse_only(character(0))
parse_only = function(code) {
  if (length(code) == 0) return(expression())
  parse(text = code, keep.source = FALSE)
}

#' Try to evaluate an expression silently
#'
#' An abbreviation of \code{try(silent = TRUE)}.
#' @param expr An R expression.
#' @export
#' @examples library(xfun)
#' z = try_silent(stop('Wrong!'))
#' inherits(z, 'try-error')
try_silent = function(expr) try(expr, silent = TRUE)
