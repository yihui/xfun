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

#' An alternative to sessionInfo() to print session information
#'
#' This function tweaks the output of \code{\link{sessionInfo}()}: (1) It adds
#' the RStudio version information if running in the RStudio IDE; (2) It removes
#' the information about matrix products, BLAS, and LAPACK; (3) It removes the
#' names of base R packages; (4) It prints out package versions in a single
#' group, and does not differentiate between loaded and attached packages.
#'
#' It also allows you to only print out the versions of specified packages (via
#' the \code{packages} argument) and optionally their recursive dependencies.
#' For these specified packages (if provided), if a function
#' \code{xfun_session_info()} exists in a package, it will be called and
#' expected to return a character vector to be appended to the output of
#' \code{session_info()}. This provides a mechanism for other packages to inject
#' more information into the \code{session_info} output. For example,
#' \pkg{rmarkdown} (>= 1.20.2) has a function \code{xfun_session_info()} that
#' returns the version of Pandoc, which can be very useful information for
#' diagnostics.
#' @param packages A character vector of package names, of which the versions
#'   will be printed. If not specified, it means all loaded and attached
#'   packages in the current R session.
#' @param dependencies Whether to print out the versions of the recursive
#'   dependencies of packages.
#' @return A character vector of the session information marked as
#'   \code{\link{raw_string}()}.
#' @export
#' @examples xfun::session_info()
#' if (loadable('MASS')) xfun::session_info('MASS')
session_info = function(packages = NULL, dependencies = TRUE) {
  res = sessionInfo()
  res$matprod = res$BLAS = res$LAPACK = NULL
  if (loadable('rstudioapi') && rstudioapi::isAvailable()) {
    res$running = paste0(res$running, ', RStudio ', rstudioapi::getVersion())
  }

  tweak_info = function(obj, extra = NULL) {
    res = capture.output(print(obj))
    i = grep('^(attached base packages|Matrix products):\\s*$', res, ignore.case = TRUE)
    if (length(i)) res = res[-c(i, i + 1)]
    res = gsubi('^\\s*locale:\\s*$', 'Locale:', res)
    res = gsubi('^\\s*other attached packages:\\s*$', 'Package version:', res)
    raw_string(c(res, extra))
  }

  version_info = function(pkgs) {
    res = lapply(pkgs, function(p) {
      list(Version = as.character(packageVersion(p)), Package = p)
    })
    as.list(setNames(res, pkgs))
  }

  res$basePkgs = raw_string(list())
  info = c(res$otherPkgs, res$loadedOnly)
  if (length(packages) > 0) {
    info = info[intersect(names(info), packages)]
    info = c(info, version_info(setdiff(packages, names(info))))
  }
  res$loadedOnly = NULL
  if (dependencies) {
    deps = pkg_dep(names(info), installed.packages(), recursive = TRUE)
    deps = sort(setdiff(deps, names(info)))
    info = c(info, version_info(deps))
  }
  if (length(packages) > 0 || dependencies) info = info[sort(names(info))]
  res$otherPkgs = info
  extra = unlist(lapply(packages, function(p) tryCatch(
    c('', getFromNamespace('xfun_session_info', p)()), error = function(e) NULL)
  ))

  tweak_info(res, extra)
}

gsubi = function(...) gsub(..., ignore.case = TRUE)
