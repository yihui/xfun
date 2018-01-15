#' Attach or load packages, and automatically install missing packages if
#' requested
#'
#' \code{pkg_attach()} is a vectorized version of \code{\link{library}()} over
#' the \code{package} argument to attach multiple packages in a single function
#' call. \code{pkg_load()} is a vectorized version of
#' \code{\link{requireNamespace}()} to load packages (without attaching them).
#' The functions \code{pkg_attach2()} and \code{pkg_load2()} are wrappers of
#' \code{pkg_attach(install = TRUE)} and \code{pkg_load(install = TRUE)},
#' respectively. \code{loadable()} is an abbreviation of
#' \code{requireNamespace(quietly = TRUE)}.
#'
#' These are convenience functions that aim to solve these common problems: (1)
#' We often need to attach or load multiple packages, and it is tedious to type
#' several \code{library()} calls; (2) We are likely to want to install the
#' packages when attaching/loading them but they have not been installed.
#' @param ... Package names (character vectors, and must always be quoted).
#' @param install Whether to automatically install packages that are not
#'   available using \code{\link{install.packages}()}. You are recommended to
#'   set a CRAN mirror in the global option \code{repos} via
#'   \code{\link{options}()} if you want to automatically install packages.
#' @return \code{pkg_attach()} returns \code{NULL} invisibly. \code{pkg_load()}
#'   returns a logical vector, indicating whether the packages can be loaded.
#' @import utils
#' @export
#' @examples library(xfun)
#' pkg_attach('stats', 'graphics')
#' # pkg_attach2('servr')  # automatically install servr if it is not installed
#'
#' (pkg_load('stats', 'graphics'))
pkg_attach = function(..., install = FALSE) {
  for (i in c(...)) {
    if (install && !loadable(i)) install.packages(i)
    library(i, character.only = TRUE)
  }
}

#' @param error Whether to signal an error when certain packages cannot be loaded.
#' @rdname pkg_attach
#' @export
pkg_load = function(..., error = TRUE, install = FALSE) {
  n = length(pkg <- c(...)); res = logical(n)
  if (n == 0) return(invisible(res))
  for (i in seq_len(n)) {
    res[i] = loadable(p <- pkg[i])
    if (install && !res[i]) {
      install.packages(p); res[i] = loadable(p)
    }
  }
  if (error && any(!res)) stop('Package(s) not loadable: ', paste(pkg[!res], collapse = ' '))
  invisible(res)
}

#' @param pkg A single package name.
#' @param strict If \code{TRUE}, use \code{\link{requireNamespace}()} to test if
#'   a package is loadable; otherwise only check if the package is in
#'   \code{\link{.packages}(TRUE)} (this does not really load the package, so it
#'   is less rigorous but on the other hand, it can keep the current R session
#'   clean).
#' @rdname pkg_attach
#' @export
loadable = function(pkg, strict = TRUE) {
  if (strict) requireNamespace(pkg, quietly = TRUE) else pkg %in% .packages(TRUE)
}

#' @rdname pkg_attach
#' @export
pkg_attach2 = function(...) pkg_attach(..., install = TRUE)

#' @rdname pkg_attach
#' @export
pkg_load2 = function(...) pkg_load(..., install = TRUE)
