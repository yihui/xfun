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

#' Set the global option \code{\link{options}(stringsAsFactors = FALSE)} inside
#' a parent function and restore the option after the parent function exits
#'
#' This is a shorthand of \code{opts = options(stringsAsFactors = FALSE);
#' on.exit(options(opts), add = TRUE)}; \code{strings_please()} is an alias of
#' \code{stringsAsStrings()}.
#' @export
#' @examples
#' f = function() {
#' xfun::strings_please()
#' data.frame(x = letters[1:4], y = factor(letters[1:4]))
#' }
#' str(f())  # the first column should be character
stringsAsStrings = function() {
  opts = options(stringsAsFactors = FALSE)
  do.call(
    on.exit, list(substitute(options(x), list(x = opts)), add = TRUE),
    envir = parent.frame()
  )
}

#' @rdname stringsAsStrings
#' @export
strings_please = stringsAsStrings

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
    res = gsub('^\\s*\\[[0-9]+]\\s*', '  ', res)  # remove vector indices like [1]
    res = gsubi('^\\s*other attached packages:\\s*$', 'Package version:', res)
    # print the locale info on a single line if possible
    if (length(i <- which(res == 'Locale:')) == 1 && res[i + 2] == '') {
      res[i] = paste(res[i], gsub('\\s*/\\s*', ' / ', gsub('^\\s+', '', res[i + 1])))
      res = res[-(i + 1)]
    }
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

#' Try various methods to download a file
#'
#' Try all possible methods in \code{\link{download.file}()} (e.g.,
#' \code{libcurl}, \code{curl}, \code{wget}, and \code{wininet}) and see if any
#' method can succeed. The reason to enumerate all methods is that sometimes the
#' default method does not work, e.g.,
#' \url{https://stat.ethz.ch/pipermail/r-devel/2016-June/072852.html}.
#' @param url The URL of the file.
#' @param output Path to the output file. If not provided, the base name of the
#'   URL will be used (query parameters and hash in the URL will be removed).
#' @param ... Other arguments to be passed to \code{\link{download.file}()}
#'   (except \code{method}).
#' @return The integer code \code{0} for success, or an error if none of the
#'   methods work.
#' @export
download_file = function(url, output = basename(url), ...) {
  if (missing(output)) output = gsub('[?#].*$', '', output)  # remove query/hash
  download = function(method = 'auto') download.file(url, output, ..., method = method)
  for (method in c('libcurl', if (is_windows()) 'wininet', 'auto')) {
    if (!inherits(try_silent(res <- download(method = method)), 'try-error') && res == 0)
      return(res)
  }

  # check for libcurl/curl/wget/lynx, call download.file with appropriate method
  res = NA
  if (Sys.which('curl') != '') {
    # curl needs to add a -L option to follow redirects
    opts = if (is.null(getOption('download.file.extra'))) options(download.file.extra = '-L')
    res = download(method = 'curl'); options(opts)
    if (res == 0) return(res)
  }
  if (Sys.which('wget') != '') {
    if ((res <- download(method = 'wget')) == 0) return(res)
  }
  if (Sys.which('lynx') != '') {
    if ((res <- download(method = 'lynx')) == 0) return(res)
  }
  if (is.na(res)) stop('No download method works (auto/wininet/wget/curl/lynx)')

  res
}

#' Turn the output of \code{\link{str}()} into a tree diagram
#'
#' The super useful function \code{str()} uses \verb{..} to indicate the level
#' of sub-elements of an object, which may be difficult to read. This function
#' uses vertical pipes to connect all sub-elements on the same level, so it is
#' clearer which elements belong to the same parent element in an object with a
#' nested structure (such as a nested list).
#' @param ... Arguments to be passed to \code{\link{str}()} (note that the
#'   \code{comp.str} is hardcoded inside this function, and it is the only
#'   argument that you cannot customize).
#' @return A character string as a \code{\link{raw_string}()}.
#' @export
#' @examples fit = lsfit(1:9, 1:9)
#' str(fit)
#' xfun::tree(fit)
#'
#' fit = lm(dist ~ speed, data = cars)
#' str(fit)
#' xfun::tree(fit)
#'
#' # some trivial examples
#' xfun::tree(1:10)
#' xfun::tree(iris)
tree = function(...) {
  x = capture.output(str(..., comp.str = '$ '))
  r = '^([^$-]+[$-] )(.*)$'
  x1 = gsub(r, '\\1', x)
  x2 = gsub(r, '\\2', x)
  x1 = gsub('[.][.]', '  ', x1)
  x1 = gsub('[$] $', '|-', x1)
  x1 = connect_pipes(x1)
  x3 = paste(x1, x2, sep = '')
  i = !grepl(r, x)
  x3[i] = x[i]
  raw_string(x3)
}

# for a tree diagram, connect the pipes on the same level, e.g., change

# |- ..
#   |- ..
#
#   |- ..

# to

# |- ..
#   |- ..
#   |
#   |- ..

# this task is not complicated, but just boring nested for-loops
connect_pipes = function(x) {
  ns = nchar(x); n = max(ns); m = length(x)
  if (n < 2 || m < 3) return(x)
  A = matrix('', nrow = m, ncol = n)
  x = strsplit(x, '')
  for (i in seq_len(m)) {
    A[i, seq_len(ns[i])] = x[[i]]
  }
  k = NULL
  for (j in seq_len(n - 1)) {
    for (i in seq_len(m - 2)) {
      if (!all(A[i, j + 0:1] == c('|', '-'))) next
      for (l in (i + 1):m) {
        cells = A[l, j + 0:1]
        if (all(cells == ' ')) {
          if (l == m) {
            k = NULL; break
          } else k = c(k, l)
        } else if (all(cells == c('|', '-'))) {
          break
        } else {
          k = NULL; break
        }
      }
      if (length(k) > 0) A[k, j] = '|'
      k = NULL
    }
  }
  apply(A, 1, paste, collapse = '')
}
