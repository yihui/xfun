`%|%` = function(x, y) if (length(x)) x else y
if (getRversion() < '4.4.0') `%||%` = function(x, y) if (is.null(x)) y else x

stop2 = function(...) stop(..., call. = FALSE)

warning2 = function(...) warning(..., call. = FALSE)

#' Obtain an attribute of an object without partial matching
#'
#' An abbreviation of [`base::attr`]`(exact = TRUE)`.
#' @param ... Passed to [base::attr()] (without the `exact` argument).
#' @export
#' @examples
#' z = structure(list(a = 1), foo = 2)
#' base::attr(z, 'f')  # 2
#' xfun::attr(z, 'f')  # NULL
#' xfun::attr(z, 'foo')  # 2
attr = function(...) base::attr(..., exact = TRUE)

#' Set environment variables
#'
#' Set environment variables from a named character vector, and return the old
#' values of the variables, so they could be restored later.
#'
#' The motivation of this function is that [Sys.setenv()] does not
#' return the old values of the environment variables, so it is not
#' straightforward to restore the variables later.
#' @param vars A named character vector of the form `c(VARIABLE = VALUE)`.
#'   If any value is `NA`, this function will try to unset the variable.
#' @return Old values of the variables (if not set, `NA`).
#' @export
#' @examples
#' vars = xfun::set_envvar(c(FOO = '1234'))
#' Sys.getenv('FOO')
#' xfun::set_envvar(vars)
#' Sys.getenv('FOO')
set_envvar = function(vars) {
  if (is.null(nms <- names(vars)) || any(nms == '')) stop(
    "The 'vars' argument must take a named character vector."
  )
  vals = Sys.getenv(nms, NA, names = TRUE)
  i = is.na(vars)
  suppressWarnings(Sys.unsetenv(nms[i]))
  if (length(vars <- vars[!i])) do.call(Sys.setenv, as.list(vars))
  invisible(vals)
}

#' Retrieve a global option from both `options()` and environment variables
#'
#' If the option exists in [options()], use its value. If not, query the
#' environment variable with the name `R_NAME` where `NAME` is the capitalized
#' option name with dots substituted by underscores. For example, for an option
#' `xfun.foo`, first we try `getOption('xfun.foo')`; if it does not exist, we
#' check the environment variable `R_XFUN_FOO`.
#'
#' This provides two possible ways, whichever is more convenient, for users to
#' set an option. For example, global options can be set in the [.Rprofile]
#' file, and environment variables can be set in the [.Renviron] file.
#' @param name The option name.
#' @param default The default value if the option is not found in [options()] or
#'   environment variables.
#' @return The option value.
#' @export
#' @examples
#' xfun::env_option('xfun.test.option')  # NULL
#'
#' Sys.setenv(R_XFUN_TEST_OPTION = '1234')
#' xfun::env_option('xfun.test.option')  # 1234
#'
#' options(xfun.test.option = TRUE)
#' xfun::env_option('xfun.test.option')  # TRUE (from options())
#' options(xfun.test.option = NULL)  # reset the option
#' xfun::env_option('xfun.test.option')  # 1234 (from env var)
#'
#' Sys.unsetenv('R_XFUN_TEST_OPTION')
#' xfun::env_option('xfun.test.option')  # NULL again
#'
#' xfun::env_option('xfun.test.option', FALSE)  # use default
env_option = function(name, default = NULL) {
  if (name %in% names(.Options)) return(.Options[[name]])
  name = toupper(paste0('R_', gsub('[.]', '_', name)))
  envs = Sys.getenv()
  if (name %in% names(envs)) envs[[name]] else default
}

#' Call `on.exit()` in a parent function
#'
#' The function [on.exit()] is often used to perform tasks when the
#' current function exits. This `exit_call()` function allows calling a
#' function when a parent function exits (thinking of it as inserting an
#' `on.exit()` call into the parent function).
#' @param fun A function to be called when the parent function exits.
#' @param n The parent frame number. For `n = 1`, `exit_call(fun)` is
#'   the same as `on.exit(fun())`; `n = 2` means adding
#'   `on.exit(fun())` in the parent function; `n = 3` means the
#'   grandparent, etc.
#' @param ... Other arguments to be passed to `on.exit()`.
#' @references This function was inspired by Kevin Ushey:
#'   <https://yihui.org/en/2017/12/on-exit-parent/>
#' @export
#' @examples
#' f = function(x) {
#'   print(x)
#'   xfun::exit_call(function() print('The parent function is exiting!'))
#' }
#' g = function(y) {
#'   f(y)
#'   print('f() has been called!')
#' }
#' g('An argument of g()!')
exit_call = function(fun, n = 2, ...) {
  do.call(
    on.exit, list(substitute(fun(), list(fun = fun)), add = TRUE, ...),
    envir = parent.frame(n)
  )
}

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

#' Test if an object is `FALSE`
#'
#' For R versions lower than 3.5.0, this function is a simple abbreviation of
#' `identical(x, FALSE)`. For higher R versions, this function calls
#' `base::isFALSE()`.
#' @param x An R object.
#' @note This function will be deprecated in the future. We recommend that you
#'   use [base::isFALSE()] instead. If you have to support R versions lower
#'   than 3.5.0, you may use `identical(x, FALSE)`, but please note that it is
#'   not equivalent to `base::isFALSE()`.
#' @export
#' @keywords internal
isFALSE = function(x) {
  pkgs = tools::dependsOnPkgs('xfun', dependencies = 'all', recursive = FALSE)
  pkgs = intersect(pkgs, sys.packages())
  vers = sapply(pkgs, function(p) as.character(packageVersion(p)))
  if ('isFALSE' %in% ls(baseenv())) stop(
    'The function xfun::isFALSE() has been deprecated. Please ',
    if (length(vers)) {
      c('update the possibly outdated package(s): ', paste(pkgs, vers, sep = ' ', collapse = ', '), '. ')
    } else {
      'consider using base::isFALSE(x) or identical(x, FALSE) instead. '
    },
    'You may see https://yihui.org/en/2023/02/xfun-isfalse/ for more info.'
  )
  identical(x, FALSE)
}

# try to get the names of packages for all functions on the call stack
sys.packages = function() {
  unique(unlist(lapply(seq_along(sys.calls()), function(i) {
    environment(sys.function(i))$.packageName
  })))
}

#' Parse R code and do not keep the source
#'
#' An abbreviation of `parse(keep.source = FALSE)`.
#' @param code A character vector of the R source code.
#' @export
#' @return R [expression()]s.
#' @examples library(xfun)
#' parse_only('1+1'); parse_only(c('y~x', '1:5 # a comment'))
#' parse_only(character(0))
parse_only = function(code) {
  if (length(code) == 0) return(expression())
  parse(text = code, keep.source = FALSE)
}

#' Try to evaluate an expression silently
#'
#' An abbreviation of `try(silent = TRUE)`.
#' @param expr An R expression.
#' @export
#' @examples library(xfun)
#' z = try_silent(stop('Wrong!'))
#' inherits(z, 'try-error')
try_silent = function(expr) try(expr, silent = TRUE)

#' Try an expression and see if it throws an error
#'
#' Use [tryCatch()] to check if an expression throws an error.
#' @inheritParams try_silent
#' @return `TRUE` (error) or `FALSE` (success).
#' @export
#' @examples
#' xfun::try_error(stop('foo'))  # TRUE
#' xfun::try_error(1:10)  # FALSE
try_error = function(expr) {
  err = FALSE
  tryCatch(expr, error = function(e) err <<- TRUE)
  err
}

#' Retry calling a function for a number of times
#'
#' If the function returns an error, retry it for the specified number of
#' times, with a pause between attempts.
#'
#' One application of this function is to download a web resource. Since the
#' download might fail sometimes, you may want to retry it for a few more times.
#' @param fun A function.
#' @param ... Arguments to be passed to the function.
#' @param .times The number of times.
#' @param .pause The number of seconds to wait before the next attempt.
#' @export
#' @examplesIf interactive()
#' # read the GitHub releases info of the repo yihui/xfun
#' xfun::retry(xfun::github_releases, 'yihui/xfun')
retry = function(fun, ..., .times = 3, .pause = 5) {
  for (i in seq_len(.times)) {
    if (!inherits(res <- tryCatch(fun(...), error = identity), 'error'))
      return(res)
    Sys.sleep(.pause)
  }
  stop(res$message, call. = FALSE)
}

gsubi = function(...) gsub(..., ignore.case = TRUE)
gsubf = function(...) gsub(..., fixed = TRUE)

#' Turn the output of [str()] into a tree diagram
#'
#' The super useful function `str()` uses \verb{..} to indicate the level
#' of sub-elements of an object, which may be difficult to read. This function
#' uses vertical pipes to connect all sub-elements on the same level, so it is
#' clearer which elements belong to the same parent element in an object with a
#' nested structure (such as a nested list).
#' @param ... Arguments to be passed to [str()] (note that the
#'   `comp.str` is hardcoded inside this function, and it is the only
#'   argument that you cannot customize).
#' @return A character string as a [raw_string()].
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

pkg_file = function(...) system.file(..., package = 'xfun', mustWork = TRUE)

#' Format numbers of bytes using a specified unit
#'
#' Call the S3 method `format.object_size()` to format numbers of bytes.
#' @param x A numeric vector (each element represents a number of bytes).
#' @param units,... Passed to [`format()`][format.object_size].
#' @return A character vector.
#' @export
#' @examples
#' xfun::format_bytes(c(1, 1024, 2000, 1e6, 2e8))
#' xfun::format_bytes(c(1, 1024, 2000, 1e6, 2e8), units = 'KB')
format_bytes = function(x, units = 'auto', ...) {
  vapply(x, function(b) {
    format(structure(b, class = 'object_size'), units = units, ...)
  }, character(1))
}

# get the function name of the parent call
func_name = function(which = 1) {
  x = sys.call(which)[[1]]
  deparse(x)[1]
}

# evaluate an expression with an error handler; originally this was for knitr to
# output error location but can also be useful for other applications
handle_error = function(
  expr, handler, label = '', fun = getOption('xfun.handle_error.loc_fun')
) {
  on.exit(if (!ok) {
    loc = if (is.function(fun)) trimws(fun(label)) else ''
    m = handler(loc)
    message(one_string(m))
  })
  ok = FALSE
  expr  # evaluate now
  ok = TRUE
  expr  # won't be evaluated again
}

# a shorthand for rm(list =, envir =)
rm_vars = function(x, envir, ...) rm(list = x, envir = envir, ...)
