stop2 = function(...) stop(..., call. = FALSE)

warning2 = function(...) warning(..., call. = FALSE)

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

#' Set environment variables
#'
#' Set environment variables from a named character vector, and return the old
#' values of the variables, so they could be restored later.
#'
#' The motivation of this function is that \code{\link{Sys.setenv}()} does not
#' return the old values of the environment variables, so it is not
#' straightforward to restore the variables later.
#' @param vars A named character vector of the form \code{c(VARIABLE = VALUE)}.
#'   If any value is \code{NA}, this function will try to unset the variable.
#' @return Old values of the variables (if not set, \code{NA}).
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

#' Call \code{on.exit()} in a parent function
#'
#' The function \code{\link{on.exit}()} is often used to perform tasks when the
#' current function exits. This \code{exit_call()} function allows calling a
#' function when a parent function exits (thinking of it as inserting an
#' \code{on.exit()} call into the parent function).
#' @param fun A function to be called when the parent function exits.
#' @param n The parent frame number. For \code{n = 1}, \code{exit_call(fun)} is
#'   the same as \code{on.exit(fun())}; \code{n = 2} means adding
#'   \code{on.exit(fun())} in the parent function; \code{n = 3} means the
#'   grandparent, etc.
#' @param ... Other arguments to be passed to \code{on.exit()}.
#' @references This function was inspired by Kevin Ushey:
#'   \url{https://yihui.org/en/2017/12/on-exit-parent/}
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
  # TODO: remove this function in the future since stringsAsFactors starts to
  # default to FALSE since R 4.0.0
  if (isFALSE(getOption('stringsAsFactors'))) return(invisible())
  opts = options(stringsAsFactors = FALSE)
  exit_call(function() options(opts))
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
#' @examples # read the Github releases info of the repo yihui/xfun
#' if (interactive()) xfun::retry(xfun::github_releases, 'yihui/xfun')
retry = function(fun, ..., .times = 3, .pause = 5) {
  for (i in seq_len(.times)) {
    if (!inherits(res <- tryCatch(fun(...), error = identity), 'error'))
      return(res)
    Sys.sleep(.pause)
  }
  stop(res$message, call. = FALSE)
}

gsubi = function(...) gsub(..., ignore.case = TRUE)

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

pkg_file = function(...) system.file(..., package = 'xfun', mustWork = TRUE)

#' Format numbers of bytes using a specified unit
#'
#' Call the S3 method \code{format.object_size()} to format numbers of bytes.
#' @param x A numeric vector (each element represents a number of bytes).
#' @param units,... Passed to \code{\link[=format.object_size]{format}()}.
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
