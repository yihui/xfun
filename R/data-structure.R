#' Strict lists
#'
#' A strict list is essentially a normal [list()] but it does not
#' allow partial matching with `$`.
#'
#' To me, partial matching is often more annoying and surprising than
#' convenient. It can lead to bugs that are very hard to discover, and I have
#' been bitten by it many times. When I write `x$name`, I always mean
#' precisely `name`. You should use a modern code editor to autocomplete
#' the `name` if it is too long to type, instead of using partial names.
#' @param ... Objects (list elements), possibly named. Ignored in the
#'   `print()` method.
#' @export
#' @return Both `strict_list()` and `as_strict_list()` return a list
#'   with the class `xfun_strict_list`. Whereas `as_strict_list()`
#'   attempts to coerce its argument `x` to a list if necessary,
#'   `strict_list()` just wraps its argument `...` in a list, i.e., it
#'   will add another list level regardless if `...` already is of type
#'   list.
#' @examples library(xfun)
#' (z = strict_list(aaa = 'I am aaa', b = 1:5))
#' z$a  # NULL!
#' z$aaa  # I am aaa
#' z$b
#' z$c = 'create a new element'
#'
#' z2 = unclass(z)  # a normal list
#' z2$a  # partial matching
#'
#' z3 = as_strict_list(z2) # a strict list again
#' z3$a  # NULL again!
strict_list = function(...) {
  as_strict_list(list(...))
}

# https://twitter.com/xieyihui/status/782462926862954496

#' @param x For `as_strict_list()`, the object to be coerced to a strict
#'   list.
#'
#'   For `print()`, a strict list.
#' @rdname strict_list
#' @export
as_strict_list = function(x) {
  structure(as.list(x), class = 'xfun_strict_list')
}

#' @param name The name (a character string) of the list element.
#' @rdname strict_list
#' @export
`$.xfun_strict_list` = function(x, name) x[[name]]

#' @rdname strict_list
#' @export
print.xfun_strict_list = function(x, ...) {
  print(unclass(x))
}

#' Print a character vector in its raw form
#'
#' The function `raw_string()` assigns the class `xfun_raw_string` to
#' the character vector, and the corresponding printing function
#' `print.xfun_raw_string()` uses `cat(x, sep = '\n')` to write the
#' character vector to the console, which will suppress the leading indices
#' (such as `[1]`) and double quotes, and it may be easier to read the
#' characters in the raw form (especially when there are escape sequences).
#' @param x For `raw_string()`, a character vector. For the print method,
#'   the `raw_string()` object.
#' @param ... Other attributes for `x`.
#' @export
#' @examples library(xfun)
#' raw_string(head(LETTERS))
#' raw_string(c('a "b"', 'hello\tworld!'))
raw_string = function(x, ...) {
  if (is.null(x)) x = as.character(x)
  class(x) = c('xfun_raw_string', class(x))
  structure(x, ...)
}

#' @param ... Other arguments (currently ignored).
#' @rdname raw_string
#' @export
print.xfun_raw_string = function(x, ...) {
  if (length(x)) cat(x, sep = '\n')
  invisible(x)
}

# provide default chunk options to litedown
#' @export
record_print.xfun_raw_string = function(x, ...) {
  cls = intersect(c(class(x), 'record_output'), .record_classes)[1]
  structure(x, class = cls, opts = list(comment = '', attr = attr(x, 'lang')))
}
