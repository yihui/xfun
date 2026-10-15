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
#' (z = strict_list(aaa = "I am aaa", b = 1:5))
#' z$a  # NULL!
#' z$aaa  # I am aaa
#' z$b
#' z$c = "create a new element"
#'
#' z2 = unclass(z)  # a normal list
#' z2$a  # partial matching
#'
#' z3 = as_strict_list(z2)  # a strict list again
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
#' raw_string(c('a "b"', "hello\tworld!"))
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
  structure(x, class = cls, opts = list(comment = '', attr = attr2(x, 'lang')))
}

#' Mixed-radix codes and group indices for row combinations
#'
#' `radix_id()` encodes each row as a numeric code via a mixed-radix key: each
#' vector contributes one digit in base `nlevels + 1`, with `NA` mapped to `0`
#' (its own group). Equal combinations share a code; use
#' `!duplicated(radix_id(...))` to flag the first occurrence of each. Faster
#' than `paste()` — no string allocation.
#'
#' `radix_group()` returns `match(radix_id(...), unique(radix_id(...)))`: a
#' 1-based group index in first-appearance order. Use `radix_id()` when only
#' `!duplicated()` is needed; `radix_group()` when the index itself matters.
#'
#' @param ... Equal-length vectors (factors, or coercible via [factor()]).
#' @return `radix_id()`: numeric vector; only equality comparisons meaningful.
#'   `radix_group()`: integer vector in `1:k` (`k` = distinct combinations).
#' @export
#' @examples
#' # radix_id(): equal combinations share a code; !duplicated() flags first occurrences
#' k = radix_id(c('b', 'a', 'b', 'a', 'c'))
#' !duplicated(k)  # TRUE FALSE TRUE FALSE TRUE
#'
#' # two vectors: combinations are matched row-wise
#' k2 = radix_id(c(1, 1, 2), c('x', 'x', 'y'))
#' k2[1] == k2[2]  # TRUE  (same combination)
#' k2[1] == k2[3]  # FALSE (different combination)
#'
#' # radix_group(): 1-based group index in first-appearance order
#' radix_group(c("b", "a", "b", "a", "c"))  # 1 2 1 2 3
#'
#' # expand a per-group result back to the full row order
#' x = c(3, 1, 3, 2, 1)
#' gid = radix_group(x)
#' u = !duplicated(gid)
#' per_group = x[u]^2  # 9 1 4 (one value per distinct group)
#' per_group[gid]      # 9 1 9 4 1 (expanded back)
radix_id = function(...) {
  key = 0
  for (x in list(...)) {
    f = if (is.factor(x)) x else factor(x)
    code = as.integer(f)
    code[is.na(code)] = 0L
    key = key * (nlevels(f) + 1L) + code
  }
  key
}

#' @rdname radix_id
#' @export
radix_group = function(...) {
  key = radix_id(...)
  match(key, unique(key))
}
