#' Strict lists
#'
#' A strict list is essentially a normal \code{\link{list}()} but it does not
#' allow partial matching with \code{$}.
#'
#' To me, partial matching is often more annoying and surprising than
#' convenient. It can lead to bugs that are very hard to discover, and I have
#' been bitten for many times. When I write \code{x$name}, I always mean
#' precisely \code{name}. You should use a modern code editor to autocomplate
#' the \code{name} if it is too long to type, instead of using a partial name.
#' @param ... Objects (list elements), possibly named. Ignored in the
#'   \code{print()} method.
#' @export
#' @examples library(xfun)
#' (z = strict_list(aaa = 'I am aaa', b = 1:5))
#' z$a  # NULL!
#' z$aaa  # I am aaa
#' z$b
#' z$c = 'create a new element'
#'
#' z2 = unclass(z)  # a normal list
#' z2$a  # partial matching
strict_list = function(...) {
  structure(list(...), class = 'strict_list')
}

# https://twitter.com/xieyihui/status/782462926862954496

#' @param x A strict list.
#' @param name The name (a character string) of the list element.
#' @rdname strict_list
#' @export
`$.strict_list` = function(x, name) x[[name]]

#' @rdname strict_list
#' @export
print.strict_list = function(x, ...) {
  print(unclass(x))
}
