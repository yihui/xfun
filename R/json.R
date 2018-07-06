#' A simple JSON serializer
#'
#' A JSON serializer that only works on a limited types of R data (\code{NULL},
#' lists, logical scalars, character/numeric vectors). A character string of the
#' class \code{JS_EVAL} is treated as raw JavaScript, so will not be quoted. The
#' function \code{json_vector()} converts an atomic R vector to JSON.
#' @param x An R object.
#' @export
#' @return A character string.
#' @seealso The \pkg{jsonlite} package provides a full JSON serializer.
#' @examples library(xfun)
#' tojson(NULL); tojson(1:10); tojson(TRUE); tojson(FALSE)
#' cat(tojson(list(a = 1, b = list(c = 1:3, d = 'abc'))))
#' cat(tojson(list(c('a', 'b'), 1:5, TRUE)))
#'
#' # the class JS_EVAL is originally from htmlwidgets::JS()
#' JS = function(x) structure(x, class = 'JS_EVAL')
#' cat(tojson(list(a = 1:5, b = JS('function() {return true;}'))))
tojson = function(x) {
  if (is.null(x)) return('null')
  if (is.logical(x)) {
    if (length(x) != 1 || any(is.na(x)))
      stop('Logical values of length > 1 and NA are not supported')
    return(tolower(as.character(x)))
  }
  if (is.character(x) && inherits(x, 'JS_EVAL')) return(paste(x, collapse = '\n'))
  if (is.character(x) || is.numeric(x)) {
    return(json_vector(x, length(x) != 1 || inherits(x, 'AsIs'), is.character(x)))
  }
  if (is.list(x)) {
    if (length(x) == 0) return('{}')
    return(if (is.null(names(x))) {
      json_vector(unlist(lapply(x, tojson)), TRUE, quote = FALSE)
    } else {
      nms = paste0('"', names(x), '"')
      paste0('{\n', paste(nms, unlist(lapply(x, tojson)), sep = ': ', collapse = ',\n'), '\n}')
    })
  }
  stop('The class of x is not supported: ', paste(class(x), collapse = ', '))
}

#' @param to_array Whether to convert a vector to a JSON array (use \code{[]}).
#' @param quote Whether to double quote the elements.
#' @rdname tojson
#' @export
json_vector = function(x, to_array = FALSE, quote = TRUE) {
  if (quote) {
    x = gsub('(["\\])', "\\\\\\1", x)
    x = gsub('[[:space:]]', " ", x)
    if (length(x)) x = paste0('"', x, '"')
  }
  if (to_array) paste0('[', paste(x, collapse = ', '), ']') else x
}
