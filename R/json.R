#' A simple JSON serializer
#'
#' A JSON serializer that only works on a limited types of R data (`NULL`,
#' lists, arrays, logical/character/numeric/date/time vectors). Other types of
#' data will be coerced to character. A character string
#' wrapped in `js()` is treated as raw JavaScript, so will not be quoted. The
#' function `json_vector()` converts an atomic R vector to JSON.
#'
#' Both `NULL` and `NA` are converted to `null`. Named lists are converted to
#' objects of the form `{key1: value1, key2: value2, ...}`. Unnamed lists are
#' converted to arrays of the form `[[value1], [value2], ...]`. The same rules
#' apply to data frames since technically they are also lists. However, please
#' note that unnamed data frames (i.e., without column names) will be converted
#' to an array with each _row_ as an array element, whereas named data frames
#' will have each _column_ as an individual element. For matrices, the JSON
#' array will have each row as an individual element, and names are discarded.
#'
#' Dates and times are coerced to character using UTC as the timezone, and
#' represented via the JavaScript expression `new Date(value)` (which is not
#' standard JSON but practically more useful).
#' @param x An R object.
#' @param factor How to serialize factors. By default (`'string'`), a factor
#'   becomes an array of its character values. With `'dict'`, it is
#'   dictionary-encoded as a runnable JavaScript expression
#'   `[codes].map(i => [levels][i])` (0-based `codes` indexing a `levels`
#'   array), so the unique values are emitted once instead of repeated per
#'   element. Like dates, this is not strict JSON but is meant to be evaluated
#'   as JavaScript (e.g. embedded in a `<script>`).
#' @export
#' @return A character string.
#' @seealso The \pkg{jsonlite} package provides a full JSON serializer.
#' @examples library(xfun)
#' tojson(NULL); tojson(1:10); tojson(TRUE); tojson(FALSE)
#' tojson(list(a = 1, b = list(c = 1:3, d = 'abc')))
#' tojson(list(c('a', 'b'), 1:5, TRUE, Sys.Date() + 1:3))
#' tojson(head(iris))  # each column is in an element
#' tojson(unname(head(iris)))  # each row is in an element
#' tojson(matrix(1:12, 3))
#'
#' # dictionary-encode a factor
#' tojson(factor(c('b', 'a', 'b', NA, 'a')), factor = 'dict')
#'
#' # literal JS code
#' tojson(list(a = 1:5, b = js('function() {return true;}')))
tojson = function(x, factor = c('string', 'dict')) {
  if (inherits(x, 'json')) return(x)
  res = structure(.tojson(x, factor = match.arg(factor)), class = 'json')
  raw_string(res, lang = '.json')
}

.tojson = function(x, n = 1, factor = 'string') {
  make_array = function(..., braces = c('[', ']')) {
    inner = paste0(strrep('  ', n), ..., collapse = ',\n')
    paste0(braces[1], '\n', inner, '\n', strrep('  ', n - 1), braces[2])
  }
  if (is.null(x)) 'null' else if (is.array(x)) {
    # vectorize the common 2d matrix case (avoid one .tojson() call per row)
    if (length(dim(x)) == 2) {
      els = matrix(json_atomic(as.vector(x), FALSE), nrow(x))
      rows = do.call(paste, c(as.data.frame(els, stringsAsFactors = FALSE), sep = ', '))
      make_array(paste0('[', rows, ']'))
    } else make_array(apply(x, 1, .tojson, n + 1, factor))
  } else if (is.factor(x) && factor == 'dict') {
    # emit the unique values once as a runnable JS expression (see `factor`)
    lv = json_atomic(I(levels(x)))
    cd = json_atomic(I(as.integer(x) - 1L))
    # levels[null] is `undefined` in JS, so map NA codes back to null
    tail = if (anyNA(x)) ' ?? null' else ''
    paste0(cd, '.map(i => ', lv, '[i]', tail, ')')
  } else if (is.list(x)) {
    if (length(x) == 0) return('{}')
    # output unnamed data frames by rows instead of columns
    nms = names(x)
    is_df = is.data.frame(x)
    by_row = is_df && is.null(nms)
    cols = unlist(lapply(x, function(z) {
      if (by_row) json_atomic(z, FALSE) else {
        # data frame columns must be arrays even for length 1
        .tojson(if (is_df) I(z) else z, n + 1, factor)
      }
    }))
    if (is.null(nms)) {
      if (by_row) {
        dim(cols) = dim(x)
        cols = apply(cols, 1, json_vector, TRUE, FALSE)
      }
      make_array(cols)
    } else {
      make_array(quote_string(nms), ': ', cols, braces = c('{', '}'))
    }
  } else if (is.character(x) && inherits(x, c('JS_LITERAL', 'JS_EVAL'))) {
    paste(x, collapse = '\n')
  } else json_atomic(x)
}

#' @rdname tojson
#' @export
js = function(x) structure(x, class = 'JS_LITERAL')

json_atomic = function(x, to_array = NA) {
  use_quote = !(is.numeric(x) || is.logical(x))
  asis = inherits(x, 'AsIs')
  if (is.factor(x)) x = as.character(x)
  if (is.logical(x)) x = tolower(as.character(x))
  if (inherits(x, c('Date', 'POSIXct', 'POSIXt'))) {
    if (asis) class(x) = setdiff(class(x), 'AsIs')
    x = sprintf('new Date("%s")', format(x, tz = 'UTC'))
    use_quote = FALSE
  }
  if (is.na(to_array)) to_array = length(x) != 1 || asis
  json_vector(x, to_array, use_quote)
}

#' @param to_array Whether to convert a vector to a JSON array (use `[]`).
#' @param quote Whether to double quote the elements.
#' @rdname tojson
#' @export
json_vector = function(x, to_array = FALSE, quote = TRUE) {
  i = is.na(x)
  if (quote) {
    x = quote_string(x)
    # escape control chars only when present (avoid 5 full passes otherwise)
    if (any(grepl('[\n\b\f\r\t]', x, useBytes = TRUE))) {
      x = gsubf('\n', '\\n', x)
      x = gsubf('\b', '\\b', x)
      x = gsubf('\f', '\\f', x)
      x = gsubf('\r', '\\r', x)
      x = gsubf('\t', '\\t', x)
    }
  } else if (is.numeric(x) && any(is.infinite(x))) {
    x = ifelse(is.infinite(x), ifelse(x > 0, 'Infinity', '-Infinity'), x)
  }
  x[i] = 'null'
  if (to_array) paste0('[', paste(x, collapse = ', '), ']') else x
}

# escape \ and " in strings, and quote them
quote_string = function(x) {
  # only escape when backslashes or double quotes are actually present
  if (any(grepl('["\\]', x, useBytes = TRUE))) x = gsub('(["\\])', "\\\\\\1", x)
  # escape </script (case-insensitively, as the HTML parser matches it) so the
  # output can't close an inline <script> block early; \/ is still valid JSON
  if (any(grepl('</script', x, ignore.case = TRUE)))
    x = gsub('</(script)', '<\\\\/\\1', x, perl = TRUE, ignore.case = TRUE)
  if (length(x)) x = paste0('"', x, '"')
  x
}
