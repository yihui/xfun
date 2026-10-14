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
#' @param dict Whether to dictionary-encode atomic vectors (and data-frame
#'   columns) to shrink the output when values repeat. Defaults to the global
#'   option `xfun.tojson.dict` (and to `FALSE` when that is unset), so a package
#'   that embeds `tojson()` output deep inside another function can turn
#'   encoding on without the call being reachable, e.g.
#'   `options(xfun.tojson.dict = 0.5)`. `FALSE`
#'   serializes normally. A number in `(0, 1]` acts as a cardinality threshold:
#'   a vector is encoded when its number of unique values is at most `dict`
#'   times its length, and only when the encoding is actually shorter than the
#'   plain form (so near-unique vectors are left alone). `TRUE` means `1`
#'   (consider every vector). An encoded vector becomes a runnable JavaScript
#'   expression `[codes].map(i => [u][i])` (0-based `codes` indexing the unique
#'   values `u`), which is not strict JSON but meant to be evaluated as
#'   JavaScript (e.g. embedded in a `<script>`).
#' @param pretty Whether to pretty-print the output with line breaks,
#'   indentation, and a space after each array/object separator. Defaults to the
#'   global option `xfun.tojson.pretty` (and to `TRUE` when that is unset), so an
#'   embedding package can turn pretty-printing off without reaching the call,
#'   e.g. `options(xfun.tojson.pretty = FALSE)`. `FALSE` produces a compact,
#'   single-line result (no line breaks or indentation, and `,` instead of `, `
#'   between array elements), which can be substantially smaller for large data.
#' @param pack Whether to pack long integer vectors as base64 (little-endian
#'   bytes, 1/2/4 bytes per value chosen by range) to shrink the output and speed
#'   up parsing. Defaults to the global option `xfun.tojson.pack` (and to `Inf`
#'   when that is unset). Either a number giving the minimum vector length to pack
#'   (`Inf` disables packing) or a `list(mode, threshold)` (either field optional),
#'   where `mode` is `"json"` (default) or `"js"` (see below) and `threshold` is
#'   the length cutoff (default `100`).
#'
#'   In mode `"json"` a packed vector is valid JSON `{"<tag>": "<base64>"}`, where
#'   the tag is a fixed prefix (a leading control char, unlikely to collide with a
#'   real key) plus `u8`, `u16`, `u32` for unsigned or `i8`, `i16`, `i32` for
#'   signed integers. A consumer turns it into a typed array with the JavaScript
#'   `json_unpack` (defines `unpack(o)`, decode one packed object on demand) or
#'   `json_reviver` (also defines `reviver`, for `JSON.parse(text, reviver)`).
#'   Prefer lazy `unpack()` over `reviver` for large payloads: a reviver fires on
#'   *every* parsed value, so attaching it to a document that still holds large
#'   *plain* arrays can make `JSON.parse()` an order of magnitude slower (vs. no
#'   reviver); on a mostly-packed document the two cost about the same. Vectors
#'   containing `NA` are not packed, and packing is used only when it shortens the
#'   output. With `dict`, the codes are packed and the vector becomes
#'   `{"<tag>u": [uniques], "<tag>c": packed}`, which `reviver` expands to an array.
#'
#'   Mode `"js"` instead returns a self-contained JavaScript expression: each
#'   packed vector becomes an `unpack({...})` call (and a dict vector
#'   `Array.from(unpack({...}), i => u[i])`), with `unpack()` defined once in a
#'   wrapping `(() => {...})()`. The whole result is a single expression to be
#'   evaluated (not `JSON.parse`d), so the consumer needs no reviver and the tags
#'   appear only as call arguments, never as standalone data. Use this when the
#'   output is embedded as a JS value; use `"json"` when it must stay valid JSON.
#' @export
#' @return A character string.
#' @seealso The \pkg{jsonlite} package provides a full JSON serializer.
#' @examples library(xfun)
#' tojson(NULL); tojson(1:10); tojson(TRUE); tojson(FALSE)
#' tojson(list(a = 1, b = list(c = 1:3, d = "abc")))
#' tojson(list(c("a", "b"), 1:5, TRUE, Sys.Date() + 1:3))
#' tojson(head(iris))  # each column is in an element
#' tojson(unname(head(iris)))  # each row is in an element
#' tojson(matrix(1:12, 3))
#'
#' # dictionary-encode repeated values
#' tojson(rep(c("b", "a", NA), 3), dict = TRUE)
#'
#' # literal JS code
#' tojson(list(a = 1:5, b = js("function() {return true;}")))
tojson = function(x, dict = getOption('xfun.tojson.dict', FALSE),
                  pretty = getOption('xfun.tojson.pretty', TRUE),
                  pack = getOption('xfun.tojson.pack', Inf)) {
  if (inherits(x, 'json')) return(x)
  if (isTRUE(dict)) dict = 1 else if (isFALSE(dict)) dict = 0
  # `pack`: a length threshold number, or a list(mode, threshold)
  if (is.numeric(pack)) pack = list(threshold = pack)
  mode = pack$mode %||% 'json'
  res = .tojson(x, dict = dict, pretty = pretty, pack = pack$threshold %||% 100, mode = mode)
  # in 'js' mode, wrap in an IIFE defining unpack() once -- but only if a value
  # was packed (an `unpack(` call is in the output)
  if (mode == 'js' && grepl('unpack(', res, fixed = TRUE))
    res = paste0('(() => {\n', json_unpack, '\nreturn ', res, '\n})()')
  raw_string(structure(res, class = 'json'), lang = '.json')
}

# a control char is unlikely to clash with a real object key
pack_prefix = '\\u0001'

.tojson = function(x, n = 1, dict = 0, pretty = TRUE, pack = Inf, mode = 'json') {
  sep = if (pretty) ', ' else ','       # between array elements
  kv = if (pretty) ': ' else ':'        # between an object key and its value
  make_array = function(..., braces = c('[', ']')) {
    if (pretty) {
      inner = paste0(strrep('  ', n), ..., collapse = ',\n')
      paste0(braces[1], '\n', inner, '\n', strrep('  ', n - 1), braces[2])
    } else {
      paste0(braces[1], paste0(..., collapse = ','), braces[2])
    }
  }
  if (is.null(x)) 'null' else if (is.array(x)) {
    # vectorize the common 2d matrix case (avoid one .tojson() call per row)
    if (length(dim(x)) == 2) {
      els = matrix(json_atomic(as.vector(x), FALSE, sep), nrow(x))
      rows = do.call(paste, c(as.data.frame(els, stringsAsFactors = FALSE), sep = sep))
      make_array(paste0('[', rows, ']'))
    } else make_array(apply(x, 1, .tojson, n + 1, dict, pretty, pack, mode))
  } else if (is.list(x)) {
    if (length(x) == 0) return('{}')
    # output unnamed data frames by rows instead of columns
    nms = names(x)
    is_df = is.data.frame(x)
    by_row = is_df && is.null(nms)
    cols = unlist(lapply(x, function(z) {
      if (by_row) json_atomic(z, FALSE, sep) else {
        # data frame columns must be arrays even for length 1
        .tojson(if (is_df) I(z) else z, n + 1, dict, pretty, pack, mode)
      }
    }))
    if (is.null(nms)) {
      if (by_row) {
        dim(cols) = dim(x)
        cols = apply(cols, 1, json_vector, TRUE, FALSE, sep)
      }
      make_array(cols)
    } else {
      make_array(quote_string(nms), kv, cols, braces = c('{', '}'))
    }
  } else if (is.character(x) && inherits(x, c('JS_LITERAL', 'JS_EVAL', 'json'))) {
    # js() and tojson() results are emitted raw, not re-quoted
    paste(x, collapse = '\n')
  } else {
    plain = json_atomic(x, sep = sep)
    if (can_pack(x, pack)) {
      p = pack_ints(x)
      if (mode == 'js') p = paste0('unpack(', p, ')')
      if (nchar(p) < nchar(plain)) plain = p
    }
    d = if (dict > 0) dict_atomic(x, dict, sep, pack, mode)
    if (length(d) && nchar(d) < nchar(plain)) d else plain
  }
}

# dictionary-encode an atomic vector as `[codes].map(i => [u][i])` when it has
# enough repeats (unique values at most `dict` times the length); return NULL to
# signal the caller to keep the plain form. unique() keeps NA, so NA becomes a
# normal entry (`u[code]` is `null`) and needs no special casing.
dict_atomic = function(x, dict, sep = ', ', pack = Inf, mode = 'json') {
  n = length(x)
  if (n == 0) return()
  u = unique(x)
  if (length(u) > dict * n) return()
  codes = match(x, u) - 1L
  u = json_atomic(I(u), sep = sep)
  if (can_pack(codes, pack)) {
    if (mode == 'js')
      return(paste0('Array.from(unpack(', pack_ints(codes), '), i => ', u, '[i])'))
    return(paste0('{"', pack_prefix, 'u":', u, ',"', pack_prefix, 'c":', pack_ints(codes), '}'))
  }
  paste0(json_atomic(I(codes), sep = sep), '.map(i => ', u, '[i])')
}

# plain integer vectors (not factors, no NA) longer than the threshold
can_pack = function(x, pack) {
  length(x) > pack && is.integer(x) && !anyNA(x) &&
    identical(setdiff(class(x), 'AsIs'), 'integer')
}

# pack an integer vector into a tagged base64 object of little-endian bytes,
# using the smallest unsigned (or, with negatives, signed) width
pack_ints = function(x) {
  r = range(x)
  w = if (r[1] >= 0) {
    if (r[2] <= 255L) 1L else if (r[2] <= 65535L) 2L else 4L
  } else {
    if (r[1] >= -128L && r[2] <= 127L) 1L else
      if (r[1] >= -32768L && r[2] <= 32767L) 2L else 4L
  }
  tag = paste0(if (r[1] >= 0) 'u' else 'i', 8L * w)
  b = writeBin(as.integer(x), raw(), size = w, endian = 'little')
  sprintf('{"%s%s":"%s"}', pack_prefix, tag, base64_encode(b))
}

#' @rdname tojson
#' @format `json_unpack` and `json_reviver` are character strings of JavaScript
#'   source.
#' @export
# slice(1) drops the one-char tag prefix
json_unpack = paste(
  'const W = {u8: Uint8Array, u16: Uint16Array, u32: Uint32Array, i8: Int8Array, i16: Int16Array, i32: Int32Array},',
  'unpack = o => { for (const k in o) { const T = W[k.slice(1)]; if (!T) continue;',
  '  const s = atob(o[k]), b = new Uint8Array(s.length);',
  '  for (let i = 0; i < b.length; i++) b[i] = s.charCodeAt(i);',
  '  return new T(b.buffer); } };',
  sep = '\n'
)

#' @rdname tojson
#' @export
# a reviver sees every value, so test k[0] === _P to tell a packed object from a
# real one (mode 'js' needs no such guard)
json_reviver = paste(
  json_unpack,
  sprintf('const _P = "%s", _U = "%su", _C = "%sc",', pack_prefix, pack_prefix, pack_prefix),
  'reviver = (k, v) => {',
  '  if (!v || typeof v !== "object" || Array.isArray(v)) return v;',
  '  if (_U in v && _C in v) return Array.from(v[_C], i => v[_U][i]);',
  '  for (const k in v) if (k[0] === _P) return unpack(v) ?? v;',
  '  return v; };',
  sep = '\n'
)

#' @rdname tojson
#' @export
js = function(x) structure(x, class = 'JS_LITERAL')

json_atomic = function(x, to_array = NA, sep = ', ') {
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
  json_vector(x, to_array, use_quote, sep)
}

#' @param to_array Whether to convert a vector to a JSON array (use `[]`).
#' @param quote Whether to double quote the elements.
#' @param sep The string separating consecutive array elements (`', '` by
#'   default; use `','` for compact output).
#' @rdname tojson
#' @export
json_vector = function(x, to_array = FALSE, quote = TRUE, sep = ', ') {
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
  if (to_array) paste0('[', paste(x, collapse = sep), ']') else x
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
