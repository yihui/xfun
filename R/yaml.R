#' Read YAML data
#'
#' If the \pkg{yaml} package is installed, use [yaml::yaml.load()] to read the
#' data. If not, use a simple parser instead, which only supports a limited
#' number of data types (see \dQuote{Examples}). In particular, it does not
#' support values that span across multiple lines (such as multi-line text).
#' @param x A character vector of YAML data.
#' @param ... Arguments to be passed to [yaml::yaml.load()].
#' @param use_yaml Whether to use the \pkg{yaml} package.
#' @return An R object (typically a list).
#' @export
#' @examples
#' # test the simple parser without using the yaml package
#' read_yaml = function(...) xfun::yaml_load(..., use_yaml = FALSE)
#' read_yaml('a: 1')
#' read_yaml('a: 1\nb: "foo"\nc: null')
#' read_yaml('a:\n  b: false\n  c: true\n  d: 1.234\ne: bar')
#' read_yaml('a: !expr paste(1:10, collapse = ", ")')
#' read_yaml('a: [1, 3, 4, 2]')
#' read_yaml('a: [1, "abc", 4, 2]')
#' read_yaml('a: ["foo", "bar"]')
#' read_yaml('a: [true, false, true]')
#' # the other form of array is not supported
#' read_yaml('a:\n  - b\n  - c')
#' # and you must use the yaml package
#' if (loadable('yaml')) yaml_load('a:\n  - b\n  - c')
yaml_load = function(x, ..., use_yaml = loadable('yaml')) {
  if (use_yaml) {
    res = try_silent(yaml::yaml.load(x, eval.expr = TRUE, ...))
    if (!inherits(res, 'try-error')) return(res)
    warning(paste(c(x, '\nThe above YAML metadata may be invalid:\n', res), collapse = '\n'))
  }
  # the below simple parser is quite limited
  res = list()
  r = '^( *)([^ ]+?):($|\\s+.*)'
  x = split_lines(x)
  x = x[grep(r, x)]
  x = x[grep('^\\s*#', x, invert = TRUE)]  # comments
  if (length(x) == 0) return(res)
  lvl = gsub(r, '\\1', x)  # indentation level
  key = gsub(r, '\\2', x)
  val = gsub('^\\s*|\\s*$', '', gsub(r, '\\3', x))
  keys = NULL
  for (i in seq_along(x)) {
    keys = c(head(keys, nchar(lvl[i])/2), key[i])
    v = if (is_blank(val[i])) list() else yaml_value(val[i])
    # special treatment of NULL (to preserve a key with a null value)
    if (is.null(v)) {
      if (length(keys) <= 1) res[keys] = list(v) else {
        res[[head(keys, -1)]][tail(keys, 1)] = list(v)
      }
    } else res[[keys]] = v
  }
  res
}

# only support logical, numeric, character values (both scalar and [] arrays),
# and R expressions starting with !r/!expr
yaml_value = function(x) {
  v = tolower(x)
  if (v == 'null') return()
  if (grepl('^true|false$', v)) return(as.logical(x))
  if (grepl(r <- '^\\s*\\[(.*)\\]\\s*$', v)) {
    v = gsub(r, '\\1', v)
    if (is_blank(v)) return(list())
    v = unname(unlist(read.csv(text = v, header = FALSE)))
    if (is.numeric(v)) return(v)
    v = gsub('^ ', '', v)  # [a, b] -> ["a", " b"] -> ["a", "b"]
    return(if (all(grepl('^true|false$', v))) as.logical(v) else v)
  }
  if (grepl('^[0-9.e+-]', v)) {
    v = suppressWarnings(as.numeric(v))
    if (!is.na(v)) return(v)
  }
  x = gsub('^["\']|["\']$', '', x)  # remove optional quotes for strings
  if (grepl(r <- '^!(r|expr) (.+)$', x)) {
    x = eval(parse_only(gsub(r, '\\2', x)), parent.frame())
  }
  x
}

#' Partition the YAML metadata and the body in a document
#'
#' Split a document into the YAML metadata (which starts with `---` in the
#' beginning of the document) and the body. The YAML metadata will be parsed.
#' @param x A character vector of the document content.
#' @export
#' @return A list of components `yaml` and `body`.
#' @examples
#' xfun::yaml_body(c('---', 'title: Hello', 'output: markdown::html_document', '---', '', 'Content.'))
yaml_body = function(x) {
  i = grep('^---\\s*$', x)
  n = length(x)
  res = if (n < 2 || length(i) < 2 || (i[1] > 1 && !all(is_blank(x[seq(i[1] - 1)])))) {
    list(yaml = list(), body = x)
  } else list(
    yaml = x[i[1]:i[2]], body = c(rep('', i[2]), tail(x, n - i[2]))
  )
  if ((n <- length(res$yaml)) >= 3) {
    res$yaml = yaml_load(res$yaml[-c(1, n)])
  }
  res
}
