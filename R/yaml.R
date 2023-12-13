#' Read YAML data
#'
#' If the \pkg{yaml} package is installed, use [yaml::yaml.load()] to read the
#' data. If not, use a simple parser instead, which only supports a limited
#' number of data types (see \dQuote{Examples}). In particular, it does not
#' support values that span across multiple lines (such as multi-line text).
#' @param x A character vector of YAML data.
#' @param ...,handlers Arguments to be passed to [yaml::yaml.load()].
#' @param envir The environment in which R expressions in YAML are evaluated. To
#'   disable the evaluation, use `envir = FALSE`.
#' @param use_yaml Whether to use the \pkg{yaml} package.
#' @return An R object (typically a list).
#' @note R expressions in YAML will be returned as [expression]s when they are
#'   not evaluated. This is different with [yaml::yaml.load()], which returns
#'   character strings for expressions.
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
yaml_load = function(
  x, ..., handlers = NULL, envir = parent.frame(), use_yaml = loadable('yaml')
) {
  if (use_yaml) return(handle_error(
    yaml::yaml.load(x, eval.expr = FALSE, handlers = yaml_handlers(handlers, envir), ...),
    function(e, loc) {
      s = e$message
      r = 'line (\\d+), column (\\d+)'
      m = regmatches(s, regexec(r, s, perl = TRUE))[[1]]
      if (length(m) < 3) return()
      m = as.integer(m[-1])  # c(row, col)
      c(
        sprintf('Failed to parse YAML%s:', loc), '',
        append(x, paste0(strrep(' ', m[2]), '^~~~~~'), m[1]), ''
      )
    }
  ))
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
    v = if (is_blank(val[i])) list() else yaml_value(val[i], envir)
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
yaml_value = function(x, envir = parent.frame()) {
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
    if (!is.na(v)) return(if ((v2 <- as.integer(v)) == v) v2 else v)
  }
  x = gsub('^["\']|["\']$', '', x)  # remove optional quotes for strings
  if (grepl(r <- '^!(r|expr) (.+)$', x)) {
    x = yaml_expr(gsub(r, '\\2', x), envir)
  }
  x
}

yaml_expr = function(x, envir) {
  x = parse_only(x)
  if (is.environment(envir)) x = eval(x, envir)
  x
}

# add !r and !expr handlers to support a custom eval environment, which
# yaml::yaml.load() doesn't support by default (vubiostat/r-yaml#54)
yaml_handlers = function(h, envir) {
  h = as.list(h)
  f = function(x) yaml_expr(x, envir)
  for (i in c('r', 'expr')) if (is.null(h[[i]])) h[[i]] = f
  h
}

#' Partition the YAML metadata and the body in a document
#'
#' Split a document into the YAML metadata (which starts with `---` in the
#' beginning of the document) and the body. The YAML metadata will be parsed.
#' @param x A character vector of the document content.
#' @param ... Arguments to be passed to `yaml_load()`.
#' @export
#' @return A list of components `yaml` and `body`.
#' @examples
#' xfun::yaml_body(c('---', 'title: Hello', 'output: markdown::html_document', '---', '', 'Content.'))
yaml_body = function(x, ...) {
  i = grep('^---\\s*$', x)
  n = length(x)
  res = if (n < 2 || length(i) < 2 || (i[1] > 1 && !all(is_blank(x[seq(i[1] - 1)])))) {
    list(yaml = list(), body = x)
  } else list(
    yaml = x[i[1]:i[2]], body = c(rep('', i[2]), tail(x, n - i[2]))
  )
  if ((n <- length(res$yaml)) >= 3) {
    res$yaml = yaml_load(res$yaml[-c(1, n)], ...)
  }
  res
}
