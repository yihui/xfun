#' Read YAML data
#'
#' If the \pkg{yaml} package is installed, use [yaml::yaml.load()] to read the
#' data. If not, use the simple parser [taml_load()] instead.
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
#' yaml_load('a: 1')
#' yaml_load('a: 1', use_yaml = FALSE)
yaml_load = function(
  x, ..., handlers = NULL, envir = parent.frame(), use_yaml = loadable('yaml')
) {
  if (!use_yaml) taml_load(x) else handle_error(
    yaml::yaml.load(x, eval.expr = FALSE, handlers = yaml_handlers(handlers, envir), ...),
    function(loc) {
      s = geterrmessage()
      r = 'line (\\d+), column (\\d+)'
      m = regmatches(s, regexec(r, s, perl = TRUE))[[1]]
      if (length(m) < 3) return()
      m = as.integer(m[-1])  # c(row, col)
      if (loc != '') loc = paste(' at lines', loc)
      c(
        sprintf('Failed to parse YAML%s:', loc), '',
        append(x, paste0(strrep(' ', m[2]), '^~~~~~'), m[1]), ''
      )
    }
  )
}

#' A simple YAML reader and writer
#'
#' TAML is a tiny subset of YAML. See
#' <https://yihui.org/litedown/#sec:yaml-syntax> for its specifications.
#' @param x For `taml_load()`, a character vector of the YAML content. For
#'   `taml_save()`, a list to be converted to YAML.
#' @param path A file path to read from or write to.
#' @inheritParams yaml_load
#' @return `taml_load()` and `taml_file()` return a list; if `path = NULL`,
#'   `taml_save()` returns a character vector, otherwise the vector is written
#'   to the file specified by the `path`.
#' @export
#' @examples
#' (res = taml_load('a: 1'))
#' taml_save(res)
#'
#' (res = taml_load('a: 1\nb: "foo"\nc: null'))
#' taml_save(res)
#'
#' (res = taml_load('a:\n  b: false\n  c: true\n  d: 1.234\ne: bar'))
#' taml_save(res)
#' taml_save(res, indent = '\t')
#'
#' taml_load('a: !expr paste(1:10, collapse = ", ")')
#' taml_load('a: [1, 3, 4, 2]')
#' taml_load('a: [1, "abc", 4, 2]')
#' taml_load('a: ["foo", "bar"]')
#' taml_load('a: [true, false, true]')
#' # the other form of array is not supported
#' taml_load('a:\n  - b\n  - c')
#' # and you must use the yaml package
#' if (loadable('yaml')) yaml_load('a:\n  - b\n  - c')
taml_load = function(x, envir = parent.frame()) {
  res = list()
  r = '^(\\s*)(.+?):($|\\s+.*)'
  x = split_lines(x)
  x = x[grep(r, x)]
  x = x[grep('^\\s*#', x, invert = TRUE)]  # comments
  if (length(x) == 0) return(res)
  lvl = gsub(r, '\\1', x)  # indentation level
  lvl = indent_level(lvl)
  key = gsub(r, '\\2', x)
  val = gsub('^\\s*|\\s*$', '', gsub(r, '\\3', x))
  keys = NULL
  for (i in seq_along(x)) {
    keys = c(head(keys, lvl[i]), key[i])
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

#' @rdname taml_load
#' @export
taml_file = function(path) taml_load(read_utf8(path))

#' @param indent A character string to indent sub-lists by one level.
#' @rdname taml_load
#' @export
taml_save = function(x, path = NULL, indent = '  ') {
  res = .taml_save(x, indent)
  if (is.null(path)) raw_string(res, lang = '.yaml') else write_utf8(res, path)
}

.taml_save = function(x, indent = '  ', level = 1) {
  if (is.list(x)) {
    nms = names(x)
    if (is.null(nms)) {
      str(x); stop('Lists must be named')
    }
    val = lapply(x, function(z) {
      if (!is.list(z)) .taml_save(z) else {
        .indent = strrep(indent, level)
        one_string(c('', paste0(.indent, .taml_save(z, indent, level + 1))))
      }
    })
    paste(nms, val, sep = ': ')
  } else {
    asis = inherits(x, 'AsIs')
    if (is.null(x)) return('null')
    if (is.logical(x)) x = tolower(x) else if (is.expression(x)) {
      x = unlist(lapply(x, deparse))
      x = paste('!expr', paste(x, collapse = '\\n'))
    } else if (is.numeric(x) || is.character(x)) {
      if (!is.numeric(x) && length(x)) x = paste0('"', x, '"')
    } else {
      str(x); stop("Unsupported data type ('", class(x)[1], "')")
    }
    if (length(x) == 1 && !asis) x else paste0('[', paste(x, collapse = ', '), ']')
  }
}

indent_level = function(x) {
  N = nchar(x); n = N[N > 0]
  if (length(n) == 0) N else ceiling(N / min(n))
}

# only support logical, numeric, character values (both scalar and [] arrays),
# and R expressions starting with !r/!expr
yaml_value = function(x, envir = parent.frame()) {
  v = tolower(x)
  if (v == 'null') return()
  if (yaml_bool(v)) return(as.logical(x))
  if (grepl(r <- '^\\s*\\[(.*)\\]\\s*$', v)) {
    v = gsub(r, '\\1', x)
    if (is_blank(v)) return()
    v = unname(unlist(read.csv(text = v, header = FALSE)))
    if (is.numeric(v)) return(v)
    v = gsub('^ ', '', v)  # [a, b] -> ["a", " b"] -> ["a", "b"]
    return(if (yaml_bool(v)) as.logical(v) else yaml_unquote(v))
  }
  if (grepl('^[0-9.e+-]', v)) {
    v = suppressWarnings(as.numeric(v))
    if (!is.na(v)) return(if ((v2 <- as.integer(v)) == v) v2 else v)
  }
  if (grepl(r <- '^!(r|expr) (.+)$', x)) {
    yaml_expr(gsub(r, '\\2', x), envir)
  } else yaml_unquote(x)
}

# convert true/false/na to boolean
yaml_bool = function(x) all(grepl('^true|false|na$', x))
# remove optional quotes for strings
yaml_unquote = function(x) gsub('^["\']|["\']$', '', x)

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
#' beginning of the document) and the body.
#' @param x A character vector of the document content.
#' @param ... Arguments to be passed to `yaml_load()`.
#' @param parse Whether to parse the YAML data.
#' @export
#' @return A list of components `yaml` (the YAML data), `lines` (starting and
#'   ending line numbers of YAML), and `body` (a character vector of the body
#'   text). If YAML metadata does not exist in the document, the components
#'   `yaml` and `lines` will be missing.
#' @examples
#' xfun::yaml_body(c('---', 'title: Hello', 'output: litedown::html_format', '---', '', 'Content.'))
yaml_body = function(x, ..., parse = TRUE) {
  n = length(x)
  res = if (length(i <- locate_yaml(x)) == 0) {
    list(body = x)
  } else list(
    yaml = x[i[1]:i[2]], body = c(rep('', i[2]), tail(x, n - i[2])), lines = i
  )
  if ((n <- length(res$yaml)) >= 2) {
    res$yaml = res$yaml[-c(1, n)]
    if (parse) res['yaml'] = list(yaml_load(res$yaml, ...))
  }
  res
}

# find lines of YAML
locate_yaml = function(x) {
  i = grep('^---\\s*$', x)
  if (length(i) > 1 && all(is_blank(x[seq_len(i[1] - 1)]))) i[1:2]
}
