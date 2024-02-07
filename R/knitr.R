# functions extracted from knitr and to be reused in other packages like litedown

#' Parse comma-separated chunk options
#'
#' For \pkg{knitr} and R Markdown documents, code chunk options can be written
#' using the comma-separated syntax (e.g., `opt1=value1, opt2=value2`). This
#' function parses these options and returns a list. If an option is not named,
#' it will be treated as the chunk label.
#' @param x The chunk options as a string.
#' @return A list of chunk options.
#' @export
#' @examples
#' xfun::csv_options('foo, eval=TRUE, fig.width=5, echo=if (TRUE) FALSE')
csv_options = function(x) {
  x = one_string(x)
  res = handle_error(
    eval(parse_only(paste('alist(', quote_label(x), ')'))),
    function(e, loc) c(
      sprintf('Invalid syntax for chunk options%s:\n', loc), x,
      '\nPlease see documentation at https://yihui.org/knitr/options/.\n'
    )
  )
  idx = which(names(res) == '')  # which option is not named?
  # remove empty options
  j = NULL
  for (i in idx) if (identical(res[[i]], alist(,)[[1]])) j = c(j, i)
  if (length(j)) res[j] = NULL
  idx = if (is.null(names(res)) && length(res) == 1L) 1L else which(names(res) == '')
  if ((n <- length(idx)) > 1L || (length(res) > 1L && is.null(names(res)))) stop(
    'Invalid chunk options: ', x,
    "\n\nAll options must be of the form 'tag=value' except for the chunk label."
  )
  if (is.null(res$label)) {
    if (n == 0L) res$label = '' else names(res)[idx] = 'label'
  }
  if (!is.character(res$label))
    res$label = gsub(' ', '', as.character(as.expression(res$label)))
  if (res$label == '') res$label = NULL
  res
}

# quote the chunk label if necessary
quote_label = function(x) {
  x = gsub('^\\s*,?', '', x)
  if (grepl('^\\s*[^\'"](,|\\s*$)', x)) {
    # <<a,b=1>>= ---> <<'a',b=1>>=
    x = gsub('^\\s*([^\'"])(,|\\s*$)', "'\\1'\\2", x)
  } else if (grepl('^\\s*[^\'"](,|[^=]*(,|\\s*$))', x)) {
    # <<abc,b=1>>= ---> <<'abc',b=1>>=
    x = gsub('^\\s*([^\'"][^=]*)(,|\\s*$)', "'\\1'\\2", x)
  }
  x
}

# comment characters for various languages
comment_chars = list(
  `#` = c('awk', 'bash', 'coffee', 'gawk', 'julia', 'octave', 'perl', 'powershell', 'python', 'r', 'ruby', 'sed', 'stan'),
  '//' = c('asy', 'cc', 'csharp', 'd3', 'dot', 'fsharp', 'go', 'groovy', 'java', 'js', 'node', 'ojs', 'Rcpp', 'sass', 'scss', 'scala'),
  `%%` = c('mermaid'),
  `%` = c('matlab', 'tikz'),
  `/* */` = c('c', 'css'),
  `* ;` = c('sas'),
  `--` = c('haskell', 'lua', 'mysql', 'psql', 'sql'),
  `!` = c('fortran', 'fortran95'),
  `*` = c('stata')
)
# reshape it using the language name as the index, i.e., from list(char = lang)
# to list(lang = char)
comment_chars = local({
  res = list(apl = '\u235D')
  for (i in names(comment_chars)) {
    chars = comment_chars[[i]]
    res = c(res, setNames(rep(list(strsplit(i, ' ')[[1]]), length(chars)), chars))
  }
  res[order(names(res))]
})

get_option_comment = function(engine) {
  char = comment_chars[[engine]] %||% '#'
  s1 = paste0(char[[1]], '| ')
  s2 = ifelse(length(char) > 1, char[[2]], '')
  list(start = s1, end = s2)
}

#' Divide chunk options from the code chunk body
#'
#' Chunk options can be written in special comments (e.g., after `#|` for R code
#' chunks) inside a code chunk. This function partitions these options from the
#' chunk body.
#' @param engine The name of the language engine (to determine the appropriate
#'   comment character).
#' @param code A character vector (lines of code).
#' @return A list with the following items:
#'
#' - `options`: The parsed options (if there are any) as a list.
#' - `src`: The part of the input that contains the options.
#' - `code`: The part of the input that contains the code.
#'
#' @note Chunk options must be written on _continuous_ lines (i.e., all lines
#'   must start with the special comment prefix such as `#|`) at the beginning
#'   of the chunk body.
#' @export
#' @examples
#' # parse yaml-like items
#' yaml_like = c("#| label: mine", "#| echo: true", "#| fig.width: 8", "#| foo: bar", "1 + 1")
#' writeLines(yaml_like)
#' xfun::divide_chunk("r", yaml_like)
#'
#' # parse CSV syntax
#' csv_like = c("#| mine, echo = TRUE, fig.width = 8, foo = 'bar'", "1 + 1")
#' writeLines(csv_like)
#' xfun::divide_chunk("r", csv_like)
divide_chunk = function(engine, code) {
  res = list(options = NULL, src = NULL, code = code)
  # mask out empty blocks
  if (length(code) == 0) return(res)

  opt_comment = get_option_comment(engine)
  s1 = opt_comment$start
  s2 = opt_comment$end

  # check for option comments
  i1 = startsWith(code, s1)
  i2 = endsWith(trimws(code, 'right'), s2)
  # if "commentChar| " is not found, try "#| " instead
  if (!i1[1] && !identical(s1, '#|')) {
    s1 = '#| '; s2 = ''
    i1 = startsWith(code, s1); i2 = TRUE
  }
  m = i1 & i2

  # has to have at least one matched line at the beginning
  if (!m[[1]]) return(res)

  # divide into yaml and code
  if (all(m)) {
    src = code
    code = NULL
  } else {
    src = head(code, which.min(m) - 1)
    code = tail(code, -length(src))
  }

  # trim right
  if (any(i2)) src = trimws(src, 'right')

  # extract meta from comments, then parse it
  meta = substr(src, nchar(s1) + 1, nchar(src) - nchar(s2))
  # see if the metadata looks like YAML or CSV
  if (grepl('^[^ :]+:($|\\s)', meta[1])) {
    meta = yaml_load(meta, envir = FALSE)
    if (!is.list(meta) || length(names(meta)) == 0) {
      warning('Invalid YAML option format in chunk: \n', one_string(meta), '\n')
      meta = list()
    }
  } else {
    meta = csv_options(meta)
  }

  # normalize field name 'id' to 'label' if provided
  meta$label = unlist(meta[c('label', 'id')])[[1]]
  meta$id = NULL

  # extract code
  if (length(code) > 0 && is_blank(code[[1]])) {
    code = code[-1]
    src = c(src, '')
  }

  list(options = meta, src = src, code = code)
}
