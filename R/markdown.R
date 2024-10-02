#' Find the indices of lines in Markdown that are prose (not code blocks)
#'
#' Filter out the indices of lines between code block fences such as \verb{```}
#' (could be three or four or more backticks).
#' @param x A character vector of text in Markdown.
#' @param warn Whether to emit a warning when code fences are not balanced.
#' @note If the code fences are not balanced (e.g., a starting fence without an
#'   ending fence), this function will treat all lines as prose.
#' @return An integer vector of indices of lines that are prose in Markdown.
#' @export
#' @examples library(xfun)
#' prose_index(c('a', '```', 'b', '```', 'c'))
#' prose_index(c('a', '````', '```r', '1+1', '```', '````', 'c'))
prose_index = function(x, warn = TRUE) {
  idx = NULL
  # if raw HTML <pre></pre> exists, it should be treated as code block
  inside_pre = if (length(p1 <- grep('<pre>', x))) {
    p2 = grep('</pre>', x)
    if (length(p1) == length(p2)) {
      idx = rbind(p1, p2)
      function(i) any(i > p1 & i < p2)
    }
  }
  r = '^(\\s*```+).*'; s = ''
  # shouldn't match ``` ``text``` ```, which is inline code, not code block
  i1 = grepl(r, x); i2 = !grepl('^\\s*```+\\s+`', x); i3 = !grepl('-->\\s*$', x)
  for (i in which(i1 & i2 & i3)) {
    if (is.function(inside_pre) && inside_pre(i)) next
    if (s == '') {
      s = gsub(r, '\\1', x[i]); idx = c(idx, i); next
    }
    # look for the next line with the same amount of backticks (end of block)
    if (grepl(paste0('^', s), x[i])) {
      idx = c(idx, i); s = ''
    }
  }
  xi = seq_along(x); n = length(idx)
  if (n == 0) return(xi)
  if (n %% 2 != 0) {
    if (warn) warning('Code fences are not balanced')
    # treat all lines as prose
    return(xi)
  }
  idx2 = matrix(idx, nrow = 2)
  idx2 = unlist(mapply(seq, idx2[1, ], idx2[2, ], SIMPLIFY = FALSE))
  xi[-idx2]
}

#' Protect math expressions in pairs of backticks in Markdown
#'
#' For Markdown renderers that do not support LaTeX math, we need to protect
#' math expressions as verbatim code (in a pair of backticks), because some
#' characters in the math expressions may be interpreted as Markdown syntax
#' (e.g., a pair of underscores may make text italic). This function detects
#' math expressions in Markdown (by heuristics), and wrap them in backticks.
#'
#' Expressions in pairs of dollar signs or double dollar signs are treated as
#' math, if there are no spaces after the starting dollar sign, or before the
#' ending dollar sign. There should be spaces before the starting dollar sign,
#' unless the math expression starts from the very beginning of a line. For a
#' pair of single dollar signs, the ending dollar sign should not be followed by
#' a number. With these assumptions, there should not be too many false
#' positives when detecing math expressions.
#'
#' Besides, LaTeX environments (\verb{\begin{*}} and \verb{\end{*}}) are also
#' protected in backticks.
#' @param x A character vector of text in Markdown.
#' @param token A character string to wrap math expressions at both ends. This
#'   can be a unique token so that math expressions can be reliably identified
#'   and restored after the Markdown text is converted.
#' @return A character vector with math expressions in backticks.
#' @note If you are using Pandoc or the \pkg{rmarkdown} package, there is no
#'   need to use this function, because Pandoc's Markdown can recognize math
#'   expressions.
#' @export
#' @examples library(xfun)
#' protect_math(c('hi $a+b$', 'hello $$\\alpha$$', 'no math here: $x is $10 dollars'))
#' protect_math(c('hi $$', '\\begin{equation}', 'x + y = z', '\\end{equation}'))
#' protect_math('$a+b$', '===')
protect_math = function(x, token = '') {
  i = prose_index(x)
  if (length(i)) x[i] = escape_math(x[i], token)
  x
}

escape_math = function(x, token = '') {
  # replace $x$ with `\(x\)` (protect inline math in <code></code>)
  m = gregexpr('(?<=^|[\\s])[$](?! )[^$]+?(?<! )[$](?![$0123456789])', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    z = sub('^[$]', paste0('`', token, '\\\\('), z)
    z = sub('[$]$', paste0('\\\\)', token, '`'), z)
    z
  })
  # replace $$x$$ with `$$x$$` (protect display math)
  m = gregexpr('(?<=^|[\\s])[$][$](?! )[^$]+?(?<! )[$][$]', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    paste0('`', token, z, token, '`')
  })
  # now, if there are still lines starting and ending with $$, they might be
  # math expressions of display style spanning multiple lines, e.g.,
  # $$\alpha +
  # \beta$$
  # we assume that $$ can only appear once on one line
  i = vapply(gregexpr('[$]', x), length, integer(1)) == 2
  if (any(i)) {
    x[i] = gsub('^(\\s*)([$][$][^ ]+)', paste0('\\1`', token, '\\2'), x[i], perl = TRUE)
    x[i] = gsub('([^ ][$][$])$', paste0('\\1', token, '`'), x[i], perl = TRUE)
  }
  # equation environments (\begin and \end must match)
  i1 = grep('^\\\\begin\\{[^}]+\\}$', x)
  i2 = grep('^\\\\end\\{[^}]+\\}$', x)
  if (length(i1) == length(i2)) {
    # TODO: do not protect inner environments in case of nested environments (#57)
    x[i1] = paste0('`', token, x[i1])
    x[i2] = paste0(x[i2], token, '`')
  }
  x
}

#' Create a fenced block in Markdown
#'
#' Wrap content with fence delimiters such as backticks (code blocks) or colons
#' (fenced Div). Optionally the fenced block can have attributes. The function
#' `fenced_div()` is a shorthand of `fenced_block(char = ':')`.
#' @param x A character vector of the block content.
#' @param attrs A vector of block attributes.
#' @param fence The fence string, e.g., `:::` or ```` ``` ````. This will be
#'   generated from the `char` argument by default.
#' @param char The fence character to be used to generate the fence string by
#'   default.
#' @return `fenced_block()` returns a character vector that contains both the
#'   fences and content.
#' @export
#' @examples
#' # code block with class 'r' and ID 'foo'
#' xfun::fenced_block('1+1', c('.r', '#foo'))
#' # fenced Div
#' xfun::fenced_block('This is a **Div**.', char = ':')
fenced_block = function(x, attrs = NULL, fence = make_fence(x, char), char = '`') {
  a = block_attr(attrs)
  c('', paste0(fence, a), x, fence)
}

#' @param ... Arguments to be passed to `fenced_block()`.
#' @rdname fenced_block
#' @export
fenced_div = function(...) fenced_block(..., char = ':')

#' @return `make_fence()` returns a character string. If the block content
#'   contains `N` fence characters (e.g., backticks), use `N + 1` characters as
#'   the fence.
#' @rdname fenced_block
#' @export
#' @examples
#' # three backticks by default
#' xfun::make_fence('1+1')
#' # needs five backticks for the fences because content has four
#' xfun::make_fence(c('````r', '1+1', '````'))
make_fence = function(x, char = '`') {
  f = strrep(char, 3)
  while (any(grepl(f, x, fixed = TRUE))) f = paste0(f, char)
  f
}

# concatenate block attributes for fenced blocks
block_attr = function(attrs) {
  a = paste(attrs, collapse = ' ')
  if (grepl('[ .=#]', a)) a = paste0('{', a, '}')
  if (a == '') a else paste0(' ', a)
}

#' Embed a file, multiple files, or directory on an HTML page
#'
#' For a file, first encode it into base64 data (a character string). Then
#' generate a hyperlink of the form \samp{<a href="base64 data"
#' download="filename">Download filename</a>}. The file can be downloaded when
#' the link is clicked in modern web browsers. For a directory, it will be
#' compressed as a zip archive first, and the zip file is passed to
#' `embed_file()`. For multiple files, they are also compressed to a zip file
#' first.
#'
#' These functions can be called in R code chunks in R Markdown documents with
#' HTML output formats. You may embed an arbitrary file or directory in the HTML
#' output file, so that readers of the HTML page can download it from the
#' browser. A common use case is to embed data files for readers to download.
#' @param path Path to the file(s) or directory.
#' @param name The default filename to use when downloading the file. Note that
#'   for `embed_dir()`, only the base name (of the zip filename) will be used.
#' @param text The text for the hyperlink.
#' @param ... For `embed_file()`, additional arguments to be passed to
#'   [html_tag()] (e.g., `class = 'foo'`). For `embed_dir()` and
#'   `embed_files()`, arguments passed to `embed_file()`.
#' @note Windows users may need to install Rtools to obtain the \command{zip}
#'   command to use `embed_dir()` and `embed_files()`.
#'
#'   Currently Internet Explorer does not support downloading embedded files
#'   (<https://caniuse.com/download>). Chrome has a 2MB limit on the file size.
#' @return An HTML tag \samp{<a>} with the appropriate attributes.
#' @export
#' @examples
#' logo = xfun:::R_logo()
#' link = xfun::embed_file(logo, text = 'Download R logo')
#' link
#' if (interactive()) xfun::html_view(link)
embed_file = function(path, name = basename(path), text = paste('Download', name), ...) {
  h = base64_uri(path)
  html_tag('a', text, href = h, download = name, ...)
}

#' @rdname embed_file
#' @export
embed_dir = function(path, name = paste0(normalize_path(path), '.zip'), ...) {
  name  = gsub('/', '', basename(name))
  in_dir(path, {
    name = file.path(tempdir(), name); on.exit(file.remove(name), add = TRUE)
    zip(name, '.'); embed_file(name, ...)
  })
}

#' @rdname embed_file
#' @export
embed_files = function(path, name = with_ext(basename(path[1]), '.zip'), ...) {
  name = file.path(tempdir(), basename(name))
  on.exit(file.remove(name), add = TRUE)
  zip(name, path)
  embed_file(name, ...)
}

zip = function(name, ...) {
  if (utils::zip(name, ...) != 0) stop('Failed to create the zip archive ', name)
  invisible(0)
}

#' Generate a simple Markdown pipe table
#'
#' A minimal Markdown table generator using the pipe `|` as column separators.
#'
#' The default argument values can be set via global options with the prefix
#' `xfun.md_table.`, e.g., `options(xfun.md_table.digits 2, xfun.md_table.na =
#' 'n/a')`.
#' @param x A 2-dimensional object (e.g., a matrix or data frame).
#' @param digits The number of decimal places to be passed to [round()]. It can
#'   be a integer vector of the same length as the number of columns in `x` to
#'   round columns separately. The default is `3`.
#' @param na A character string to represent `NA` values. The default is an
#'   empty string.
#' @param newline A character string to substitute `\n` in `x` (because pipe
#'   tables do not support line breaks in cells). The default is a space.
#' @param limit The maximum number of rows to show in the table. If it is
#'   smaller than the number of rows, the data in the middle will be omitted. If
#'   it is of length 2, the second number will be used to limit the number of
#'   columns. Zero and negative values are ignored.
#' @return A character vector.
#' @seealso [knitr::kable()] (which supports more features)
#' @export
#' @examples
#' xfun::md_table(head(iris))
#' xfun::md_table(mtcars, limit = c(10, 6))
md_table = function(x, digits = NULL, na = NULL, newline = NULL, limit = NULL) {
  if (length(d <- dim(x)) != 2)
    stop('xfun::md_table() only supports 2-dimensional objects.')
  if (d[2] == 0) return(character())
  if (is.null(digits))
    digits = getOption('xfun.md_table.digits', min(getOption('digits'), 3))
  digits = rep(digits, d[2])  # recycle for all columns
  num = logical(d[2])  # numeric columns
  for (j in seq_len(d[2])) if (is.numeric(x[, j])) {
    num[j] = TRUE
    x[, j] = round(x[, j], digits[j])
  }
  is_na = is.na(x)
  x = as.matrix(format(x))
  if (any(is_na)) x[is_na] = na %||% getOption('xfun.md_table.na', '')
  rn = rownames(x)
  # ignore empty and automatic row names (integers)
  if (!all(grepl('^[0-9]*$', rn))) {
    x = cbind(' ' = rn, x)
    num = c(FALSE, num)
  }
  # get first and last limit/2 rows/cols in N rows/cols
  if (length(limit <- limit %||% getOption('xfun.md_table.limit'))) {
    # subset rows
    l1 = limit[1]
    if (l1 > 0 && l1 < d[1]) {
      n1 = round(l1/2); n2 = l1 - n1
      x = rbind(head(x, n1), '&vellip;', tail(x, n2))
    }
    # subset columns
    if (length(limit) >= 2 && (l2 <- limit[2]) > 0 && l2 < d[2]) {
      n1 = round(l2/2); n2 = l2 - n1
      x = cbind(
        x[, seq_len(n1), drop = FALSE],
        `...` = '...',
        x[, d[2] - seq_len(n2) + 1, drop = FALSE]
      )
      num = c(head(num, n1), FALSE, tail(num, n2))
    }
    d = dim(x)
    if (d[2] == 0) return(character())
  }
  cn = colnames(x) %||% rep(' ', d[2])  # table header
  a = ifelse(num, '--:', '---')  # alignment
  a[cn == '...'] = ':-:'
  x = rbind(cn, a, x)
  d = dim(x)
  x = gsub('|', '&#124;', x, fixed = TRUE)
  dim(x) = d
  res = do.call(function(...) paste(..., sep = '|'), as.data.frame(x))
  res = gsub('\n', newline %||% getOption('xfun.md_table.newline', ' '), res, fixed = TRUE)
  paste0('|', res, '|')
}
