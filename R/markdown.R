#' Find the indices of lines in Markdown that are prose (not code blocks)
#'
#' Filter out the indices of lines between code block fences such as \verb{```}
#' (could be three or four or more backticks).
#' @param x A character vector of text in Markdown.
#' @note If the code fences are not balanced (e.g., a starting fence without an
#'   ending fence), this function will treat all lines as prose.
#' @return An integer vector of indices of lines that are prose in Markdown.
#' @export
#' @examples library(xfun)
#' prose_index(c('a', '```', 'b', '```', 'c'))
#' prose_index(c('a', '````', '```r', '1+1', '```', '````', 'c'))
prose_index = function(x) {
  idx = NULL; r = '^(\\s*```*).*'; s = ''
  for (i in grep(r, x)) {
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
    # treat all lines as prose
    warning('Code fences are not balanced'); return(xi)
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
#' @return A character vector with math expressions in backticks.
#' @note If you are using Pandoc or the \pkg{rmarkdown} package, there is no
#'   need to use this function, because Pandoc's Markdown can recognize math
#'   expressions.
#' @export
#' @examples library(xfun)
#' protect_math(c('hi $a+b$', 'hello $$\\alpha$$', 'no math here: $x is $10 dollars'))
#' protect_math(c('hi $$', '\\begin{equation}', 'x + y = z', '\\end{equation}'))
protect_math = function(x) {
  i = prose_index(x)
  if (length(i)) x[i] = escape_math(x[i])
  x
}

escape_math = function(x) {
  # replace $x$ with `\(x\)` (protect inline math in <code></code>)
  m = gregexpr('(?<=^|[\\s])[$](?! )[^$]+?(?<! )[$](?![$0123456789])', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    z = sub('^[$]', '`\\\\(', z)
    z = sub('[$]$', '\\\\)`', z)
    z
  })
  # replace $$x$$ with `$$x$$` (protect display math)
  m = gregexpr('(?<=^|[\\s])[$][$](?! )[^$]+?(?<! )[$][$]', x, perl = TRUE)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z) == 0) return(z)
    paste0('`', z, '`')
  })
  # if a line start or end with $$, treat it as math under some conditions
  i = !grepl('^[$].+[$]$', x)
  if (any(i)) {
    x[i] = gsub('^([$][$])([^ ]+)', '`\\1\\2', x[i], perl = TRUE)
    x[i] = gsub('([^ ])([$][$])$', '\\1\\2`', x[i], perl = TRUE)
  }
  # equation environments
  i = grep('^\\\\begin\\{[^}]+\\}$', x)
  x[i] = paste0('`', x[i])
  i = grep('^\\\\end\\{[^}]+\\}$', x)
  x[i] = paste0(x[i], '`')
  x
}
