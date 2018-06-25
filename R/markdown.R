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

#' Convert numbers to English words
#'
#' This can be helpful when writing reports with \pkg{knitr}/\pkg{rmarkdown} if
#' we want to print numbers as English words in the output. The function
#' \code{n2w()} is an alias of \code{numbers_to_words()}.
#' @param x A numeric vector. Values should be integers. The absolute values
#'   should not be greater than \code{1e13}.
#' @param cap Whether to capitalize the first letter of the word. This can be
#'   useful when the word is at the beginning of a sentence. Default is
#'   \code{FALSE}.
#' @param hyphen Whether to insert hyphen (-) when the number is between 21 and
#'   99 (except 30, 40, etc.).
#' @param and Whether to insert \code{and} between hundreds and tens, e.g.,
#'   write 110 as \dQuote{one hundred and ten} if \code{TRUE} instead of
#'   \dQuote{one hundred ten}.
#' @return A character vector.
#' @export
#' @examples library(xfun)
#' n2w(0, cap = TRUE)
#' n2w(0:121, and = TRUE)
#' n2w(1e6)
#' n2w(1e11+12345678)
numbers_to_words = function(x, cap = FALSE, hyphen = TRUE, and = FALSE) {

  if (!is.numeric(x)) stop('The input is not numeric.')
  if (any(abs(x) > 1e13)) stop('The absolute value must not be greater than 1e13.')
  opts = options(scipen = 15); on.exit(options(opts), add = TRUE)  # avoid scientific notation
  if (length(grep('[^0-9]', as.character(x)))) stop('The numbers must be integer.')

  zero_to_19 = c(
    'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',
    'eleven', 'twelve', paste0(c('thir', 'four', 'fif', 'six', 'seven', 'eigh', 'nine'), 'teen')
  )
  names(zero_to_19) = as.character(0:19)
  tens = c('twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')
  names(tens) = as.character(seq(20, 90, 10))
  marks = c('', 'thousand,', 'million,', 'billion,')

  convert_1 = function(x_c) zero_to_19[x_c]  # 0 - 9

  # 10 - 99
  convert_2 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    if (x_cs[1] == 1) {
      zero_to_19[x_c]  # 10 - 19
    } else {
      if (x_cs[2] == 0) {
        tens[x_c]  # 20, 30, 40, ...
      } else {
        # 21, 22, etc.
        paste(tens[as.integer(x_cs[1]) - 1], convert_1(x_cs[2]), sep = if (hyphen) '-' else ' ')
      }
    }
  }

  # 100 - 999
  convert_3 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    n_hundreds = paste(convert_1(x_cs[1]), 'hundred', sep = ' ')
    out = if (x_cs[2] == '0') {
      if (x_cs[3] == '0') return(n_hundreds)  # x00
      convert_1(x_cs[3])  # x0x
    } else {
      convert_2(paste(x_cs[2:3], collapse = ''))  # xxx
    }
    paste(n_hundreds, out, sep = if (and) ' and ' else ' ')
  }

  convert_le3 = function(x_c) {
    x_c = gsub('^0+', '', x_c) # avoid something like 000, 001, 010; but also remove 0
    n = nchar(x_c)
    if (n == 0) return('')
    if (n == 1) return(convert_1(x_c))
    if (n == 2) return(convert_2(x_c))
    if (n == 3) return(convert_3(x_c))
  }

  convert_one = function(x) {
    minus = if (x >= 0) '' else {
      x = abs(x); 'minus '
    }
    if (x == 0) {
      out = 'zero'  # because convert_le3 removed all 0s
    } else {
      x_marks = strsplit(format(x, big.mark = ','), split = ',')[[1]]  # e.g. 123,456,789
      out = vapply(x_marks, convert_le3, character(1))  # group by 3 digits
      x_marks2 = marks[length(x_marks):1]  # units?
      x_marks2[which(out == '')] = ''  # e.g. 4,000,123, 000, remove millions
      out = paste(out, x_marks2, sep = ' ', collapse = ' ')  # zip together
    }
    out = paste0(minus, out)
    out = gsub('^ *|,? *$', '', out)  # trim heading/trailing space
    out = gsub(' {2,}', ' ', out)  # remove multiple spaces
    # if (x < 100 & hyphen) out = gsub(' ', '-', out)
    if (cap) out = sub('^([a-z])', '\\U\\1', out, perl = TRUE)
    out
  }

  if (length(x) > 1) vapply(x, convert_one, character(1)) else convert_one(x)
}

#' @export
#' @rdname numbers_to_words
n2w = numbers_to_words
