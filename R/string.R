#' Convert numbers to English words
#'
#' This can be helpful when writing reports with \pkg{knitr}/\pkg{rmarkdown} if
#' we want to print numbers as English words in the output. The function
#' \code{n2w()} is an alias of \code{numbers_to_words()}.
#' @param x A numeric vector. Values should be integers. The absolute values
#'   should be less than \code{1e15}.
#' @param cap Whether to capitalize the first letter of the word. This can be
#'   useful when the word is at the beginning of a sentence. Default is
#'   \code{FALSE}.
#' @param hyphen Whether to insert hyphen (-) when the number is between 21 and
#'   99 (except 30, 40, etc.).
#' @param and Whether to insert \code{and} between hundreds and tens, e.g.,
#'   write 110 as \dQuote{one hundred and ten} if \code{TRUE} instead of
#'   \dQuote{one hundred ten}.
#' @return A character vector.
#' @author Daijiang Li
#' @export
#' @examples library(xfun)
#' n2w(0, cap = TRUE)
#' n2w(0:121, and = TRUE)
#' n2w(1e6)
#' n2w(1e11+12345678)
#' n2w(-987654321)
#' n2w(1e15-1)
numbers_to_words = function(x, cap = FALSE, hyphen = TRUE, and = FALSE) {

  if (!is.numeric(x)) stop('The input is not numeric.')
  if (any(abs(x) >= 1e15)) stop('The absolute value must be less than 1e15.')
  opts = options(scipen = 15); on.exit(options(opts), add = TRUE)  # avoid scientific notation
  if (any(x != floor(x))) stop('The numbers must be integer. ')

  zero_to_19 = c(
    'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',
    'eleven', 'twelve', paste0(c('thir', 'four', 'fif', 'six', 'seven', 'eigh', 'nine'), 'teen')
  )
  names(zero_to_19) = as.character(0:19)
  tens = c('twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety')
  names(tens) = as.character(seq(20, 90, 10))
  marks = c('', 'thousand,', 'million,', 'billion,', 'trillion,')

  convert_1 = function(x_c) zero_to_19[x_c]  # 0 - 9

  # 10 - 99
  convert_2 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    if (x_cs[1] == 1) return(zero_to_19[x_c])  # 10 - 19
    if (x_cs[2] == 0) return(tens[x_c])  # 20, 30, 40, ...
    # 21, 22, etc.
    paste(tens[as.integer(x_cs[1]) - 1], convert_1(x_cs[2]), sep = if (hyphen) '-' else ' ')
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
    if (cap) out = sub('^([a-z])', '\\U\\1', out, perl = TRUE)
    out
  }

  if (length(x) > 1) vapply(x, convert_one, character(1)) else convert_one(x)
}

#' @export
#' @rdname numbers_to_words
n2w = numbers_to_words
