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
#' Convert numbers to English words. This can be helpful when writing
#' articles with rmarkdown if we want to print numbers as English words in the
#' final output.
#'
#' @param x A numeric vector.
#' @param cap Whether to capitalize the first letter of the word; this can be useful when the word is
#' at the beginning of a sentence. Default is \code{FALSE}.
#' @param hyphen Whether to insert hyphen (-) when the number is between 21 and 99 (except 30, 40, etc.).
#' @return A character vector.
#' @export
#' @examples library(xfun)
#' number_to_word(0, cap = TRUE)
#' number_to_word(0:30, cap = TRUE)
number_to_word = function(x, cap = FALSE, hyphen = TRUE){
  if(length(x) > 1) return(sapply(x, number_to_word, cap = cap, hyphen = hyphen))

  if(!is.numeric(x)) stop("the input is not a number")
  if(x > 1e12) {
    warning("the number is too large, skip the convertion")
    return(x)
  }

  opts <- options(scipen = 12)
  on.exit(options(opts))

  x_char = as.character(x)
  if(!is.integer(x)) {
    if(grepl("[.]", x_char)){
      warning("the number is not an integer, skip the convertion")
      return(x)
    }
  }

  zero_to_19 = c("zero", "one", "two", "three", "four", "five", "six", "seven",
                 "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                 "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  names(zero_to_19) = as.character(0:19)
  tens = c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  names(tens) = as.character(seq(20, 90, 10))

  n_digists = nchar(x_char)
  x_chars = strsplit(x_char, split = "")[[1]]

  convert_1_digit = function(x_c) unname(zero_to_19[x_c]) # account for zero

  convert_2_digits = function(x_c){
    x_cs = strsplit(x_c, split = "")[[1]]
    if(x_cs[1] == 1){# 10 - 19
      out = zero_to_19[x_c]
    } else { # >= 20
      if(x_cs[2] == 0) { # 20, 30, 40, ...
        out = unname(tens[x_c])
      } else { # 21, 22, etc.
        out = paste(tens[as.integer(x_cs[1]) - 1],
              convert_1_digit(x_cs[2]),
              sep = " ")
      }
    }
    unname(out)
  }

  convert_3_digits = function(x_c){
    x_cs = strsplit(x_c, split = "")[[1]]
    n_hundreds = paste(convert_1_digit(x_cs[1]), "hundred", sep = " ")
    if(x_cs[2] == "0") {
      if(x_cs[3] == "0") {
        return(n_hundreds)
      } else {
        out = convert_1_digit(x_cs[3])
      }
    } else {
      out = convert_2_digits(paste(x_cs[2:3], collapse = ""))
    }
    paste(n_hundreds, out, sep = " ")
  }

  convert_le3_digits = function(x_c){
    x_c = gsub("^0*", "", x_c)
    n = nchar(x_c)
    if(n == 0) return("")
    if(n == 1) return(convert_1_digit(x_c))
    if(n == 2) return(convert_2_digits(x_c))
    if(n == 3) return(convert_3_digits(x_c))
  }

  if(n_digists >= 10){ # billions
    part1 = paste(convert_le3_digits(paste(x_chars[1: (n_d <- n_digists - 9)],
                                           collapse = "")),
          "billion", sep = " ")
    p2 = convert_le3_digits(paste(x_chars[(1:3) + n_d], collapse = ""))
    part2 = paste(p2, if(p2 != "") "million" else "", sep = " ")
    p3 = convert_le3_digits(paste(x_chars[(4:6) + n_d], collapse = ""))
    part3 = paste(p3, if(p3 != "") "thousand" else "", sep = " ")
    part4 = convert_le3_digits(paste(x_chars[(7:9) + n_d], collapse = ""))
    out = paste(part1, part2, part3, part4, collapse = "")
  } else {
    if(n_digists >= 7){ # millions
      part1 = paste(convert_le3_digits(paste(x_chars[1: (n_d <- n_digists - 6)],
                                             collapse = "")),
                    "million", sep = " ")
      p2 = convert_le3_digits(paste(x_chars[(1:3) + n_d], collapse = ""))
      part2 = paste(p2, if(p2 != "") "thousand" else "", sep = " ")
      part3 = convert_le3_digits(paste(x_chars[(4:6) + n_d], collapse = ""))
      out = paste(part1, part2, part3, collapse = "")
    } else {
      if(n_digists >= 4){ # thounsands
        part1 = paste(convert_le3_digits(paste(x_chars[1: (n_d <- n_digists - 3)],
                                               collapse = "")),
                      "thousand", sep = " ")
        part2 = convert_le3_digits(paste(x_chars[(1:3) + n_d], collapse = ""))
        out = paste(part1, part2, collapse = "")
      } else { # 0 - 999
        out = if(x == 0) "zero" else convert_le3_digits(paste(x_chars,
                                                              collapse = ""))
      }
    }
  }

  out = gsub("^ *| *$", "", out)
  out = gsub(" {2,}", " ", out)
  if(n_digists < 3 & hyphen) out = gsub(" ", "-", out)
  if(cap) substr(out, 1, 1) = toupper(substr(out, 1, 1))
  out
}


