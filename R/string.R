#' Test if a character vector consists of blank strings
#'
#' Return a logical vector indicating if elements of a character vector are
#' blank (white spaces or empty strings).
#' @param x A character vector.
#' @return `TRUE` for blank elements, or `FALSE` otherwise.
#' @export
#' @examples
#' xfun::is_blank('')
#' xfun::is_blank('abc')
#' xfun::is_blank(c('', '  ', '\n\t'))
#' xfun::is_blank(c('', ' ', 'abc'))
is_blank = function(x) grepl('^\\s*$', x)

# remote blank elements from both ends
strip_blank = function(x) {
  if (!(N <- length(x))) return(x)
  r = rle(is_blank(x))
  l = r$lengths; v = r$values; n = length(v)
  i = c(if (v[1]) seq_len(l[1]), if (n > 1 && v[n]) N - seq_len(l[n]) + 1)
  if (length(i)) x[-i] else x
}

#' Convert numbers to English words
#'
#' This can be helpful when writing reports with \pkg{knitr}/\pkg{rmarkdown} if
#' we want to print numbers as English words in the output. The function `n2w()`
#' is an alias of `numbers_to_words()`.
#' @param x A numeric vector. The absolute values should be less than `1e15`.
#' @param cap Whether to capitalize the first letter of the word. This can be
#'   useful when the word is at the beginning of a sentence. Default is `FALSE`.
#' @param hyphen Whether to insert hyphen (-) when the number is between 21 and
#'   99 (except 30, 40, etc.).
#' @param and Whether to insert `and` between hundreds and tens, e.g., write 110
#'   as \dQuote{one hundred and ten} if `TRUE` instead of \dQuote{one hundred
#'   ten}.
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
#' n2w(123.456)
#' n2w(123.45678901)
#' n2w(123.456789098765)
numbers_to_words = function(x, cap = FALSE, hyphen = TRUE, and = FALSE) {
  if (!is.numeric(x)) stop('The input is not numeric.')
  if (any(abs(x) >= 1e15)) stop('The absolute value must be less than 1e15.')
  opts = options(scipen = 15, OutDec = '.')  # avoid scientific notation
  on.exit(options(opts), add = TRUE)

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
      x_marks = strsplit(format(floor(x), big.mark = ','), split = ',')[[1]]  # e.g. 123,456,789
      out = vapply(x_marks, convert_le3, character(1))  # group by 3 digits
      x_marks2 = marks[length(x_marks):1]  # units?
      x_marks2[which(out == '')] = ''  # e.g. 4,000,123, 000, remove millions
      out = paste(out, x_marks2, sep = ' ', collapse = ' ')  # zip together
    }
    out = paste0(minus, out)
    out = gsub('^ *|,? *$', '', out)  # trim heading/trailing space
    out = gsub(' {2,}', ' ', out)  # remove multiple spaces
    if (cap) out = sub('^([a-z])', '\\U\\1', out, perl = TRUE)
    if (x - floor(x) > 0) {
      frac = sub('^[0-9]+[.]', '', as.character(x))
      frac = convert_1(strsplit(frac, '')[[1]])
      out = paste(c(out, 'point', frac), collapse = ' ')
    }
    out
  }

  if (length(x) > 1) vapply(x, convert_one, character(1)) else convert_one(x)
}

#' @export
#' @rdname numbers_to_words
n2w = numbers_to_words

#' Join multiple words into a single string
#'
#' If `words` is of length 2, the first word and second word are joined by the
#' `and` string; if `and` is blank, `sep` is used. When the length is greater
#' than 2, `sep` is used to separate all words, and the `and` string is
#' prepended to the last word.
#' @param words A character vector.
#' @param sep Separator to be inserted between words.
#' @param and Character string to be prepended to the last word.
#' @param before,after A character string to be added before/after each word.
#' @param oxford_comma Whether to insert the separator between the last two
#'   elements in the list.
#' @return A character string marked by [raw_string()].
#' @export
#' @examples join_words('a'); join_words(c('a', 'b'))
#' join_words(c('a', 'b', 'c'))
#' join_words(c('a', 'b', 'c'), sep = ' / ', and = '')
#' join_words(c('a', 'b', 'c'), and = '')
#' join_words(c('a', 'b', 'c'), before = '"', after = '"')
#' join_words(c('a', 'b', 'c'), before = '"', after = '"', oxford_comma = FALSE)
join_words = function(
  words, sep = ', ', and = ' and ', before = '', after = before, oxford_comma = TRUE
) {
  n = length(words)
  if (n > 0) {
    words = paste0(before, words, after)
    if (n == 2) {
      words = paste(words, collapse = if (is_blank(and)) sep else and)
    } else if (n > 2) {
      if (oxford_comma && grepl('^ ', and) && grepl(' $', sep)) and = gsub('^ ', '', and)
      words[n] = paste0(and, words[n])
      # combine the last two words directly without the comma
      if (!oxford_comma) {
        words[n - 1] = paste0(words[n - 1:0], collapse = '')
        words = words[-n]
      }
      words = paste(words, collapse = sep)
    }
  }
  raw_string(words)
}

#' Evaluate an expression after forcing the decimal point to be a dot
#'
#' Sometimes it is necessary to use the dot character as the decimal separator.
#' In R, this could be affected by two settings: the global option
#' `options(OutDec)` and the `LC_NUMERIC` locale. This function sets the former
#' to `.` and the latter to `C` before evaluating an expression, such as
#' coercing a number to character.
#' @param x An expression.
#' @export
#' @return The value of `x`.
#' @examples
#' opts = options(OutDec = ',')
#' as.character(1.234)  # using ',' as the decimal separator
#' print(1.234)  # same
#' xfun::decimal_dot(as.character(1.234))  # using dot
#' xfun::decimal_dot(print(1.234))  # using dot
#' options(opts)
decimal_dot = function(x) {
  opts = options(OutDec = '.'); on.exit(options(opts), add = TRUE)
  lcn = Sys.getlocale('LC_NUMERIC')
  if (lcn != 'C') {
    Sys.setlocale('LC_NUMERIC', 'C')
    on.exit(suppressWarnings(Sys.setlocale('LC_NUMERIC', lcn)), add = TRUE)
  }
  x
}

# create a URL query string from named parameters
query_params = function(..., .list = list()) {
  x = if (missing(.list)) list(...) else .list
  x = paste(names(x), x, sep = '=', collapse = '&')
  if (x != '') paste0('?', x) else x
}

#' Wrap character vectors
#'
#' A wrapper function to make [strwrap()] return a character vector of the same
#' length as the input vector; each element of the output vector is a string
#' formed by concatenating wrapped strings by `\n`.
#' @param ... Arguments passed to [strwrap()].
#' @return A character vector.
#' @export
#' @examples
#' x = sample(c(letters, ' '), 200, TRUE, c(rep(.5/26, 26), .5))
#' x = rep(paste(x, collapse = ''), 2)
#' strwrap(x, width = 30)
#' xfun::str_wrap(x, width = 30)  # same length as x
str_wrap = function(...) {
  res = strwrap(..., simplify = FALSE)
  unlist(lapply(res, one_string))
}

#' Split a character vector by line breaks
#'
#' Call `unlist(strsplit(x, '\n'))` on the character vector `x` and
#' make sure it works in a few edge cases: `split_lines('')` returns
#' `''` instead of `character(0)` (which is the returned value of
#' `strsplit('', '\n')`); `split_lines('a\n')` returns `c('a',
#' '')` instead of `c('a')` (which is the returned value of
#' `strsplit('a\n', '\n')`.
#' @param x A character vector.
#' @return All elements of the character vector are split by `'\n'` into
#'   lines.
#' @export
#' @examples xfun::split_lines(c('a', 'b\nc'))
split_lines = function(x) {
  if (length(grep('\n', x)) == 0L) return(x)
  x = gsub('\n$', '\n\n', x)
  x[x == ''] = '\n'
  unlist(strsplit(x, '\r?\n'))
}

#' Split source lines into complete expressions
#'
#' Parse the lines of code one by one to find complete expressions in the code,
#' and put them in a list.
#' @param x A character vector of R source code.
#' @param merge_comments Whether to merge consecutive lines of comments as a
#'   single expression to be combined with the next non-comment expression (if
#'   any).
#' @param line_number Whether to store the line numbers of each expression in
#'   the returned value.
#' @return A list of character vectors, and each vector contains a complete R
#'   expression, with an attribute `lines` indicating the starting and ending
#'   line numbers of the expression if the argument `line_number = TRUE`.
#' @export
#' @examples
#' code = c('# comment 1', '# comment 2', 'if (TRUE) {', '1 + 1', '}', 'print(1:5)')
#' xfun::split_source(code)
#' xfun::split_source(code, merge_comments = TRUE)
split_source = function(x, merge_comments = FALSE, line_number = FALSE) {
  if ((n <- length(x)) < 1) return(list(x))
  i1 = i2 = 1L
  res = list()
  add_source = function(x) {
    res[[length(res) + 1]] <<- if (line_number) structure(x, lines = c(i1, i2)) else x
  }
  while (i2 <= n) {
    piece = x[i1:i2]
    if ((!merge_comments || (!all(grepl('^#', piece)) || i2 == n)) && valid_syntax(piece)) {
      add_source(piece)
      i1 = i2 + 1L # start from the next line
    }
    i2 = i2 + 1L
  }
  if (i1 <= n) parse(text = piece)  # must be an error there
  res
}

#' Check if the syntax of the code is valid
#'
#' Try to [parse()] the code and see if an error occurs.
#' @param code A character vector of R source code.
#' @param silent Whether to suppress the error message when the code is not
#'   valid.
#' @return `TRUE` if the code could be parsed, otherwise `FALSE`.
#' @export
#' @examples xfun::valid_syntax('1+1')
#' xfun::valid_syntax('1+')
#' xfun::valid_syntax(c('if(T){1+1}', 'else {2+2}'), silent = FALSE)
valid_syntax = function(code, silent = TRUE) {
  !inherits(suppressWarnings(try(parse_only(code), silent = silent)), 'try-error')
}

#' Bump version numbers
#'
#' Increase the last digit of version numbers, e.g., from `0.1` to
#' `0.2`, or `7.23.9` to `7.23.10`.
#' @param x A vector of version numbers (of the class `"numeric_version"`),
#'   or values that can be coerced to version numbers via
#'   `as.numeric_version()`.
#' @return A vector of new version numbers.
#' @export
#' @examples xfun::bump_version(c('0.1', '91.2.14'))
bump_version = function(x) {
  x = as.numeric_version(x)
  for (i in seq_along(x)) {
    v = x[i]
    n = length(unclass(v)[[1]])
    v[[1, n]] = v[[1, n]] + 1  # bump the last digit
    x[i] = v
  }
  x
}

#' Fix pairs of characters in a file
#'
#' For example, the curly braces may be wrong (the opening and closing braces
#' are swapped for some reason).
#' @param x A character vector (by default, read from `file`).
#' @param file Path to a text file.
#' @param chars A vector of characters of length 2. By default, it is a pair of
#'   curly double quotes.
#' @references <https://d.cosx.org/d/420794/5>
#' @noRd
#' @examples
#' files = list.files('.', '[.]R?md$', recursive = TRUE, full.names = TRUE)
#' for (f in files) {
#'   pair_chars(file = f)
#'   # curly single quotes
#'   pair_chars(file = f, chars = c('\U2018', '\U2019'))
#' }
pair_chars = function(x = read_utf8(file), file, chars = c('\U201c', '\U201d')) {
  if (length(chars) != 2) stop("'chars' must be of length 2 (i.e., a pair of characters)")
  is_file = !missing(file)
  r = paste(c('[', chars, ']'), collapse = '')
  k = gregexpr(r, x)
  m = regmatches(x, k)
  for (i in seq_along(m)) {
    n = length(m[[i]])
    if (n %% 2 != 0) {
      warning(
        'The characters do not appear in pairs in the text (',
        'line: ', i, if (is_file) c('; file: ', file), '):\n', x[i], '\n'
      )
      next
    }
    m[[i]] = rep(chars, length.out = n)
  }
  x2 = x
  regmatches(x, k) = m
  if (is_file) {
    if (!identical(x, x2)) write_utf8(x, file)
    invisible(x)
  } else x
}

#' Generate ID strings
#'
#' Substitute certain (by default, non-alphanumeric) characters with dashes and
#' remove extra dashes at both ends to generate ID strings. This function is
#' intended for generating IDs for HTML elements, so HTML tags in the input text
#' will be removed first.
#' @param x A character vector.
#' @param exclude A (Perl) regular expression to detect characters to be
#'   replaced by dashes. By default, non-alphanumeric characters are replaced.
#' @return A character vector of IDs.
#' @export
#' @examples
#' x = c('Hello world 123!', 'a  &b*^##c 456')
#' xfun::alnum_id(x)
#' xfun::alnum_id(x, '[^[:alpha:]]+')  # only keep alphabetical chars
#' # when text contains HTML tags
#' xfun::alnum_id('<h1>Hello <strong>world</strong>!')
alnum_id = function(x, exclude = '[^[:alnum:]]+') {
  x = strip_html(x)
  tolower(gsub('^-+|-+$', '', gsub(exclude, '-', x, perl = TRUE)))
}

#' Strip HTML tags
#'
#' Remove HTML tags and comments from text.
#' @param x A character vector.
#' @return A character vector with HTML tags and comments stripped off.
#' @export
#' @examples
#' xfun::strip_html('<a href="#">Hello <!-- comment -->world!</a>')
strip_html = function(x) {
  x = gsub('<!--.*?-->', '', x)
  x = gsub('<[^>]+>', '', x)
  x
}

# no need to close these tags
.void_tags = c(
  'area', 'base', 'br', 'col', 'command', 'embed', 'hr', 'img', 'input',
  'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr'
)

.html_class2 = c(.html_class <- c('xfun_html', 'html'), 'xfun_raw_string')

#' Tools for HTML tags
#'
#' Given a tag name, generate an HTML tag with optional attributes and content.
#' [html_tag()] can be viewed as a simplified version of `htmltools::tags`,
#' [html_value()] adds classes on the value so that it will be treated as raw
#' HTML (not escaped by `html_tag()`), [html_escape()] escapes special
#' characters in HTML, and `html_view()` launches a browser or viewer to view
#' the HTML content.
#' @param .name The tag name.
#' @param .content The content between opening and closing tags. Ignored for
#'   void tags such as `<img>`. Special characters such as `&`, `<`, and `>`
#'   will be escaped unless the value was generated from [html_value()]. The
#'   content can be either a character vector or a list. If it is a list, it may
#'   contain both normal text and HTML content.
#' @param .attrs A named list of attributes.
#' @param ... For `html_tag()`, named arguments as an alternative way to provide
#'   attributes. For `html_view()`, other arguments to be passed to [new_app()].
#' @return A character string.
#' @export
#' @examples
#' xfun::html_tag('a', '<R Project>', href = 'https://www.r-project.org', target = '_blank')
#' xfun::html_tag('br')
#' xfun::html_tag('a', xfun::html_tag('strong', 'R Project'), href = '#')
#' xfun::html_tag('a', list('<text>', xfun::html_tag('b', 'R Project')), href = '#')
html_tag = function(.name, .content = NULL, .attrs = NULL, ...) {
  if (is.null(.attrs)) .attrs = list(...)
  if (length(.attrs) && (is.null(nm <- names(.attrs)) || any(nm == '')))
    stop('Tag attributes must be named values')
  x1 = c('<', .name)
  x2 = if (length(.attrs)) .mapply(function(a, v) {
    if (is.null(v)) a else sprintf('%s="%s"', a, html_escape(v, TRUE))
  }, list(nm, .attrs), NULL)
  x2 = paste(unlist(x2), collapse = ' ')
  x3 = if (.name %in% .void_tags) ' />' else {
    c('>', one_string(html_content(.content)), '</', .name, '>')
  }
  x = c(x1, if (x2 != '') c(' ', x2), x3)
  x = paste(x, collapse = '')
  html_value(x)
}

# recursively resolve HTML content if it's a list containing both HTML and regular text
html_content = function(x) {
  if (is.list(x)) unlist(lapply(x, html_content)) else {
    if (length(x) && !inherits(x, .html_class)) x = html_escape(x)
    x
  }
}

#' @param x A character vector to be treated as raw HTML content for
#'   `html_value()`, escaped for `html_escape()`, and viewed for `html_view()`.
#' @rdname html_tag
#' @export
html_value = function(x) structure(x, class = .html_class2, lang = '.html')

#' @param attr Whether to escape `"`, `\r`, and `\n` (which should be escaped
#'   for tag attributes).
#' @rdname html_tag
#' @export
#' @examples
#' xfun::html_escape('" quotes " & brackets < >')
#' xfun::html_escape('" & < > \r \n', attr = TRUE)
html_escape = function(x, attr = FALSE) {
  x = gsubf('&', '&amp;', x)
  x = gsubf('<', '&lt;', x)
  x = gsubf('>', '&gt;', x)
  # for attributes, we still need to escape more characters
  if (attr) {
    x = gsubf('"', '&quot;', x)
    x = gsubf('\r', '&#13;', x)
    x = gsubf('\n', '&#10;', x)
  }
  x
}

#' @rdname html_tag
#' @export
html_view = function(x, ...) {
  new_app('xfun-html', function(path, ...) {
    if (dir_exists(path)) list(payload = if (path == '.') x else path) else {
      list(file = normalizePath(path), `content-type` = mime_type(path))
    }
  }, ...)
}

one_string = function(x, ...) paste(x, ..., collapse = '\n')

# en/decrypt a string via a character map (old and new must be 16 unique hex chars)
.crypt = function(x, old, new) {
  x2 = chartr(old, new, as.character(charToRaw(x)))
  rawToChar(as.raw(strtoi(x2, 16L)))
}
encrypt = function(x, key) .crypt(x, '0123456789abcdef', key)
decrypt = function(x, key) .crypt(x, key, '0123456789abcdef')
