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
  idx = NULL; r = '^(\\s*```+).*'; s = ''
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
  # now, if there are still lines starting and ending with $$, they might be
  # math expressions of display style spanning multiple lines, e.g.,
  # $$\alpha +
  # \beta$$
  # we assume that $$ can only appear once on one line
  i = vapply(gregexpr('[$]', x), length, integer(1)) == 2
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

#' Embed a file, multiple files, or directory on an HTML page
#'
#' For a file, first encode it into base64 data (a character string). Then
#' generate a hyperlink of the form \samp{<a href="base64 data"
#' download="filename">Download filename</a>}. The file can be downloaded when
#' the link is clicked in modern web browsers. For a directory, it will be
#' compressed as a zip archive first, and the zip file is passed to
#' \code{embed_file()}. For multiple files, they are also compressed to a zip
#' file first.
#'
#' These functions can be called in R code chunks in R Markdown documents with
#' HTML output formats. You may embed an arbitrary file or directory in the HTML
#' output file, so that readers of the HTML page can download it from the
#' browser. A common use case is to embed data files for readers to download.
#' @param path Path to the file(s) or directory.
#' @param name The default filename to use when downloading the file. Note that
#'   for \code{embed_dir()}, only the base name (of the zip filename) will be
#'   used.
#' @param text The text for the hyperlink.
#' @param ... For \code{embed_file()}, additional arguments to be passed to
#'   \code{htmltools::a()} (e.g., \code{class = 'foo'}). For \code{embed_dir()}
#'   and \code{embed_files()}, arguments passed to \code{embed_file()}.
#' @note Windows users may need to install Rtools to obtain the \command{zip}
#'   command to use \code{embed_dir()} and \code{embed_files()}.
#'
#'   These functions require R packages \pkg{mime}, \pkg{base64enc}, and
#'   \pkg{htmltools}. If you have installed the \pkg{rmarkdown} package, these
#'   packages should be available, otherwise you need to install them
#'   separately.
#'
#'   Currently Internet Explorer does not support downloading embedded files
#'   (\url{https://caniuse.com/#feat=download}).
#' @return An HTML tag \samp{<a>} with the appropriate attributes.
#' @export
#' @examples
#' logo = file.path(R.home("doc"), "html", "logo.jpg")
#' link = xfun::embed_file(logo, 'R-logo.jpg', 'Download R logo')
#' link
#' if (interactive()) htmltools::browsable(link)
embed_file = function(path, name = basename(path), text = paste('Download', name), ...) {
  h = paste0("data:", mime::guess_type(path), ";base64,", base64enc::base64encode(path))
  htmltools::a(text, href = h, download = name, ...)
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
