#' Encode data into the base64 encoding.
#'
#' Encode a file or a raw vector into the base64 encoding.
#' @param x A raw vector. If not raw, it is assumed to be a file or a connection
#'   to be read as raw via \code{readBin()}.
#' @return A character string.
#' @useDynLib xfun, .registration = TRUE
#' @export
#' @examples xfun::base64_encode(as.raw(1:10))
#' logo = file.path(R.home('doc'), 'html', 'logo.jpg')
#' xfun::base64_encode(logo)
base64_encode = function(x) {
  if (!is.raw(x)) x = read_bin(x)
  .Call('base64_enc', x)
}

# an R implementation of base64 encoding by Wush Wu moved from knitr (of
# historic interest only): https://github.com/yihui/knitr/pull/324
base64_encode_r = function(raw.string) {
  chars = c(LETTERS, letters, 0:9, '+', '/')
  n = length(s <- as.integer(raw.string))
  res = rep(NA, (n + 2) / 3 * 4)
  i = 0L  # index of res vector
  j = 1L  # index of base64_table
  while (n > 2L) {
    res[i <- i + 1L] = chars[s[j] %/% 4L + 1L]
    res[i <- i + 1L] = chars[16 * (s[j] %% 4L) + s[j + 1L] %/% 16 + 1L]
    res[i <- i + 1L] = chars[4L * (s[j + 1L] %% 16) + s[j + 2L] %/% 64L + 1L]
    res[i <- i + 1L] = chars[s[j + 2L] %% 64L + 1L]
    j = j + 3L
    n = n - 3L
  }
  if (n) {
    res[i <- i + 1L] = chars[s[j] %/% 4L + 1L]
    if (n > 1L) {
      res[i <- i + 1L] = chars[16 * (s[j] %% 4L) + s[j + 1L] %/% 16 + 1L]
      res[i <- i + 1L] = chars[4L * (s[j + 1L] %% 16) + 1L]
      res[i <- i + 1L] = '='
    } else {
      res[i <- i + 1L] = chars[16 * (s[j] %% 4L) + 1L]
      res[i <- i + 1L] = '='
      res[i <- i + 1L] = '='
    }
  }
  paste(res[!is.na(res)], collapse = '')
}

#' Generate the Data URI for a file
#'
#' Encode the file in the base64 encoding, and add the media type. The data URI
#' can be used to embed data in HTML documents, e.g., in the \code{src}
#' attribute of the \verb{<img />} tag.
#' @param x A file path.
#' @return A string of the form \verb{data:<media type>;base64,<data>}.
#' @export
#' @examples
#' logo = file.path(R.home('doc'), 'html', 'logo.jpg')
#' img = htmltools::img(src = xfun::base64_uri(logo), alt = 'R logo')
#' if (interactive()) htmltools::browsable(img)
base64_uri = function(x) {
  paste0("data:", mime::guess_type(x), ";base64,", base64_encode(x))
}
