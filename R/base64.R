#' Encode/decode data into/from base64 encoding.
#'
#' The function \code{base64_encode()} encodes a file or a raw vector into the
#' base64 encoding. The function \code{base64_decode()} decodes data from the
#' base64 encoding.
#' @param x For \code{base64_encode()}, a raw vector. If not raw, it is assumed
#'   to be a file or a connection to be read via \code{readBin()}. For
#'   \code{base64_decode()}, a string.
#' @param from If provided (and \code{x} is not provided), a connection or file
#'   to be read via \code{readChar()}, and the result will be passed to the
#'   argument \code{x}.
#' @return \code{base64_encode()} returns a character string.
#'   \code{base64_decode()} returns a raw vector.
#' @useDynLib xfun, .registration = TRUE
#' @export
#' @examples xfun::base64_encode(as.raw(1:10))
#' logo = xfun:::R_logo()
#' xfun::base64_encode(logo)
base64_encode = function(x) {
  if (!is.raw(x)) x = read_bin(x)
  .Call('base64_enc', x, PACKAGE = 'xfun')
}

#' @export
#' @rdname base64_encode
#' @examples xfun::base64_decode("AQIDBAUGBwgJCg==")
base64_decode = function(x, from = NA) {
  if (!is.na(from)) {
    if (!missing(x)) stop("Please provide either 'x' or 'from', but not both.")
    x = readChar(from, file.size(from), TRUE)
  }
  if (!is.character(x) || length(x) != 1) stop("'x' must be a single character string.")
  .Call('base64_dec', x, PACKAGE = 'xfun')
}

# an R implementation of base64 encoding by Wush Wu moved from knitr (of
# historic interest only): https://github.com/yihui/knitr/pull/324
base64_encode_r = function(x) {
  if (!is.raw(x)) x = read_bin(x)
  chars = c(LETTERS, letters, 0:9, '+', '/')
  n = length(s <- as.integer(x))
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
#' @param type The MIME type of the file, e.g., \code{"image/png"} for a PNG
#'   image file.
#' @return A string of the form \verb{data:<media type>;base64,<data>}.
#' @note By default, this function requires the \pkg{mime} package to determine
#'   the MIME type of the file.
#' @export
#' @examples
#' logo = xfun:::R_logo()
#' img = htmltools::img(src = xfun::base64_uri(logo), alt = 'R logo')
#' if (interactive()) htmltools::browsable(img)
base64_uri = function(x, type = mime::guess_type(x)) {
  if (missing(type)) type = guess_type(x)
  paste0("data:", type, ";base64,", base64_encode(x))
}

# a limited version of mime::guess_type()
guess_type = function(x, use_mime = loadable('mime')) {
  if (use_mime) return(mime::guess_type(x))
  res = mimemap[tolower(file_ext(x))]
  if (any(i <- is.na(res))) {
    warning(
      'Cannot determine the MIME type(s) of ', paste(x[i], collapse = ', '),
      '. You may try to install the "mime" package or report an issue to ',
      packageDescription('xfun')$BugReports, '.'
    )
    res[i] = 'application/octet-stream'
  }
  unname(res)
}

# a comprehensive version is mime::mimemap (can extend it upon user request)
mimemap = c(
  css = 'text/css', csv = 'text/csv', gif = 'image/gif', jpeg = 'image/jpeg',
  jpg = 'image/jpeg', js = 'application/javascript', png = 'image/png',
  svg = 'image/svg+xml', ttf = 'application/font-sfnt',
  woff = 'application/font-woff', woff2 = 'application/octet-stream'
)
