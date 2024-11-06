#' Encode/decode data into/from base64 encoding.
#'
#' The function `base64_encode()` encodes a file or a raw vector into the
#' base64 encoding. The function `base64_decode()` decodes data from the
#' base64 encoding.
#' @param x For `base64_encode()`, a raw vector. If not raw, it is assumed
#'   to be a file or a connection to be read via `readBin()`. For
#'   `base64_decode()`, a string.
#' @param from If provided (and `x` is not provided), a connection or file
#'   to be read via `readChar()`, and the result will be passed to the
#'   argument `x`.
#' @return `base64_encode()` returns a character string.
#'   `base64_decode()` returns a raw vector.
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
#' can be used to embed data in HTML documents, e.g., in the `src` attribute of
#' the `<img />` tag.
#' @param x A file path.
#' @param type The MIME type of the file, e.g., `"image/png"` for a PNG image
#'   file.
#' @return A string of the form `data:<media type>;base64,<data>`.
#' @export
#' @examples
#' logo = xfun:::R_logo()
#' img = xfun::html_tag('img', src = xfun::base64_uri(logo), alt = "R logo")
#' if (interactive()) xfun::html_view(img)
base64_uri = function(x, type = mime_type(x)) {
  paste0("data:", type, ";base64,", base64_encode(x))
}

#' Get the MIME types of files
#'
#' If the \pkg{mime} package is installed, call [mime::guess_type()], otherwise
#' use the system command `file --mime-type` to obtain the MIME type of a file.
#' Typically, the `file` command exists on *nix. On Windows, the command should
#' exist if Cygwin or Rtools is installed. If it is not found, .NET's
#' `MimeMapping` class will be used instead (which requires the .NET framework).
#' @param x A vector of file paths.
#' @param use_mime Whether to use the \pkg{mime} package.
#' @param empty The MIME type for files without extensions (e.g., `Makefile`).
#'   If `NA`, the type will be obtained from system command. This argument is
#'   used only for `use_mime = FALSE`.
#' @return A character vector of MIME types.
#' @note When querying the MIME type via the system command, the result will be
#'   cached to `xfun:::cache_dir()`. This will make future queries much faster,
#'   since running the command in real time can be a little slow.
#' @export
#' @examplesIf tolower(Sys.getenv('CI')) == 'true'
#' f = list.files(R.home('doc'), full.names = TRUE)
#' mime_type(f)
#' mime_type(f, FALSE)  # don't use mime
#' mime_type(f, FALSE, NA)  # run command for files without extension
mime_type = function(x, use_mime = loadable('mime'), empty = 'text/plain') {
  if (use_mime) return(mime::guess_type(x))
  ext = tolower(file_ext(x))
  res = character(n <- length(x))
  # try to read from cache since running command in real time can be slow
  p = file.path(cache_dir(), 'mime.rds')
  db = mimemap
  for (i in seq_len(n)) {
    e = ext[i]
    m = if (e == '') empty else .mime_type(x[i], e)
    if (is.na(m) && is.na(m <- db[e])) {
      if (file_exists(p)) db = readRDS(p)
      m = db[e]
    }
    if (is.na(m)) {
      m = .mime_cmd(x[i])
      if (grepl('^[a-zA-Z]+/[-a-zA-Z0-9.+]+$', m)) {
        if (e != '') {
          db[e] = m; if (dir_create(dirname(p))) saveRDS(db, p)
        }
      } else warning("'", m, "' does not appear to be a valid MIME type for '", x[i], "'")
    }
    res[i] = m
  }
  res
}

# try to get the MIME type from tools:::mime_type()
.mime_type = function(x, ext = file_ext(x)) {
  if (ext != '' && is.function(f <- asNamespace('tools')$mime_type)) {
    m = f(x, ext)
    if (m != 'text/plain') return(m)
  }
  NA
}

# MIME type from command line `powershell` or `file`
.mime_cmd = function(x, use_file = !is_windows() || Sys.which('file') != '') {
  if (!file.exists(x <- path.expand(x))) stop("The file '", x, "' does not exist.")
  if (use_file) {
    cmd = 'file'
    arg = c('--mime-type', '-b')
  } else {
    # IT said "thou shall not include .ps1 in source package", so I use .txt;
    # then Windoz said "thou shall not execute .txt", so I copy .txt to .ps1;
    # How much of our life has been wasted on making these idiots happy...
    ps1 = tempfile(fileext = '.ps1'); on.exit(unlink(ps1), add = TRUE)
    file.copy(pkg_file('scripts', 'mime-type.txt'), ps1)
    cmd = 'powershell'
    arg = c('-ExecutionPolicy', 'Bypass', '-File', shQuote(ps1))
  }
  if (Sys.which(cmd) == '') stop("The '", cmd, "' command is not found")
  system2(cmd, c(arg, shQuote(x)), stdout = TRUE)[1]
}

# a comprehensive version is mime::mimemap
mimemap = c(
  css = 'text/css', csv = 'text/csv', gif = 'image/gif', jpeg = 'image/jpeg',
  jpg = 'image/jpeg', js = 'text/javascript', png = 'image/png',
  r = 'text/plain', rmd = 'text/x-markdown', svg = 'image/svg+xml',
  ttf = 'font/ttf', woff = 'font/woff', woff2 = 'font/woff2'
)
