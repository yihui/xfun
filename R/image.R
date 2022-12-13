# add a border to an image via ImageMagick
add_border = function(input, pixels = 1, color = 'black', output) {
  input = normalizePath(input)
  if (missing(output))
    output = paste0(sans_ext(input), '-output.', file_ext(input))
  system2('convert', shQuote(c(
    input, '-shave', paste(pixels, pixels, sep = 'x'), '-bordercolor', color,
    '-border', pixels, output)
  ))
  optipng(dirname(output))
}

#' Use the Tinify API to compress PNG and JPEG images
#'
#' Compress PNG/JPEG images with \samp{api.tinify.com}, and download the
#' compressed images. These functions require R packages \pkg{curl} and
#' \pkg{jsonlite}. \code{tinify_dir()} is a wrapper function of \code{tinify()}
#' to compress images under a directory.
#'
#' You are recommended to set the API key in \file{.Rprofile} or
#' \file{.Renviron}. After that, the only required argument of this function is
#' \code{input}. If the original images can be overwritten by the compressed
#' images, you may either use \code{output = identity}, or set the value of the
#' \code{history} argument in \file{.Rprofile} or \file{.Renviron}.
#' @param input A vector of input paths of images.
#' @param output A vector of output paths or a function that takes \code{input}
#'   and returns a vector of output paths (e.g., \code{output = \link{identity}}
#'   means \code{output = input}). By default, if the \code{history} argument is
#'   not a provided, \code{output} is \code{input} with a suffix \code{-min}
#'   (e.g., when \code{input = 'foo.png'}, \code{output = 'foo-min.png'}),
#'   otherwise \code{output} is the same as \code{input}, which means the
#'   original image files will be overwritten.
#' @param quiet Whether to suppress detailed information about the compression,
#'   which is of the form \samp{input.png (10 Kb) ==> output.png (5 Kb, 50\%);
#'   compression count: 42}. The percentage after \code{output.png} stands for
#'   the compression ratio, and the compression count shows the number of
#'   compressions used for the current month.
#' @param force Whether to compress an image again when it appears to have been
#'   compressed before. This argument only makes sense when the \code{history}
#'   argument is provided.
#' @param key The Tinify API key. It can be set via either the global option
#'   \code{xfun.tinify.key} (you may set it in \file{~/.Rprofile}) or the
#'   environment variable \code{R_XFUN_TINIFY_KEY} (you may set it in
#'   \file{~/.Renviron}).
#' @param history Path to a history file to record the MD5 checksum of
#'   compressed images. If the checksum of an expected output image exists in
#'   this file and \code{force = FALSE}, the compression will be skipped. This
#'   can help you avoid unnecessary API calls.
#' @return The output file paths.
#' @references Tinify API: \url{https://tinypng.com/developers}.
#' @seealso The \pkg{tinieR} package (\url{https://github.com/jmablog/tinieR/})
#'   is a more comprehensive implementation of the Tinify API, whereas
#'   \code{xfun::tinify()} has only implemented the feature of shrinking images.
#' @export
#' @examplesIf interactive()
#' f = xfun:::R_logo('jpg$')
#' xfun::tinify(f)  # remember to set the API key before trying this
tinify = function(
  input, output, quiet = FALSE, force = FALSE,
  key = getOption('xfun.tinify.key', Sys.getenv('R_XFUN_TINIFY_KEY')),
  history = getOption('xfun.tinify.history', Sys.getenv('R_XFUN_TINIFY_HISTORY'))
) {
  if (!(is.character(key) && length(key) == 1 && key != '')) stop(
    "The value of the 'key' argument must be a single non-empty character string."
  )
  if (length(input) == 0) return(invisible(input))
  if (any(i <- !file_exists(input))) stop(
    'Input file(s) not found: ', paste(input[i], collapse = ', ')
  )
  if (missing(output)) {
    output = if (is.character(history)) input else {
      paste0(sans_ext(input), '-min.', file_ext(input))
    }
  } else if (is.function(output)) output = output(input)

  # avoid optimizing the input image if its md5 checksum exists in history
  save_history = function(file) {
    if (!is.character(history) || history == '') return()
    dir_create(dirname(history))
    cat(paste0(tools::md5sum(file), '\n'), file = history, append = TRUE)
  }
  test_history = function(file) {
    is.character(history) && all(file_exists(c(history, file))) &&
      (tools::md5sum(file) %in% readLines(history))
  }

  auth = paste('Authorization: Basic', base64_encode(charToRaw(paste0('api:', key))))

  mapply(input, output, FUN = function(i, o) {
    if (!force && test_history(o)) {
      if (!quiet) message(
        'The image ', o, ' has been compressed before. ',
        'To compress it again, call tinify() with force = TRUE.'
      )
      return()
    }
    if (grepl('[.]png$', i, ignore.case = TRUE))
      optipng(files = i, stdout = if (quiet) FALSE else '')
    res = curl::curl_upload(i, 'https://api.tinify.com/shrink', httpheader = auth, verbose = FALSE)
    cnt = curl::parse_headers_list(res$headers)[['compression-count']]
    res = jsonlite::fromJSON(rawToChar(res$content))
    if (!is.character(u <- res$output$url)) stop2(
      "Failed to shrink '", i, "'", sprintf(': %s (%s)', res$error, res$message)
    )
    if (!quiet) message(sprintf(
      '%s (%s) ==> %s (%s, %.01f%%); compression count: %s',
      i, format_bytes(res$input$size), o, format_bytes(res$output$size),
      res$output$ratio * 100, if (length(cnt)) cnt else NA
    ))
    # back up the original image and restore it if download failed
    if (i == o) {
      b = paste0(i, '~')
      file.rename(i, b)
      on.exit(if (file_exists(o)) file.remove(b) else file.rename(b, i), add = TRUE)
    }
    curl::curl_download(u, o)
    save_history(o)
  })

  invisible(output)
}

#' @param dir A directory under which all \file{.png}, \file{.jpeg}, and
#'   \file{.webp} files are to be compressed.
#' @param ... Arguments passed to \code{\link{tinify}()}.
#' @rdname tinify
#' @export
tinify_dir = function(dir = '.', ...) {
  tinify(all_files('[.](png|jpe?g|webp)$', dir), ...)
}

#' Shrink images to a maximum width
#'
#' Use \code{\link[magick:image_resize]{magick::image_resize}()} to shrink an
#' image if its width is larger than the value specified by the argument
#' \code{width}, and optionally call \code{\link{tinify}()} to compress it.
#' @param width The desired maximum width of images.
#' @param dir The directory of images.
#' @param files A vector of image file paths. By default, this is all
#'   \file{.png}, \file{.jpeg}, and \file{.webp} images under \code{dir}.
#' @param tinify Whether to compress images using \code{\link{tinify}()}.
#' @export
#' @examples
#' f = xfun:::all_files('[.](png|jpe?g)$', R.home('doc'))
#' file.copy(f, tempdir())
#' f = file.path(tempdir(), basename(f))
#' magick::image_info(magick::image_read(f))  # some widths are larger than 300
#' xfun::shrink_images(300, files = f)
#' magick::image_info(magick::image_read(f))  # all widths <= 300 now
#' file.remove(f)
shrink_images = function(
  width = 800, dir = '.', files = all_files('[.](png|jpe?g|webp)$', dir),
  tinify = FALSE
) {
  for (f in files) {
    x = magick::image_read(f)
    if (magick::image_info(x)$width <= width) next
    x = magick::image_resize(x, sprintf('%dx', width))
    magick::image_write(x, f)
  }
  if (tinify) tinify(files, identity)
}
