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
