#' Manipulate filename extensions
#'
#' Functions to obtain (\code{file_ext()}), remove (\code{sans_ext()}), and
#' change (\code{with_ext()}) extensions in filenames.
#'
#' \code{file_ext()} is a wrapper of \code{tools::\link{file_ext}()}.
#' \code{sans_ext()} is a wrapper of \code{tools::\link{file_path_sans_ext}()}.
#' @param x A character of file paths.
#' @export
#' @return A character vector of the same length as \code{x}.
#' @examples library(xfun)
#' p = c('abc.doc', 'def123.tex', 'path/to/foo.Rmd')
#' file_ext(p); sans_ext(p); with_ext(p, '.txt')
#' with_ext(p, c('.ppt', '.sty', '.Rnw')); with_ext(p, 'html')
file_ext = function(x) tools::file_ext(x)

#' @rdname file_ext
#' @export
sans_ext = function(x) tools::file_path_sans_ext(x)

#' @param ext A vector of new extensions.
#' @rdname file_ext
#' @export
with_ext = function(x, ext) {
  if (anyNA(ext)) stop("NA is not allowed in 'ext'")
  n1 = length(x); n2 = length(ext); r = '([.][[:alnum:]]+)?$'
  if (n1 * n2 == 0) return(x)
  i = !grepl('^[.]', ext) & ext != ''
  ext[i] = paste0('.', ext[i])

  if (all(ext == '')) ext = ''
  if (length(ext) == 1) return(sub(r, ext, x))

  if (n1 > 1 && n1 != n2) stop("'ext' must be of the same length as 'x'")
  mapply(sub, r, ext, x, USE.NAMES = FALSE)
}

#' Normalize paths
#'
#' A wrapper function of \code{normalizePath()} with different defaults.
#' @param path,winslash,must_work Arguments passed to
#'   \code{\link{normalizePath}()}.
#' @export
#' @examples library(xfun)
#' normalize_path('~')
normalize_path = function(path, winslash = '/', must_work = FALSE) {
  res = normalizePath(path, winslash = winslash, mustWork = must_work)
  if (is_windows()) res[is.na(path)] = NA
  res
}

#' Test if two paths are the same after they are normalized
#'
#' Compare two paths after normalizing them with the same separator (\code{/}).
#' @param p1,p2 Two vectors of paths.
#' @export
#' @examples library(xfun)
#' same_path('~/foo', file.path(Sys.getenv('HOME'), 'foo'))
same_path = function(p1, p2) {
  normalize_path(p1) == normalize_path(p2)
}
