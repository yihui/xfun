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
#' @param ... Arguments to be passed to \code{\link{normalize_path}()}.
#' @export
#' @examples library(xfun)
#' same_path('~/foo', file.path(Sys.getenv('HOME'), 'foo'))
same_path = function(p1, p2, ...) {
  normalize_path(p1, ...) == normalize_path(p2, ...)
}

#' Rename files with a sequential numeric prefix
#'
#' Rename a series of files and add an incremental numeric prefix to the
#' filenames. For example, files \file{a.txt}, \file{b.txt}, and \file{c.txt}
#' can be renamed to \file{1-a.txt}, \file{2-b.txt}, and \file{3-c.txt}.
#' @param pattern A regular expression for \code{\link{list.files}()} to obtain
#'   the files to be renamed. For example, to rename \code{.jpeg} files, use
#'   \code{pattern = "[.]jpeg$"}.
#' @param format The format for the numeric prefix. This is passed to
#'   \code{\link{sprintf}()}. The default format is \code{"\%0Nd"} where \code{N
#'   = floor(log10(n)) + 1} and \code{n} is the number of files, which means the
#'   prefix may be padded with zeros. For example, if there are 150 files to be
#'   renamed, the format will be \code{"\%03d"} and the prefixes will be
#'   \code{001}, \code{002}, ..., \code{150}.
#' @param replace Whether to remove existing numeric prefixes in filenames.
#' @param start The starting number for the prefix (it can start from 0).
#' @param dry_run Whether to not really rename files. To be safe, the default is
#'   \code{TRUE}. If you have looked at the new filenames and are sure the new
#'   names are what you want, you may rerun \code{rename_seq()} with
#'   \code{dry_run = FALSE)} to actually rename files.
#' @return A named character vector. The names are original filenames, and the
#'   vector itself is the new filenames.
#' @export
#' @examples xfun::rename_seq()
#' xfun::rename_seq('[.](jpeg|png)$', format = '%04d')
rename_seq = function(
  pattern = '^[0-9]+-.+[.]Rmd$', format = 'auto', replace = TRUE, start = 1,
  dry_run = TRUE
) {
  n = length(files <- list.files('.', pattern))
  if (n == 0) return(files)

  files2 = if (replace) sub('^[0-9]+-*', '', files) else files
  if (format == 'auto') format = paste0('%0', floor(log10(n)) + 1, 'd')
  files2 = paste(sprintf(format, seq_len(n) + start - 1), files2, sep = '-')

  if (!dry_run) file.rename(files, files2)
  setNames(files2, files)
}
