#' Manipulate filename extensions
#'
#' Functions to obtain (\code{file_ext()}), remove (\code{sans_ext()}), and
#' change (\code{with_ext()}) extensions in filenames.
#'
#' \code{file_ext()} is similar to \code{tools::\link{file_ext}()}, and
#' \code{sans_ext()} is similar to \code{tools::\link{file_path_sans_ext}()}.
#' The main differences are that they treat \code{tar.(gz|bz2|xz)} and
#' \code{nb.html} as extensions (but functions in the \pkg{tools} package
#' doesn't allow double extensions by default), and allow characters \code{~}
#' and \code{#} to be present at the end of a filename.
#' @param x A character of file paths.
#' @param extra Extra characters to be allowed in the extensions. By default,
#'   only alphanumeric characters are allowed (and also some special cases in
#'   \sQuote{Details}). If other characters should be allowed, they can be
#'   specified in a character string, e.g., \code{"-+!_#"}.
#' @export
#' @return A character vector of the same length as \code{x}.
#' @examples library(xfun)
#' p = c('abc.doc', 'def123.tex', 'path/to/foo.Rmd', 'backup.ppt~', 'pkg.tar.xz')
#' file_ext(p); sans_ext(p); with_ext(p, '.txt')
#' with_ext(p, c('.ppt', '.sty', '.Rnw', 'doc', 'zip')); with_ext(p, 'html')
#'
#' # allow for more characters in extensions
#' p = c('a.c++', 'b.c--', 'c.e##')
#' file_ext(p)  # -/+/# not recognized by default
#' file_ext(p, extra = '-+#')
file_ext = function(x, extra = '') {
  ext = character(length(x))
  i = grep(r <- reg_path(extra), x)
  ext[i] = sub(r, '\\3', x[i])
  ext
}

#' @rdname file_ext
#' @export
sans_ext = function(x, extra = '') {
  sub(reg_path(extra), '\\1', x)
}

#' @param ext A vector of new extensions. It must be either of length 1, or the
#'   same length as \code{x}.
#' @rdname file_ext
#' @export
with_ext = function(x, ext, extra = '') {
  if (anyNA(ext)) stop("NA is not allowed in 'ext'")
  n1 = length(x); n2 = length(ext)
  if (n1 * n2 == 0) return(x)
  i = !grepl('^[.]', ext) & ext != ''
  ext[i] = paste0('.', ext[i])

  if (all(ext == '')) ext = ''
  r = sub('[$]$', '?$', reg_ext(extra))  # make extensions in 'x' optional
  if (length(ext) == 1) return(sub(r, ext, x))

  if (n1 > 1 && n1 != n2) stop("'ext' must be of the same length as 'x'")
  mapply(sub, r, ext, x, USE.NAMES = FALSE)
}

# regex to extract base path and extension from a file path
reg_ext  = function(extra = '') {
  sprintf('([.](([%s[:alnum:]]+|tar[.](gz|bz2|xz)|nb[.]html)[~#]?))$', extra)
}
reg_path = function(...) paste0('^(.*?)', reg_ext(...))

#' Normalize paths
#'
#' A wrapper function of \code{normalizePath()} with different defaults.
#' @param x,winslash,must_work Arguments passed to
#'   \code{\link{normalizePath}()}.
#' @param resolve_symlink Whether to resolve symbolic links.
#' @export
#' @examples library(xfun)
#' normalize_path('~')
normalize_path = function(x, winslash = '/', must_work = FALSE, resolve_symlink = TRUE) {
  if (!resolve_symlink) {
    # apply the trick on all files on Windows since Sys.readlink() doesn't work
    # and we can't know which files are symlinks
    i = if (is_windows()) file_test('-f', x) else is_symlink(x)
    b = basename(x[i])
    x[i] = dirname(x[i])  # normalize the dirs of symlinks instead
  }
  res = normalizePath(x, winslash = winslash, mustWork = must_work)
  if (is_windows()) res[is.na(x)] = NA
  if (!resolve_symlink) {
    res[i] = file.path(res[i], b, fsep = winslash)
  }
  res
}

is_symlink = function(x) {
  !is.na(y <- Sys.readlink(x)) & (y != '')
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

#' Find file paths that exist
#'
#' This is a shorthand of \code{x[file.exists(x)]}, and optionally returns the
#' first existing file path.
#' @param x A vector of file paths.
#' @param first Whether to return the first existing path. If \code{TRUE} and no
#'   specified files exist, it will signal an error unless the argument
#'   \code{error = FALSE}.
#' @param error Whether to throw an error when \code{first = TRUE} but no files
#'   exist. It can also take a character value, which will be used as the error
#'   message.
#' @return A vector of existing file paths.
#' @export
#' @examples
#' xfun::existing_files(c('foo.txt', system.file('DESCRIPTION', package='xfun')))
existing_files = function(x, first = FALSE, error = TRUE) {
  x = x[file_exists(x)]
  if (!first) return(x)
  x = head(x, 1)
  if (length(x) != 1 && !isFALSE(error)) {
    if (isTRUE(error)) error = 'None of the specified files exist.'
    stop(error, call. = FALSE)
  }
  x
}

#' Return the (possible) root directory of a project
#'
#' Given a path of a file (or dir) in a potential project (e.g., an R package or
#' an RStudio project), return the path to the project root directory.
#'
#' The search for the root directory is performed by a series of tests,
#' currently including looking for a \file{DESCRIPTION} file that contains
#' \code{Package: *} (which usually indicates an R package), and a
#' \file{*.Rproj} file that contains \code{Version: *} (which usually indicates
#' an RStudio project). If files with the expected patterns are not found in the
#' initial directory, the search will be performed recursively in upper-level
#' directories.
#' @param path The initial path to start the search. If it is a file path, its
#'   parent directory will be used.
#' @param rules A matrix of character strings of two columns: the first column
#'   contains regular expressions to look for filenames that match the patterns,
#'   and the second column contains regular expressions to match the content of
#'   the matched files. The regular expression can be an empty string, meaning
#'   that it will match anything.
#' @return Path to the root directory if found, otherwise \code{NULL}.
#' @export
#' @note This function was inspired by the \pkg{rprojroot} package, but is much
#'   less sophisticated. It is a rather simple function designed to be used in
#'   some of packages that I maintain, and may not meet the need of general
#'   users until this note is removed in the future (which should be unlikely).
#'   If you are sure that you are working on the types of projects mentioned in
#'   the \sQuote{Details} section, this function may be helpful to you,
#'   otherwise please consider using \pkg{rprojroot} instead.
proj_root = function(path = './', rules = root_rules) {
  path = normalize_path(path)
  dir = if (dir_exists(path)) path else dirname(path)
  if (same_path(dir, file.path(dir, '..'))) return()
  if (is.null(dim(rules))) dim(rules) = c(1, length(rules))
  for (i in seq_len(nrow(rules))) {
    file = rules[i, 1]; pattern = rules[i, 2]
    for (f in list.files(dir, file, full.names = TRUE)) {
      if (pattern == '' || length(grep(pattern, read_utf8(f)))) return(dir)
    }
  }
  proj_root(dirname(dir), rules)
}

#' @rdname proj_root
#' @export
root_rules = matrix(c(
  '^DESCRIPTION$', '^Package: ',
  '.+[.]Rproj$',   '^Version: '
), ncol = 2, byrow = TRUE, dimnames = list(NULL, c('file', 'pattern')))

#' Get the relative path of a path relative to a directory
#'
#' Given a directory, return the relative path that is relative to this
#' directory. For example, the path \file{foo/bar.txt} relative to the directory
#' \file{foo/} is \file{bar.txt}, and the path \file{/a/b/c.txt} relative to
#' \file{/d/e/} is \file{../../a/b/c.txt}.
#' @param dir Path to a directory.
#' @param x A vector of paths to be converted to relative paths.
#' @param use.. Whether to use double-dots (\file{..}) in the relative path. A
#'   double-dot indicates the parent directory (starting from the directory
#'   provided by the \code{dir} argument).
#' @param error Whether to signal an error if a path cannot be converted to a
#'   relative path.
#' @return A vector of relative paths if the conversion succeeded; otherwise the
#'   original paths when \code{error = FALSE}, and an error when \code{error =
#'   TRUE}.
#' @export
#' @examples
#' xfun::relative_path('foo/bar.txt', 'foo/')
#' xfun::relative_path('foo/bar/a.txt', 'foo/haha/')
#' xfun::relative_path(getwd())
relative_path = function(x, dir = '.', use.. = TRUE, error = TRUE) {
  res = x
  for (i in seq_along(x)) res[i] = relative_path_one(x[i], dir, use.., error)
  res
}

relative_path_one = function(x, dir, use.., error) {
  # on Windows, if a relative path doesn't exist, normalizePath() will use
  # getwd() as its parent dir; however, normalizePath() just returns the
  # relative path on *nix, and we have to assume it's relative to getwd()
  abs_path = function(p) {
    if (!file.exists(p) && is_unix() && is_rel_path(p)) p = file.path(getwd(), p)
    normalize_path(p)
  }
  p = abs_path(x); n1 = nchar(p)
  if ((n1 <- nchar(p)) == 0) return(x)  # not sure what you mean
  d = abs_path(dir); n2 = nchar(d)
  if (is_sub_path(p, d, n2)) {
    p2 = get_subpath(p, n1, n2)
    if (p2 == '') p2 = '.'  # if the subpath is empty, it means the current dir
    return(p2)
  }
  if (!use..) {
    if (error) stop("When use.. = FALSE, the path 'x' must be under the 'dir'")
    return(x)
  }
  s = '../'; d1 = d
  while (!is_sub_path(p, d2 <- dirname(d1))) {
    if (same_path(d1, d2)) {
      if (error) stop(
        "The path 'x' cannot be converted to a relative path to 'dir'. ",
        "Perhaps they are on different volumes of the disk."
      )
      return(x)
    }
    s = paste0('../', s)
    d1 = d2  # go to one level up
  }
  paste0(s, get_subpath(p, n1, nchar(d2)))
}

#' Test if a path is a subpath of a dir
#'
#' Check if the path starts with the dir path.
#' @inheritParams is_abs_path
#' @param dir A vector of directory paths.
#' @param n The length of \code{dir} paths.
#' @return A logical vector.
#' @note You may want to normalize the values of the \code{x} and \code{dir}
#'   arguments first (with \code{xfun::\link{normalize_path}()}), to make sure
#'   the path separators are consistent.
#' @export
#' @examples
#' xfun::is_sub_path('a/b/c.txt', 'a/b')  # TRUE
#' xfun::is_sub_path('a/b/c.txt', 'd/b')  # FALSE
#' xfun::is_sub_path('a/b/c.txt', 'a\\b')  # FALSE (even on Windows)
is_sub_path = function(x, dir, n = nchar(dir)) substr(x, 1, n) == dir

# remove the first n2 characters and the possible / from the path
get_subpath = function(p, n1, n2) {
  p = substr(p, n2 + 1, n1)
  sub('^/', '', p)
}

#' Test if paths are relative or absolute
#'
#' On Unix, check if the paths start with \file{/} or \file{~} (if they do, they
#' are absolute paths). On Windows, check if a path remains the same (via
#' \code{xfun::\link{same_path}()}) if it is prepended with \file{./} (if it
#' does, it is a relative path).
#' @param x A vector of paths.
#' @return A logical vector.
#' @export
#' @examples
#' xfun::is_abs_path(c('C:/foo', 'foo.txt', '/Users/john/', tempdir()))
#' xfun::is_rel_path(c('C:/foo', 'foo.txt', '/Users/john/', tempdir()))
is_abs_path = function(x) {
  if (is_unix()) grepl('^[/~]', x) else !same_path(x, file.path('.', x))
}

#' @rdname is_abs_path
#' @export
is_rel_path = function(x) !is_abs_path(x)

#' Test if a path is a web path
#'
#' Check if a path starts with \file{http://} or \file{https://} or
#' \file{ftp://} or \file{ftps://}.
#' @inheritParams is_abs_path
#' @return A logical vector.
#' @export
#' @examples
#' xfun::is_web_path('https://www.r-project.org')  # TRUE
#' xfun::is_web_path('www.r-project.org')  # FALSE
is_web_path = function(x) {
  grepl('^(f|ht)tps?://', x)
}

#' Get the relative path of a path in a project relative to the current working
#' directory
#'
#' First compose an absolute path using the project root directory and the
#' relative path components, i.e., \code{\link{file.path}(root, ...)}. Then
#' convert it to a relative path with \code{\link{relative_path}()}, which is
#' relative to the current working directory.
#'
#' This function was inspired by \code{here::here()}, and the major difference
#' is that it returns a relative path by default, which is more portable.
#' @param ... A character vector of path components \emph{relative to the root
#'   directory of the project}.
#' @param root The root directory of the project.
#' @param error Whether to signal an error if the path cannot be converted to a
#'   relative path.
#' @return A relative path, or an error when the project root directory cannot
#'   be determined or the conversion failed and \code{error = TRUE}.
#' @export
#' @examples
#' \dontrun{
#' xfun::from_root('data', 'mtcars.csv')
#' }
from_root = function(..., root = proj_root(), error = TRUE) {
  if (is.null(root)) stop('Cannot determin the root directory of the current project.')
  p = file.path(root, ..., fsep = '/')
  relative_path(p, error = error)
}

#' Find a file or directory under a root directory
#'
#' Given a path, try to find it recursively under a root directory. The input
#' path can be an incomplete path, e.g., it can be a base filename, and
#' \code{magic_path()} will try to find this file under subdirectories.
#' @param ... A character vector of path components.
#' @param root The root directory under which to search for the path. If
#'   \code{NULL}, the current working directory is used.
#' @param relative Whether to return a relative path.
#' @param error Whether to signal an error if the path is not found, or multiple
#'   paths are found.
#' @param message Whether to emit a message when multiple paths are found and
#'   \code{error = FALSE}.
#' @param n_dirs The number of subdirectories to recursively search. The
#'   recursive search may be time-consuming when there are a large number of
#'   subdirectories under the root directory. If you really want to search for
#'   all subdirectories, you may try \code{n_dirs = Inf}.
#' @return The path found under the root directory, or an error when \code{error
#'   = TRUE} and the path is not found (or multiple paths are found).
#' @export
#' @examples
#' \dontrun{
#' xfun::magic_path('mtcars.csv')  # find any file that has the base name mtcars.csv
#' }
magic_path = function(
  ..., root = proj_root(), relative = TRUE, error = TRUE,
  message = getOption('xfun.magic_path.message', TRUE),
  n_dirs = getOption('xfun.magic_path.n_dirs', 10000)
) {
  if (file.exists(p <- file.path(...))) return(p)
  if (is.null(root)) root = getwd()
  nd = 0
  # find a path 'f' recursively under a directory 'd'
  find_it = function(f, d) {
    if (nd > n_dirs) {
      if (error) stop(
        'Failed to find the path under ', n_dirs, ' subdirectories. If you want ',
        'to search for the path in more subdirectories, increase the value of ',
        "the 'n_dirs' argument of magic_path()."
      )
      return(p)
    }
    ds = list.files(d, full.names = TRUE)
    ds = ds[dir_exists(ds)]
    if ((n1 <- length(ds)) == 0) return()
    nd <<- nd + n1
    fs = file.path(ds, f)
    fs = fs[file.exists(fs)]
    if ((n2 <- length(fs)) == 1) return(fs)
    if (n2 > 1) {
      msg = c(
        'Found more than one path containg the input path "', f, '":\n\n',
        paste('*', fs, collapse = '\n')
      )
      if (error) stop(msg)
      if (message) base::message(msg, '\n\nReturned the first one.')
      return(fs[1])
    }
    # look into subdirectories one by one
    for (i in seq_len(n1)) {
      fs = find_it(f, file.path(ds[i]))
      if (length(fs)) return(fs)
    }
  }
  f = find_it(p, root)
  if (is.null(f)) {
    if (error) stop('Could not find the path "', p, '" in any subdirectories.')
    p
  } else {
    if (relative) relative_path(f, error = error) else f
  }
}

#' Test the existence of files and directories
#'
#' These are wrapper functions of \code{utils::\link{file_test}()} to test the
#' existence of directories and files. Note that \code{file_exists()} only tests
#' files but not directories, which is the main difference between
#' \code{\link{file.exists}()} in base R. If you use are using the R version
#' 3.2.0 or above, \code{dir_exists()} is the same as \code{\link{dir.exists}()}
#' in base R.
#' @param x A vector of paths.
#' @export
#' @return A logical vector.
dir_exists = function(x) file_test('-d', x)

#' @rdname dir_exists
#' @export
file_exists = function(x) file_test('-f', x)

#' Create a directory recursively by default
#'
#' First check if a directory exists. If it does, return \code{TRUE}, otherwise
#' create it with \code{\link{dir.create}(recursive = TRUE)} by default.
#' @param x A path name.
#' @param recursive Whether to create all directory components in the path.
#' @param ... Other arguments to be passed to \code{\link{dir.create}()}.
#' @return A logical value indicating if the directory either exists or is
#'   successfully created.
#' @export
dir_create = function(x, recursive = TRUE, ...) {
  dir_exists(x) || dir.create(x, recursive = recursive, ...)
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
  structure(setNames(files2, files), class = 'xfun_rename_seq')
}

#' @export
print.xfun_rename_seq = function(x, ...) {
  x = unclass(x)
  tab = data.frame(original = names(x), ' ' = '->', new = unname(x), check.names = FALSE)
  if (loadable('knitr')) tab = knitr::kable(tab, 'simple')
  print(tab)
}

# return path to R's svg logo if it exists, otherwise return the jpg logo; or
# specify a regex to match the logo path, e.g., ext = 'jpg$'
R_logo = function(ext = NULL, all = FALSE) {
  x = file.path(R.home('doc'), 'html', c('Rlogo.svg', 'logo.jpg'))
  if (!is.null(ext)) x = grep(ext, x, value = TRUE)
  existing_files(x, first = !all)
}

#' Extract filenames from a URLs
#'
#' Get the base names of URLs via \code{\link{basename}()}, and remove the
#' possible query parameters or hash from the names.
#' @param x A character vector of URLs.
#' @return A character vector of filenames at the end of URLs.
#' @export
#' @examples
#' xfun::url_filename('https://yihui.org/images/logo.png')
#' xfun::url_filename('https://yihui.org/index.html')
#' xfun::url_filename('https://yihui.org/index.html?foo=bar')
#' xfun::url_filename('https://yihui.org/index.html#about')
url_filename = function(x) {
  gsub('[?#].*$', '', basename(x))  # remove query/hash from url
}

#' Delete an empty directory
#'
#' Use \code{list.file()} to check if there are any files or subdirectories
#' under a directory. If not, delete this empty directory.
#' @param dir Path to a directory. If \code{NULL} or the directory does not
#'   exist, no action will be performed.
#' @export
del_empty_dir = function(dir) {
  if (is.null(dir) || !dir_exists(dir)) return()
  files = list.files(dir, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0) unlink(dir, recursive = TRUE)
}

#' Mark some paths as directories
#'
#' Add a trailing backlash to a file path if this is a directory. This is useful
#' in messages to the console for example to quickly identify directories from
#' files.
#'
#' If \code{x} is a vector of relative paths, directory test is done with
#' path relative to the current working dir. Use \code{xfun::\link{in_dir}()} or
#' use absolute paths.
#'
#' @param x Character vector of paths to files and directories.
#' @examples
#' mark_dirs(list.files(find.package("xfun"), full.names = TRUE))
#' @export
mark_dirs = function(x) {
  i = dir_exists(x) & !grepl("/$", x)
  x[i] = paste0(x[i], "/")
  x
}

# change list.files()'s default argument values
all_files = function(
  pattern = NULL, dir = '.', ignore.case = TRUE, full.names = TRUE,
  recursive = TRUE, ...
) {
  list.files(
    dir, pattern, ignore.case = ignore.case, full.names = full.names,
    recursive = recursive, no.. = TRUE, ...
  )
}
