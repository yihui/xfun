#' Cache the value of an R expression to an RDS file
#'
#' Save the value of an expression to a cache file (of the RDS format). Next
#' time the value is loaded from the file if it exists.
#'
#' Note that the \code{file} argument does not provide the full cache filename.
#' The actual name of the cache file is of the form \file{BASENAME_HASH.rds},
#' where \file{BASENAME} is the base name provided via the \file{file} argument
#' (e.g., if \code{file = 'foo.rds'}, \code{BASENAME} would be \file{foo}), and
#' \file{HASH} is the MD5 hash (also called the \sQuote{checksum}) calculated
#' from the R code provided to the \code{expr} argument and the value of the
#' \code{hash} argument, which means when the code or the \code{hash} argument
#' changes, the \file{HASH} string may also change, and the old cache will be
#' invalidated (if it exists). If you want to find the cache file, look for
#' \file{.rds} files that contain 32 hexadecimal digits (consisting of 0-9 and
#' a-z) at the end of the filename.
#'
#' The possible ways to invalidate the cache are: 1) change the code in
#' \code{expr} argument; 2) delete the cache file manually or automatically
#' through the argument \code{rerun = TRUE}; and 3) change the value of the
#' \code{hash} argument. The first two ways should be obvious. For the third
#' way, it makes it possible to automatically invalidate the cache based on
#' changes in certain R objects. For example, when you run \code{cache_rds({ x +
#' y })}, you may want to invalidate the cache to rerun \code{{ x + y }} when
#' the value of \code{x} or \code{y} has been changed, and you can tell
#' \code{cache_rds()} to do so by \code{cache_rds({ x + y }, hash = list(x,
#' y))}. The value of the argument \code{hash} is expected to be a list, but it
#' can also take a special value, \code{"auto"}, which means
#' \code{cache_rds(expr)} will try to automatically figure out the global
#' variables in \code{expr}, return a list of their values, and use this list as
#' the actual value of \code{hash}. This behavior is most likely to be what you
#' really want: if the code in \code{expr} uses an external global variable, you
#' may want to invalidate the cache if the value of the global variable has
#' changed. Here a \dQuote{global variable} means a variable not created locally
#' in \code{expr}, e.g., for \code{cache_rds({ x <- 1; x + y })}, \code{x} is a
#' local variable, and \code{y} is (most likely to be) a global variable, so
#' changes in \code{y} should invalidate the cache. However, you know your own
#' code the best. If you want to be completely sure when to invalidate the
#' cache, you can always provide a list of objects explicitly rather than
#' relying on \code{hash = "auto"}.
#'
#' By default (the argument \code{clean = TRUE}), old cache files will be
#' automatically cleaned up. Sometimes you may want to use \code{clean = FALSE}
#' (set the R global option \code{options(xfun.cache_rds.clean = FALSE)} if you
#' want \code{FALSE} to be the default). For example, you may not have decided
#' which version of code to use, and you can keep the cache of both versions
#' with \code{clean = FALSE}, so when you switch between the two versions of
#' code, it will still be fast to run the code.
#' @param expr An R expression.
#' @param rerun Whether to delete the RDS file, rerun the expression, and save
#'   the result again (i.e., invalidate the cache if it exists).
#' @param file The \emph{base} (see Details) cache filename under the directory
#'   specified by the \code{dir} argument. If not specified and this function is
#'   called inside a code chunk of a \pkg{knitr} document (e.g., an R Markdown
#'   document), the default is the current chunk label plus the extension
#'   \file{.rds}.
#' @param dir The path of the RDS file is partially determined by
#'   \code{paste0(dir, file)}. If not specified and the \pkg{knitr} package is
#'   available, the default value of \code{dir} is the \pkg{knitr} chunk option
#'   \code{cache.path} (so if you are compiling a \pkg{knitr} document, you do
#'   not need to provide this \code{dir} argument explicitly), otherwise the
#'   default is \file{cache/}. If you do not want to provide a \code{dir} but
#'   simply a valid path to the \code{file} argument, you may use \code{dir =
#'   ""}.
#' @param hash A \code{list} object that contributes to the MD5 hash of the
#'   cache filename (see Details). It can also take a special character value
#'   \code{"auto"}. Other types of objects are ignored.
#' @param clean Whether to clean up the old cache files automatically when
#'   \code{expr} has changed.
#' @param ... Other arguments to be passed to \code{\link{saveRDS}()}.
#' @note Changes in the code in the \code{expr} argument do not necessarily
#'   always invalidate the cache, if the changed code is \code{\link{parse}d} to
#'   the same expression as the previous version of the code. For example, if
#'   you have run \code{cache_rds({Sys.sleep(5);1+1})} before, running
#'   \code{cache_rds({ Sys.sleep( 5 ) ; 1 + 1 })} will use the cache, because
#'   the two expressions are essentially the same (they only differ in white
#'   spaces). Usually you can add/delete white spaces or comments to your code
#'   in \code{expr} without invalidating the cache. See the package vignette
#'   \code{vignette('xfun', package = 'xfun')} for more examples.
#'
#'   When this function is called in a code chunk of a \pkg{knitr} document, you
#'   may not want to provide the filename or directory of the cache file,
#'   because they have reasonable defaults.
#'
#'   Side-effects (such as plots or printed output) will not be cached. The
#'   cache only stores the last value of the expression in \code{expr}.
#' @return If the cache file does not exist, run the expression and save the
#'   result to the file, otherwise read the cache file and return the value.
#' @export
#' @examples
#' f = tempfile()  # the cache file
#' compute = function(...) {
#'   res = xfun::cache_rds({
#'     Sys.sleep(1)
#'     1:10
#'   }, file = f, dir = '', ...)
#'   res
#' }
#' compute()  # takes one second
#' compute()  # returns 1:10 immediately
#' compute()  # fast again
#' compute(rerun = TRUE)  # one second to rerun
#' compute()
#' file.remove(f)
cache_rds = function(
  expr = {}, rerun = FALSE, file = 'cache.rds', dir = 'cache/',
  hash = NULL, clean = getOption('xfun.cache_rds.clean', TRUE), ...
) {
  if (loadable('knitr')) {
    if (missing(file) && !is.null(lab <- knitr::opts_current$get('label')))
      file = paste0(lab, '.rds')
    if (missing(dir) && !is.null(d <- knitr::opts_current$get('cache.path')))
      dir = d
  }
  path = paste0(dir, file)
  if (!grepl(r <- '([.]rds)$', path)) path = paste0(path, '.rds')
  code = deparse(substitute(expr))
  md5  = md5_obj(code)
  if (identical(hash, 'auto')) hash = global_vars(code, parent.frame(2))
  if (is.list(hash)) md5 = md5_obj(c(md5, md5_obj(hash)))
  path = sub(r, paste0('_', md5, '\\1'), path)
  if (rerun) unlink(path)
  if (clean) clean_cache(path)
  if (file_exists(path)) readRDS(path) else {
    obj = expr  # lazy evaluation
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(obj, path, ...)
    obj
  }
}

# write an object to a file and return the md5 sum
md5_obj = function(x) {
  f = tempfile(); on.exit(unlink(f), add = TRUE)
  if (is.character(x)) writeLines(x, f) else saveRDS(x, f)
  unname(tools::md5sum(f))
}

# clean up old cache files (those with the same base names as the new cache
# file, e.g., if the new file is FOO_0123abc...z.rds, then FOO_9876def...x.rds
# should be deleted)
clean_cache = function(path) {
  olds = list.files(dirname(path), '_[0-9a-f]{32}[.]rds$', full.names = TRUE)
  olds = c(olds, path)  # `path` may not exist; make sure it is in target paths
  base = basename(olds)
  keep = basename(path) == base  # keep this file (will cache to this file)
  base = substr(base, 1, nchar(base) - 37)  # 37 = 1 (_) + 32 (md5 sum) + 4 (.rds)
  unlink(olds[(base == base[keep][1]) & !keep])
}

# analyze code and find out global variables
find_globals = function(code) {
  fun = eval(parse_only(c('function(){', code, '}')))
  setdiff(codetools::findGlobals(fun), known_globals)
}

known_globals = c(
  '{', '[', '(', ':', '<-', '=', '+', '-', '*', '/', '%%', '%/%', '%*%', '%o%', '%in%'
)

# return a list of values of global variables in code
global_vars = function(code, env) {
  if (length(vars <- find_globals(code)) > 0) mget(vars, env)
}

#' Download a file from a URL and cache it on disk
#'
#' This object provides methods to download files and cache them on disk.
#' @format A list of methods:
#'
#'   \code{$get(url, mode)} downloads a URL, caches it, and returns the file
#'   content according to the value of \code{mode} (possible values:
#'   \code{"text"} means the text content; \code{"base64"} means the base64
#'   encoded data; \code{"raw"} means the raw binary content; \code{"auto"} is
#'   the default and means the ).
#'
#'   \code{$summary()} gives a summary of existing cache files.
#'
#'   \code{$remove(url, mode)} removes a single cache file.
#'
#'   \code{$purge()} deletes all cache files.
#' @export
#' @examplesIf interactive()
#' # the first time it may take a few seconds
#' x1 = xfun::download_cache$get('https://www.r-project.org/')
#' head(x1)
#'
#' # now you can get the cached content
#' x2 = xfun::download_cache$get('https://www.r-project.org/')
#' identical(x1, x2)  # TRUE
#'
#' # a binary file
#' x3 = xfun::download_cache$get('https://yihui.org/images/logo.png')
#' length(x3)
#'
#' # show a summary
#' xfun::download_cache$summary()
#' # remove a specific cache file
#' xfun::download_cache$remove('https://yihui.org/images/logo.png')
#' # remove all cache files
#' xfun::download_cache$purge()
download_cache = local({
  pre = 'url'  # file prefix
  c_dir = function() {
    getOption('xfun.cache.dir', tools::R_user_dir('xfun', 'cache'))
  }
  c_file = function(url, mode) {
    file.path(c_dir(), sprintf('%s_%s_%s.rds', pre, mode, md5_obj(url)))
  }
  read = function(url, mode) {
    if (length(f <- c_file(url, mode)) && file.exists(f)) readRDS(f)
  }
  write = function(url, mode, data) {
    if (length(f <- c_file(url, mode))) {
      dir_create(dirname(f))
      saveRDS(data, f)
    }
  }
  list_cache = function() {
    d = c_dir()
    list.files(d, sprintf('^%s_.+[.]rds$', pre), full.names = TRUE)
  }
  list(
    get = function(url, mode = c('auto', 'text', 'base64', 'raw')) {
      mode = mode[1]
      if (!is.null(x <- read(url, mode))) return(x[[url]])
      if ((auto <- mode == 'auto')) mode = if (length(grep(
        '^content-type:\\s+(text/.+|[^;]+;\\s+charset=utf-8)\\s*$',
        curlGetHeaders(url), ignore.case = TRUE
      ))) 'text' else 'raw'
      dir_create(d <- tempfile())
      on.exit(unlink(d, recursive = TRUE), add = TRUE)
      x = in_dir(d, {
        o = url_filename(url)
        download_file(url, o)
        switch(
          mode, text = read_utf8(o), base64 = base64_uri(o), raw = read_bin(o)
        )
      })
      write(url, if (auto) 'auto' else mode, setNames(list(x), url))
      x
    },
    summary = function() {
      f = list_cache()
      if (length(f) == 0) return(invisible())
      t = gsub('^url_([^_]+)_.+$', '\\1', basename(f))
      u = vapply(f, function(x) names(readRDS(x)), character(1))
      s = file.size(f)
      if (length(f) > 1) {
        u = c(u, 'Total')
        t = c(t, '')
        s = c(s, sum(s))
      }
      d = data.frame(url = u, mode = t, size = s, size_h = format_bytes(s))
      rownames(d) = NULL
      d
    },
    remove = function(url, mode = 'auto') file.remove(c_file(url, mode)),
    purge = function() {
      f = list_cache()
      fs = file.size(f)
      i = file.remove(f)
      message(sprintf(
        "Purged %d cache file(s) from '%s' (%s)",
        sum(i), c_dir(), format_bytes(sum(fs[i]))
      ))
    }
  )
})
