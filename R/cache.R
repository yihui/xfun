#' Cache the value of an R expression to an RDS file
#'
#' Save the value of an expression to a cache file (of the RDS format). Next
#' time the value is loaded from the file if it exists.
#'
#' Note that the `file` argument does not provide the full cache filename. The
#' actual name of the cache file is of the form \file{BASENAME_HASH.rds}, where
#' \file{BASENAME} is the base name provided via the \file{file} argument (e.g.,
#' if `file = 'foo.rds'`, `BASENAME` would be \file{foo}), and \file{HASH} is
#' the MD5 hash (also called the \sQuote{checksum}) calculated from the R code
#' provided to the `expr` argument and the value of the `hash` argument, which
#' means when the code or the `hash` argument changes, the \file{HASH} string
#' may also change, and the old cache will be invalidated (if it exists). If you
#' want to find the cache file, look for \file{.rds} files that contain 32
#' hexadecimal digits (consisting of 0-9 and a-z) at the end of the filename.
#'
#' The possible ways to invalidate the cache are: 1) change the code in `expr`
#' argument; 2) delete the cache file manually or automatically through the
#' argument `rerun = TRUE`; and 3) change the value of the `hash` argument. The
#' first two ways should be obvious. For the third way, it makes it possible to
#' automatically invalidate the cache based on changes in certain R objects. For
#' example, when you run `cache_rds({ x + y })`, you may want to invalidate the
#' cache to rerun `{ x + y }` when the value of `x` or `y` has been changed, and
#' you can tell `cache_rds()` to do so by `cache_rds({ x + y }, hash = list(x,
#' y))`. The value of the argument `hash` is expected to be a list, but it can
#' also take a special value, `"auto"`, which means `cache_rds(expr)` will try
#' to automatically figure out the global variables in `expr`, return a list of
#' their values, and use this list as the actual value of `hash`. This behavior
#' is most likely to be what you really want: if the code in `expr` uses an
#' external global variable, you may want to invalidate the cache if the value
#' of the global variable has changed. Here a \dQuote{global variable} means a
#' variable not created locally in `expr`, e.g., for `cache_rds({ x <- 1; x + y
#' })`, `x` is a local variable, and `y` is (most likely to be) a global
#' variable, so changes in `y` should invalidate the cache. However, you know
#' your own code the best. If you want to be completely sure when to invalidate
#' the cache, you can always provide a list of objects explicitly rather than
#' relying on `hash = "auto"`.
#'
#' By default (the argument `clean = TRUE`), old cache files will be
#' automatically cleaned up. Sometimes you may want to use `clean = FALSE` (set
#' the R global option `options(xfun.cache_rds.clean = FALSE)` if you want
#' `FALSE` to be the default). For example, you may not have decided which
#' version of code to use, and you can keep the cache of both versions with
#' `clean = FALSE`, so when you switch between the two versions of code, it will
#' still be fast to run the code.
#' @param expr An R expression.
#' @param rerun Whether to delete the RDS file, rerun the expression, and save
#'   the result again (i.e., invalidate the cache if it exists).
#' @param file The *base* (see Details) cache filename under the directory
#'   specified by the `dir` argument. If not specified and this function is
#'   called inside a code chunk of a \pkg{knitr} document (e.g., an R Markdown
#'   document), the default is the current chunk label plus the extension
#'   \file{.rds}.
#' @param dir The path of the RDS file is partially determined by `paste0(dir,
#'   file)`. If not specified and the \pkg{knitr} package is available, the
#'   default value of `dir` is the \pkg{knitr} chunk option `cache.path` (so if
#'   you are compiling a \pkg{knitr} document, you do not need to provide this
#'   `dir` argument explicitly), otherwise the default is \file{cache/}. If you
#'   do not want to provide a `dir` but simply a valid path to the `file`
#'   argument, you may use `dir = ""`.
#' @param hash A `list` object that contributes to the MD5 hash of the cache
#'   filename (see Details). It can also take a special character value
#'   `"auto"`. Other types of objects are ignored.
#' @param clean Whether to clean up the old cache files automatically when
#'   `expr` has changed.
#' @param ... Other arguments to be passed to [saveRDS()].
#' @note Changes in the code in the `expr` argument do not necessarily always
#'   invalidate the cache, if the changed code is [`parse`]`d` to the same
#'   expression as the previous version of the code. For example, if you have
#'   run `cache_rds({Sys.sleep(5);1+1})` before, running `cache_rds({ Sys.sleep(
#'   5 ) ; 1 + 1 })` will use the cache, because the two expressions are
#'   essentially the same (they only differ in white spaces). Usually you can
#'   add/delete white spaces or comments to your code in `expr` without
#'   invalidating the cache. See the package vignette `vignette('xfun', package
#'   = 'xfun')` for more examples.
#'
#'   When this function is called in a code chunk of a \pkg{knitr} document, you
#'   may not want to provide the filename or directory of the cache file,
#'   because they have reasonable defaults.
#'
#'   Side-effects (such as plots or printed output) will not be cached. The
#'   cache only stores the last value of the expression in `expr`.
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
#' - `$get(url, type, handler)` downloads a URL, caches it, and returns the file
#'   content according to the value of `type` (possible values: `"text"` means
#'   the text content; `"base64"` means the base64 encoded data; `"raw"` means
#'   the raw binary content; `"auto"` is the default and means the type is
#'   determined by the content type in the URL headers). Optionally a `handler`
#'   function can be applied to the content.
#' - `$list()` gives the list of cache files.
#' - `$summary()` gives a summary of existing cache files.
#' - `$remove(url, type)` removes a single cache file.
#' - `$purge()` deletes all cache files.
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
#' x3 = xfun::download_cache$get('https://yihui.org/images/logo.png', 'raw')
#' length(x3)
#'
#' # show a summary
#' xfun::download_cache$summary()
#' # remove a specific cache file
#' xfun::download_cache$remove('https://yihui.org/images/logo.png', 'raw')
#' # remove all cache files
#' xfun::download_cache$purge()
download_cache = local({
  pre = 'url'  # file prefix
  c_dir = function() {
    getOption('xfun.cache.dir', tools::R_user_dir('xfun', 'cache'))
  }
  c_file = function(url, type) {
    file.path(c_dir(), sprintf('%s_%s_%s.rds', pre, type, md5_obj(url)))
  }
  read = function(url, type) {
    if (length(f <- c_file(url, type)) && file.exists(f)) readRDS(f)
  }
  write = function(url, type, data) {
    if (length(f <- c_file(url, type))) {
      dir_create(dirname(f))
      saveRDS(data, f)
    }
  }
  list_cache = function() {
    d = c_dir()
    list.files(d, sprintf('^%s_.+[.]rds$', pre), full.names = TRUE)
  }
  list(
    get = function(url, type = c('auto', 'text', 'base64', 'raw'), handler = NULL) {
      type = type[1]
      if (!is.null(x <- read(url, type))) return(x[[url]])
      if ((auto <- type == 'auto')) type = if (length(grep(
        '^content-type:\\s+(text/.+|[^;]+;\\s+charset=utf-8)\\s*$',
        curlGetHeaders(url), ignore.case = TRUE
      ))) 'text' else 'raw'
      dir_create(d <- tempfile())
      on.exit(unlink(d, recursive = TRUE), add = TRUE)
      x = in_dir(d, {
        o = url_filename(url)
        download_file(url, o)
        switch(
          type, text = read_utf8(o), base64 = base64_uri(o), raw = read_bin(o)
        )
      })
      if (is.function(handler)) x = handler(x)
      write(url, if (auto) 'auto' else type, setNames(list(x), url))
      x
    },
    summary = function() {
      f = list_cache()
      if (length(f) == 0) return(invisible())
      t = gsub('^url_([^_]+)_.+$', '\\1', basename(f))
      u = vapply(f, function(x) names(readRDS(x)), character(1))
      s = file.size(f)
      if (length(f) > 1) message('Total size: ', format_bytes(sum(s)))
      d = data.frame(url = u, type = t, size = s, size_h = format_bytes(s))
      rownames(d) = NULL
      unname(split(d, seq_len(nrow(d))))
    },
    list = list_cache,
    remove = function(url, type = 'auto') file.remove(c_file(url, type)),
    purge = function() {
      f = list_cache()
      s = file.size(f)
      i = file.remove(f)
      message(sprintf(
        "Purged %d cache file(s) from '%s' (%s)",
        sum(i), c_dir(), format_bytes(sum(s[i]))
      ))
    }
  )
})
