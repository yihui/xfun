#' Cache the execution of an expression in memory or on disk
#'
#' Caching is based on the assumption that if the input does not change, the
#' output will not change. After an expression is executed for the first time,
#' its result will be saved (either in memory or on disk). The next run will be
#' skipped and the previously saved result will be loaded directly if all
#' external inputs of the expression remain the same, otherwise the cache will
#' be invalidated and the expression will be re-executed.
#'
#' Arguments supported in `...` include:
#'
#' - `vars`: Names of local variables (which are created inside the expression).
#' By default, local variables are automatically detected from the expression
#' via [find_locals()]. Locally created variables are cached along with the
#' value of the expression.
#'
#' - `hash` and `extra`: R objects to be used to determine if cache should be
#' loaded or invalidated. If (the MD5 hash of) the objects is not changed, the
#' cache is loaded, otherwise the cache is invalidated and rebuilt. By default,
#' `hash` is a list of values of global variables in the expression (i.e.,
#' variables created outside the expression). Global variables are automatically
#' detected by [find_globals()]. You can provide a vector of names to override
#' the automatic detection if you want some specific global variables to affect
#' caching, or the automatic detection is not reliable. You can also provide
#' additional information via the `extra` argument. For example, if the
#' expression reads an external file `foo.csv`, and you want the cache to be
#' invalidated after the file is modified, you may use `extra =
#' file.mtime("foo.csv")`.
#'
#' - `keep`: By default, only one copy of the cache corresponding to an `id`
#' under `path` is kept, and all other copies for this `id` is automatically
#' purged. If `TRUE`, all copies of the cache are kept. If `FALSE`, all copies
#' are removed, which means the cache is *always* invalidated, and can be useful
#' to force re-executing the expression.
#'
#' - `rw`: A list of functions to read/write the cache files. The list is of the
#' form `list(name = 'xxx', load = function(file) {}, save = function(x, file)
#' {})`. By default, [readRDS()] and [saveRDS()] are used. This argument can
#' also take a character string to use some built-in read/write methods.
#' Currently available methods include `rds` (the default), `raw` (using
#' [serialize()] and [unserialize()]), and `qs` (using [qs::qread()] and
#' [qs::qsave()]). The `rds` and `raw` methods only use base R functions (the
#' `rds` method generates smaller files because it uses compression, but is
#' often slower than the `raw` method, which does not use compression). The `qs`
#' method requires the \pkg{qs} package, which can be much faster than base R
#' methods and also supports compression.
#' @param expr An R expression to be cached.
#' @param path The path to save the cache. The special value `":memory:"` means
#'   in-memory caching. If it is intended to be a directory path, please make
#'   sure to add a trailing slash.
#' @param id A stable and unique string identifier for the expression to be used
#'   to identify a unique copy of cache for the current expression from all
#'   cache files (or in-memory elements). If not provided, an MD5 digest of the
#'   [deparse]d expression will be used, which means if the expression does not
#'   change (changes in comments or white spaces do not matter), the `id` will
#'   remain the same. This may not be a good default is two identical
#'   expressions are cached under the same `path`, because they could overwrite
#'   each other's cache when one expression's cache is invalidated, which may or
#'   may not be what you want. If you do not want that to happen, you need to
#'   manually provide an `id`.
#' @param ... More arguments to control the behavior of caching (see
#'   \sQuote{Details}).
#' @return If the cache is found, the cached value of the expression will be
#'   loaded and returned (other local variables will also be lazy-loaded into
#'   the current environment as a side-effect). If cache does not exist, the
#'   expression is executed and its value is returned.
#' @references See <https://yihui.org/litedown/#sec:option-cache> for how it
#'   works and an application in \pkg{litedown}.
#' @export
#' @examples
#' # the first run takes about 1 second
#' y1 = xfun::cache_exec({
#'   x = rnorm(1e5)
#'   Sys.sleep(1)
#'   x
#' }, path = ':memory:', id = 'sim-norm')
#'
#' # the second run takes almost no time
#' y2 = xfun::cache_exec({
#'   # comments won't affect caching
#'   x = rnorm(1e5)
#'   Sys.sleep(1)
#'   x
#' }, path = ':memory:', id = 'sim-norm')
#'
#' # y1, y2, and x should be identical
#' stopifnot(identical(y1, y2), identical(y1, x))
cache_exec = function(expr, path = 'cache/', id = NULL, ...) {
  use_cache = FALSE
  ret = cache_code(
    substitute(expr), parent.frame(), use_cache <- TRUE,
    list(path = path, id = id, ...)
  )
  if (use_cache) ret else expr
}

# a dictionary containing names and hashes of global variables
.cache_dict = new.env(parent = emptyenv())

# an in-memory database containing results from previous run, values of local
# variables, and their hashes, of the form environment(*__id__* = list(results,
# values, hashes))
.cache_db = new.env(parent = emptyenv())

cache_code = function(
  code, envir = parent.frame(), found = NULL, config = list(
    path = NULL, vars = NULL, hash = NULL, extra = NULL, keep = NULL, id = NULL,
    rw = NULL
  )
) {
  dict = .cache_dict
  if (!is.character(path <- config$path)) {
    # when caching is not enabled, we should clean up hashes for local variables
    # to make sure the next cached chunk will get the up-to-date hash
    vars = intersect(config$vars %||% find_locals(code), ls_all(dict))
    rm_vars(vars, dict)
    return()
  }

  # functions to read/write cache files
  if (is.null(rw <- config$rw)) rw = 'rds'
  if (is.character(rw)) rw = io_methods[[rw]]
  if (!is.character(rw$name) || length(rw$name) != 1) stop(
    'The cache method must have a name (as a character string).'
  )

  hash = config$hash %||% find_globals(code, envir)
  # get the values of hash variables (unless hash is I(character()))
  if (is.character(hash) && !inherits(hash, 'AsIs')) {
    # if a variable exists in the hash dictionary, use its hash (32 chars) in
    # the dict instead of its actual value, because the value may be large and
    # slow to serialize or compute md5 checksum
    hash = lapply(hash, function(x) dict[[x]] %||% get(x, envir))
  }
  # "normalized" code that doesn't rely on comments or white spaces
  code2 = deparse(if (is.character(code)) parse_only(code) else code)
  hash = md5_one(c(hash, config$extra, list(code2)))

  mem_cache = path == ':memory:'  # in-memory or disk cache

  # by default, the ID is a checksum of the code, which may not be a good idea
  # (e.g., two identical code fragments could be executed one after another
  # but they should not share the same cache); for litedown, id is chunk label
  if (is.null(id <- config$id)) id = md5_one(code2)
  id = paste0(path, if (mem_cache) '__', id, if (mem_cache) '__')

  file_pattern = function(x) sprintf('^[0-9a-z]{32}[.]%s([.][0-9]+)?$', x)
  # try to retrieve cache from memory (the dictionary) or disk
  cached = if (mem_cache) {
    db = .cache_db
    id_names = function(x) {
      x = ls_all(db)
      x[startsWith(x, id) & nchar(x) == nchar(id) + 32]
    }
    hits = id_names(db)
    # clean up all cache for this id, i.e., remove :memory:__id__hash
    if (base::isFALSE(config$keep)) rm_vars(hits, db)
    id = paste0(id, hash)
    id %in% ls_all(db)
  } else {
    db = list.files(id, full.names = TRUE)
    hits = grepl(file_pattern('[[:alnum:]]+'), basename(db))
    if (base::isFALSE(config$keep)) file.remove(db[hits])
    id = file.path(id, paste0(hash, '.', rw$name))
    file_exists(id)
  }
  # return now if cache is found
  if (cached) {
    # I could implement lazy-loading here but I'm not sure if it's worth it
    ret = if (mem_cache) db[[id]] else rw$load(id)
    list2env(ret$hashes, dict)  # update the hash dictionary
    # lazy-load cached objects into envir
    .mapply(function(name, file) {
      delayedAssign(name, if (mem_cache) ret$values[[name]] else rw$load(file), assign.env = envir)
    }, list(names(ret$hashes), sprintf('%s.%d', id, seq_along(ret$hashes))), NULL)
    found
    return(ret$result)
  }

  # clean up other versions of cache before saving a new version
  if (!isTRUE(config$keep)) {
    if (mem_cache) rm_vars(hits, db) else {
      # clean cache for the current method only; don't touch other methods' cache
      hits = grepl(file_pattern(rw$name), basename(db))
      file.remove(db[hits])
    }
  }
  vars = config$vars %||% find_locals(code)
  # inject an on.exit() call to the parent function to save its returned value;
  # note that returnValue() requires R >= 3.2.0 released on 2015-04-16, which I
  # hope is a reasonable requirement (anyone using R from a decade ago?)
  exit_call(function() {
    # we can't tell if returnValue() is from a successful call of a function or
    # not, so we pass a default that is very unlikely to be the returned value
    # on success; if we get this value, it's very likely that errors occurred
    void_return = new.env()  # should be unlikely for two new.env() to be identical
    res = returnValue(void_return)
    if (identical(res, void_return)) return()
    # save the checksums of objects to be used for a future run
    v1 = ls_all(envir)
    v2 = setdiff(vars, v1)
    if (length(v2)) warning(
      'Variable(s) not found in the environment: ', paste(v2, collapse = ', ')
    )
    hashes = as.list(do.call(md5, vals <- mget(vars, envir)))
    ret = list(result = res, hashes = hashes)
    if (mem_cache) {
      ret$values = vals
      db[[id]] = ret
    } else {
      dir_create(dirname(id))
      rw$save(ret, id)  # result and variable hashes are saved to the main file
      # variable values are saved in separate files of the form HASH.i; the i-th
      # file is for the i-th variable
      .mapply(rw$save, list(vals, sprintf('%s.%d', id, seq_along(vals))), NULL)
    }
    list2env(hashes, dict)
  })
}

# functions to load/save cache files
io_methods = list(
  raw = list(
    name = 'raw',
    load = function(...) unserialize(read_bin(...)),
    save = function(x, file, ...) {
      s = serialize(x, NULL, xdr = FALSE, ...)
      writeBin(s, file)
    }
  ),
  rds = list(
    name = 'rds',
    load = function(...) readRDS(...),
    save = function(x, file, ...) saveRDS(x, file, ...)
  ),
  qs = list(
    name = 'qs',
    load = function(...) qs::qread(...),
    save = function(x, file, ...) qs::qsave(x, file, ...)
  )
)

#' Cache the value of an R expression to an RDS file
#'
#' Save the value of an expression to a cache file (of the RDS format). Next
#' time the value is loaded from the file if it exists. Please consider using
#' [cache_exec()] instead, which is more flexible and intelligent.
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
#'
#'   The hash is calculated by saving the hash object to a temporary file via
#'   `saveRDS()` and getting the MD5 digest of the file. Since the file
#'   generated by `saveRDS()` may be platform-dependent, the cache may not be
#'   platform-agnostic. In other words, the cache saved on one platform may be
#'   invalidated on another platform even if nothing in the hash object has
#'   changed. By comparison, [cache_exec()] doesn't have this problem.
#' @return If the cache file does not exist, run the expression and save the
#'   result to the file, otherwise read the cache file and return the value.
#' @seealso [cache_exec()], which is more flexible (e.g., it supports in-memory
#'   caching and different read/write methods for cache files).
#' @export
#' @keywords internal
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
#' unlink(paste0(f, '_*.rds'))
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

# an old hash function for cache_rds() only
md5_obj = function(x) {
  f = tempfile(); on.exit(unlink(f), add = TRUE)
  if (is.character(x)) writeLines(x, f) else saveRDS(x, f)
  unname(tools::md5sum(f))
}

#' Calculate the MD5 checksums of R objects
#'
#' [Serialize][serialize()] an object and calculate the checksum via
#' [tools::md5sum()]. If `tools::md5sum()` does not have the argument `bytes`,
#' the object will be first serialized to a temporary file, which will be
#' deleted after the checksum is calculated, otherwise the raw bytes of the
#' object will be passed to the `bytes` argument directly (which will be
#' faster than writing to a temporary file).
#' @param ... Any number of R objects.
#' @return A character vector of the checksums of objects passed to `md5()`. If
#'   the arguments are named, the results will also be named.
#' @export
#' @examples
#' x1 = 1; x2 = 1:10; x3 = seq(1, 10); x4 = iris; x5 = paste
#' (m = xfun::md5(x1, x2, x3, x4, x5))
#' stopifnot(m[2] == m[3])  # x2 and x3 should be identical
#'
#' xfun::md5(x1 = x1, x2 = x2)  # named arguments
md5 = function(...) {
  obj = list(...)
  res = unlist(lapply(obj, md5_one))
  names(res) = names(obj)
  res
}

md5_one = function(x) {
  # no need to write to a file if md5sum() has the 'bytes' arg (R > 4.4.1)
  m = tools::md5sum
  if ('bytes' %in% names(formals(m))) f = NULL else {
    f = tempfile(); on.exit(unlink(f), add = TRUE)
  }
  s = serialize(x, NULL, xdr = FALSE)
  s = tail(s, -14)  # the first 14 bytes contain version info, etc
  if (is.null(f)) m(bytes = s) else {
    writeBin(s, f); unname(m(f))
  }
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

#' Find global/local variables in R code
#'
#' Use [codetools::findGlobals()] and [codetools::findLocalsList()] to find
#' global and local variables in a piece of code. Global variables are defined
#' outside the code, and local variables are created inside the code.
#' @param code Either a character vector of R source code, or an R expression.
#' @param envir The global environment in which global variables are to be
#'   found.
#' @return A character vector of the variable names. If the source code contains
#'   syntax errors, an empty character vector will be returned.
#' @note Due to the flexibility of creating and getting variables in R, these
#'   functions are not guaranteed to find all possible variables in the code
#'   (e.g., when the code is hidden behind `eval()`).
#' @export
#' @examples
#' x = 2
#' xfun::find_globals('y = x + 1')
#' xfun::find_globals("y = get('x') + 1")  # x is not recognized
#' xfun::find_globals('y = zzz + 1')  # zzz doesn't exist
#'
#' xfun::find_locals('y = x + 1')
#' xfun::find_locals("assign('y', x + 1)")  # it works
#' xfun::find_locals("assign('y', x + 1, new.env())")  # still smart
#' xfun::find_locals("eval(parse(text = 'y = x + 1'))")  # no way
find_globals = function(code, envir = parent.frame()) {
  fun = function() {}
  if (is.language(code)) {
    body(fun) = code
  } else {
    fun = eval(parse2(c('function(){', code, '}'), fun), baseenv())
  }
  obj = codetools::findGlobals(fun)
  intersect(obj, ls_all(envir, TRUE))
}

# ls() all objects in the envir and if recursive, all parent environments until
# globalenv() or emptyenv()
ls_all = function(envir, recursive = FALSE) {
  x = ls(envir, all.names = TRUE)
  if (!recursive) return(x)
  while (TRUE) {
    # in theory, we shouldn't stop at global env but should keep recursion, but
    # in practice, objects in the parent environments of globalenv() (often
    # package namespaces) are unlikely to change and won't affect cache
    if (identical(envir, emptyenv()) || identical(envir, .GlobalEnv)) break
    envir = parent.env(envir)
    x = c(x, ls(envir, all.names = TRUE))
  }
  unique(x)
}

#' @rdname find_globals
#' @export
find_locals = function(code) {
  code = if (is.language(code)) as.expression(code) else parse2(code)
  codetools::findLocalsList(code)
}

# if code cannot be parsed, return an empty expression
parse2 = function(code, fallback = expression()) {
  tryCatch(parse_only(code), error = function(e) fallback)
}

# return a list of values of global variables in code
global_vars = function(code, envir) {
  if (length(vars <- find_globals(code, envir)) > 0)
    mget(vars, envir, inherits = TRUE)
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
  c_file = function(url, type) {
    file.path(cache_dir(), sprintf('%s_%s_%s.rds', pre, type, md5_one(url)))
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
    d = cache_dir()
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
        sum(i), cache_dir(), format_bytes(sum(s[i]))
      ))
    }
  )
})

cache_dir = function() {
  getOption('xfun.cache.dir', tools::R_user_dir('xfun', 'cache'))
}
