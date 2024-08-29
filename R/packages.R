#' Attach or load packages, and automatically install missing packages if
#' requested
#'
#' `pkg_attach()` is a vectorized version of [library()] over
#' the `package` argument to attach multiple packages in a single function
#' call. `pkg_load()` is a vectorized version of
#' [requireNamespace()] to load packages (without attaching them).
#' The functions `pkg_attach2()` and `pkg_load2()` are wrappers of
#' `pkg_attach(install = TRUE)` and `pkg_load(install = TRUE)`,
#' respectively. `loadable()` is an abbreviation of
#' `requireNamespace(quietly = TRUE)`. `pkg_available()` tests if a
#' package with a minimal version is available.
#'
#' These are convenience functions that aim to solve these common problems: (1)
#' We often need to attach or load multiple packages, and it is tedious to type
#' several `library()` calls; (2) We are likely to want to install the
#' packages when attaching/loading them but they have not been installed.
#' @param ... Package names (character vectors, and must always be quoted).
#' @param install Whether to automatically install packages that are not
#'   available using [install.packages()]. Besides `TRUE` and
#'   `FALSE`, the value of this argument can also be a function to install
#'   packages (`install = TRUE` is equivalent to `install =
#'   install.packages`), or a character string `"pak"` (equivalent to
#'   `install = pak::pkg_install`, which requires the \pkg{pak} package).
#'   You are recommended to set a CRAN mirror in the global option `repos`
#'   via [options()] if you want to automatically install packages.
#' @param message Whether to show the package startup messages (if any startup
#'   messages are provided in a package).
#' @return `pkg_attach()` returns `NULL` invisibly. `pkg_load()`
#'   returns a logical vector, indicating whether the packages can be loaded.
#' @seealso `pkg_attach2()` is similar to `pacman::p_load()`, but does
#'   not allow non-standard evaluation (NSE) of the `...` argument, i.e.,
#'   you must pass a real character vector of package names to it, and all names
#'   must be quoted. Allowing NSE adds too much complexity with too little gain
#'   (the only gain is that it saves your effort in typing two quotes).
#' @import utils
#' @export
#' @examples library(xfun)
#' pkg_attach('stats', 'graphics')
#' # pkg_attach2('servr')  # automatically install servr if it is not installed
#'
#' (pkg_load('stats', 'graphics'))
pkg_attach = function(
  ..., install = FALSE, message = getOption('xfun.pkg_attach.message', TRUE)
) {
  if (!message) library = function(...) {
    suppressPackageStartupMessages(base::library(...))
  }
  for (i in c(...)) {
    if (!identical(install, FALSE) && !loadable(i)) pkg_install(i, install)
    library(i, character.only = TRUE)
  }
}

#' @param error Whether to signal an error when certain packages cannot be loaded.
#' @rdname pkg_attach
#' @export
pkg_load = function(..., error = TRUE, install = FALSE) {
  n = length(pkg <- c(...)); res = logical(n)
  if (n == 0) return(invisible(res))
  for (i in seq_len(n)) {
    res[i] = loadable(p <- pkg[i])
    if (!identical(install, FALSE) && !res[i]) {
      pkg_install(p, install); res[i] = loadable(p)
    }
  }
  if (error && any(!res)) stop('Package(s) not loadable: ', paste(pkg[!res], collapse = ' '))
  invisible(res)
}

#' @param pkg A single package name.
#' @param strict If `TRUE`, use [requireNamespace()] to test if
#'   a package is loadable; otherwise only check if the package is in
#'   [`.packages`]`(TRUE)` (this does not really load the package, so it
#'   is less rigorous but on the other hand, it can keep the current R session
#'   clean).
#' @param new_session Whether to test if a package is loadable in a new R
#'   session. Note that `new_session = TRUE` implies `strict = TRUE`.
#' @rdname pkg_attach
#' @export
loadable = function(pkg, strict = TRUE, new_session = FALSE) {
  if (length(pkg) != 1L) stop("'pkg' must be a character vector of length one")
  if (new_session) {
    Rscript(c('-e', shQuote(sprintf('library("%s")', pkg))), stdout = FALSE, stderr = FALSE) == 0
  } else {
    if (strict) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = TRUE))
    } else pkg %in% .packages(TRUE)
  }
}

#' @param version A minimal version number. If `NULL`, only test if a
#'   package is available and do not check its version.
#' @rdname pkg_attach
#' @export
pkg_available = function(pkg, version = NULL) {
  loadable(pkg) && (is.null(version) || packageVersion(pkg) >= version)
}

#' @rdname pkg_attach
#' @export
pkg_attach2 = function(...) pkg_attach(..., install = TRUE)

#' @rdname pkg_attach
#' @export
pkg_load2 = function(...) pkg_load(..., install = TRUE)

pkg_require = function(pkgs, which = length(sys.calls()) - 1) {
  f = func_name(which)
  for (p in pkgs) if (!loadable(p)) stop2(
    "The '", p, "' package is required by the function '", f, "' but not available.",
    if (is_R_CMD_check()) c(
      " If you are developing an R package, you need to declare the dependency on '",
      p, "' in the DESCRIPTION file (e.g., in 'Imports')."
    )
  )
}

# update all packages in libraries that are writable by the current user
pkg_update = function(...) {
  libs = .libPaths()
  libs = libs[file.access(libs, 2) >= 0]
  for (l in libs) update.packages(l, ask = FALSE, checkBuilt = TRUE, ...)
}

# allow users to specify a custom install.packages() function via the global
# option xfun.install.packages
pkg_install = function(pkgs, install = TRUE, ...) {
  if (length(pkgs) == 0) return()
  # in case the CRAN repo is not set up
  repos = getOption('repos')
  if (length(repos) == 0 || identical(repos, c(CRAN = '@CRAN@'))) {
    opts = options(repos = c(CRAN = 'https://cran.rstudio.com'))
    on.exit(options(opts), add = TRUE)
  }
  if (length(pkgs) > 1)
    message('Installing ', length(pkgs), ' packages: ', paste(pkgs, collapse = ' '))
  if (isTRUE(install)) install = getOption(
    'xfun.install.packages',
    if (is.na(Sys.getenv('RENV_PROJECT', NA)) || !loadable('renv')) install.packages else {
      function(pkgs, lib = NULL, ...) renv::install(pkgs, library = lib, ...)
    }
  )
  if (identical(install, 'pak')) install = pak::pkg_install
  retry(install, pkgs, ..., .pause = 0)
}

#' Find out broken packages and reinstall them
#'
#' If a package is broken (i.e., not [loadable()]), reinstall it.
#'
#' Installed R packages could be broken for several reasons. One common reason
#' is that you have upgraded R to a newer `x.y` version, e.g., from `4.0.5` to
#' `4.1.0`, in which case you need to reinstall previously installed packages.
#' @param reinstall Whether to reinstall the broken packages, or only list their
#'   names.
#' @return A character vector of names of broken package.
#' @export
broken_packages = function(reinstall = TRUE) {
  libs = .libPaths()
  pkgs = unlist(lapply(libs, function(lib) {
    p = unlist(lapply(.packages(TRUE, lib), function(p) {
      if (!loadable(p, new_session = TRUE)) p
    }))
    if (length(p) && reinstall) {
      remove.packages(p, lib); pkg_install(p, lib = lib)
    }
    p
  }))
  if(reinstall) invisible(pkgs) else pkgs
}

# remove (binary) packages that were built with a previous major-minor version of R
check_built = function(dir = '.', dry_run = TRUE) {
  ext = if (is_macos()) 'tgz' else if (is_windows()) 'zip' else 'tar.gz'
  r =  paste0('_[-.0-9]+[.]', ext, '$')
  pkgs = list.files(dir, r, full.names = TRUE)
  meta = file.path(dir, 'PACKAGES')
  info = if (file_exists(meta)) read.dcf(meta)
  extract = if (grepl('gz$', ext)) untar else unzip
  for (f in pkgs) {
    d = file.path(gsub(r, '', basename(f)), 'DESCRIPTION')
    extract(f, d)
    if (is.na(b <- read.dcf(d, 'Built')[1, 1])) next
    unlink(dirname(d), recursive = TRUE)
    v = as.numeric_version(gsub('^\\s*R ([^;]+);.*', '\\1', b))
    if (major_minor_smaller(v, getRversion())) {
      message('The package ', f, ' was built with R ', v)
      if (!dry_run) file.remove(f)
    }
  }
  if (!is.null(info) && !dry_run) tools::write_PACKAGES(dir)
}

# is one version smaller than the other in major.minor? e.g., 4.1.0 is smaller
# than 4.2.0, but not smaller than 4.1.1
major_minor_smaller = function(v1, v2) {
  v1 = unclass(v1)[[1]]
  v2 = unclass(v2)[[1]]
  if (length(v1) < 3 || length(v2) < 3) return(TRUE)  # should return NA
  v1[1] < v2[1] || v1[2] < v2[2]
}

#' Install a source package from a directory
#'
#' Run \command{R CMD build} to build a tarball from a source directory, and run
#' \command{R CMD INSTALL} to install it.
#' @param pkg The package source directory.
#' @param build Whether to build a tarball from the source directory. If
#'   `FALSE`, run \command{R CMD INSTALL} on the directory directly (note
#'   that vignettes will not be automatically built).
#' @param build_opts The options for \command{R CMD build}.
#' @param install_opts The options for \command{R CMD INSTALL}.
#' @export
#' @return Invisible status from \command{R CMD INSTALL}.
install_dir = function(pkg, build = TRUE, build_opts = NULL, install_opts = NULL) {
  if (build) {
    pkg = pkg_build(pkg, build_opts)
    on.exit(unlink(pkg), add = TRUE)
  }
  res = Rcmd(c('INSTALL', install_opts, pkg))
  if (res != 0) stop('Failed to install the package ', pkg)
  invisible(res)
}

pkg_build = function(dir = '.', opts = NULL) {
  desc = file.path(dir, 'DESCRIPTION')
  pv = read.dcf(desc, fields = c('Package', 'Version'))
  # delete existing tarballs
  unlink(sprintf('%s_*.tar.gz', pv[1, 1]))
  Rcmd(c('build', opts, shQuote(dir)))
  pkg = sprintf('%s_%s.tar.gz', pv[1, 1], pv[1, 2])
  if (!file_exists(pkg)) stop('Failed to build the package ', pkg)
  pkg
}

# query the Homebrew dependencies of an R package
brew_dep = function(pkg) {
  u = sprintf('https://sysreqs.r-hub.io/pkg/%s/osx-x86_64-clang', pkg)
  x = retry(readLines, u, warn = FALSE)
  x = gsub('^\\s*\\[|\\]\\s*$', '', x)
  x = unlist(strsplit(gsub('"', '', x), '[, ]+'))
  x = setdiff(x, 'null')
  if (length(x))
    message('Package ', pkg, ' requires Homebrew packages: ', paste(x, collapse = ' '))
  x
}
brew_deps = function(pkgs) {
  if (length(pkgs) == 0) return()
  deps = pkg_brew_deps()
  unlist(lapply(pkgs, function(p) {
    if (is.null(deps[[p]])) brew_dep(p) else deps[[p]]
  }))
}

pkg_brew_deps = function() {
  con = url('https://macos.rbind.io/bin/macosx/sysreqsdb.rds')
  on.exit(close(con), add = TRUE)
  readRDS(con)
}

install_brew_deps = function(pkg = .packages(TRUE)) {
  inst = installed.packages()
  pkg = intersect(pkg, pkg_needs_compilation(inst))
  deps = pkg_brew_deps()
  deps = deps[c(pkg, pkg_dep(pkg, inst, recursive = TRUE))]
  deps = paste(na.omit(unique(unlist(deps))), collapse = ' ')
  if (deps != '') system(paste('brew install', deps))
}

pkg_needs_compilation = function(db = installed.packages()) {
  pkgs = unname(db[tolower(db[, 'NeedsCompilation']) == 'yes', 'Package'])
  pkgs[!is.na(pkgs)]
}

#' An alias of `remotes::install_github()`
#'
#' This alias is to make autocomplete faster via `xfun::install_github`, because
#' most `remotes::install_*` functions are never what I want. I only use
#' `install_github` and it is inconvenient to autocomplete it, e.g.
#' `install_git` always comes before `install_github`, but I never use it. In
#' RStudio, I only need to type `xfun::ig` to get `xfun::install_github`.
#' @param ... Arguments to be passed to [remotes::install_github()].
#' @export
install_github = function(...) remotes::install_github(...)

# Remove packages not installed from CRAN
reinstall_from_cran = function(dry_run = TRUE, skip_github = TRUE) {
  r = paste(c('Repository', if (skip_github) 'GithubRepo'), collapse = '|')
  r = paste0('^(', r, '): ')
  for (lib in .libPaths()) {
    pkgs = .packages(TRUE, lib)
    pkgs = setdiff(pkgs, c('xfun', 'rstudio', base_pkgs()))
    for (p in pkgs) {
      desc = read_utf8(system.file('DESCRIPTION', package = p, lib.loc = lib))
      if (!any(grepl(r, desc))) {
        if (dry_run) message(p, ': ', lib) else install.packages(p, lib = lib)
      }
    }
  }
}

#' Convert package news to the Markdown format
#'
#' Read the package news with [news()], convert the result to
#' Markdown, and write to an output file (e.g., \file{NEWS.md}). Each package
#' version appears in a first-level header, each category (e.g., \samp{NEW
#' FEATURES} or \samp{BUG FIXES}) is in a second-level header, and the news
#' items are written into bullet lists.
#' @param package,... Arguments to be passed to [news()].
#' @param output The output file path.
#' @param category Whether to keep the category names.
#' @return If `output = NA`, returns the Markdown content as a character
#'   vector, otherwise the content is written to the output file.
#' @export
#' @examplesIf interactive()
#' # news for the current version of R
#' xfun::news2md('R', Version == getRversion(), output = NA)
news2md = function(package, ..., output = 'NEWS.md', category = TRUE) {
  db = news(package = package, ...)
  k = db[, 'Category']
  db[is.na(k), 'Category'] = ''  # replace NA category with ''
  res = unlist(lapply(unique(db[, 'Version']), function(v) {
    d1 = db[db[, 'Version'] == v, ]
    res = unlist(lapply(unique(d1[, 'Category']), function(k) {
      txt = d1[d1[, 'Category'] == k, 'Text']
      txt = txt[txt != '']
      if (k == '' && length(txt) == 0) return()
      txt = gsub('\n *', ' ', txt)
      c(if (category && k != '') paste('##', k), if (length(txt)) paste('-', txt))
    }))
    if (is.na(dt <- d1[1, 'Date'])) dt = '' else dt = paste0(' (', dt, ')')
    c(sprintf('# CHANGES IN %s VERSION %s%s', package, v, dt), res)
  }))
  res = c(rbind(res, ''))  # add a blank line after each line
  if (is.na(output)) raw_string(res) else write_utf8(res, output)
}

#' Get base R package names
#'
#' Return base R package names.
#' @return A character vector of base R package names.
#' @export
#' @examples
#' xfun::base_pkgs()
base_pkgs = function() {
  if (is.function(f <- asNamespace('tools')$standard_package_names)) f()[['base']] else c(
    'base', 'compiler', 'datasets', 'graphics', 'grDevices', 'grid', 'methods',
    'parallel', 'splines', 'stats', 'stats4', 'tcltk', 'tools', 'utils'
  )
}

# update one package (from source by default)
pkg_update_one = function(pkg, type = 'source') {
  opts = options(repos = c(CRAN = 'https://cran.r-project.org'))
  on.exit(options(opts), add = TRUE)
  if (is.null(pkgs <- old.packages(type = type)) || !pkg %in% rownames(pkgs)) return()
  install.packages(pkg, pkgs[pkg, 'LibPath'], type = type, INSTALL_opts = '--no-staged-install')
  NULL
}
