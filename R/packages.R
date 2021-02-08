#' Attach or load packages, and automatically install missing packages if
#' requested
#'
#' \code{pkg_attach()} is a vectorized version of \code{\link{library}()} over
#' the \code{package} argument to attach multiple packages in a single function
#' call. \code{pkg_load()} is a vectorized version of
#' \code{\link{requireNamespace}()} to load packages (without attaching them).
#' The functions \code{pkg_attach2()} and \code{pkg_load2()} are wrappers of
#' \code{pkg_attach(install = TRUE)} and \code{pkg_load(install = TRUE)},
#' respectively. \code{loadable()} is an abbreviation of
#' \code{requireNamespace(quietly = TRUE)}. \code{pkg_available()} tests if a
#' package with a minimal version is available.
#'
#' These are convenience functions that aim to solve these common problems: (1)
#' We often need to attach or load multiple packages, and it is tedious to type
#' several \code{library()} calls; (2) We are likely to want to install the
#' packages when attaching/loading them but they have not been installed.
#' @param ... Package names (character vectors, and must always be quoted).
#' @param install Whether to automatically install packages that are not
#'   available using \code{\link{install.packages}()}. Besides \code{TRUE} and
#'   \code{FALSE}, the value of this argument can also be a function to install
#'   packages (\code{install = TRUE} is equivalent to \code{install =
#'   install.packages}), or a character string \code{"pak"} (equivalent to
#'   \code{install = pak::pkg_install}, which requires the \pkg{pak} package).
#'   You are recommended to set a CRAN mirror in the global option \code{repos}
#'   via \code{\link{options}()} if you want to automatically install packages.
#' @param message Whether to show the package startup messages (if any startup
#'   messages are provided in a package).
#' @return \code{pkg_attach()} returns \code{NULL} invisibly. \code{pkg_load()}
#'   returns a logical vector, indicating whether the packages can be loaded.
#' @seealso \code{pkg_attach2()} is similar to \code{pacman::p_load()}, but does
#'   not allow non-standard evaluation (NSE) of the \code{...} argument, i.e.,
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
    if (!isFALSE(install) && !loadable(i)) pkg_install(i, install)
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
    if (!isFALSE(install) && !res[i]) {
      pkg_install(p, install); res[i] = loadable(p)
    }
  }
  if (error && any(!res)) stop('Package(s) not loadable: ', paste(pkg[!res], collapse = ' '))
  invisible(res)
}

#' @param pkg A single package name.
#' @param strict If \code{TRUE}, use \code{\link{requireNamespace}()} to test if
#'   a package is loadable; otherwise only check if the package is in
#'   \code{\link{.packages}(TRUE)} (this does not really load the package, so it
#'   is less rigorous but on the other hand, it can keep the current R session
#'   clean).
#' @param new_session Whether to test if a package is loadable in a new R
#'   session. Note that \code{new_session = TRUE} implies \code{strict = TRUE}.
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

#' @param version A minimal version number. If \code{NULL}, only test if a
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

pkg_update = function(...) {
  update.packages(ask = FALSE, checkBuilt = TRUE, ...)
}

broken_packages = function(reinstall = TRUE) {
  pkgs = unlist(plapply(.packages(TRUE), function(p) if (!loadable(p)) p))
  if (reinstall) {
    remove.packages(pkgs); pkg_install(pkgs)
  } else pkgs
}

#' Install a source package from a directory
#'
#' Run \command{R CMD build} to build a tarball from a source directory, and run
#' \command{R CMD INSTALL} to install it.
#' @param src The package source directory.
#' @param build Whether to build a tarball from the source directory. If
#'   \code{FALSE}, run \command{R CMD INSTALL} on the directory directly (note
#'   that vignettes will not be automatically built).
#' @param build_opts The options for \command{R CMD build}.
#' @param install_opts The options for \command{R CMD INSTALL}.
#' @export
#' @return Invisible status from \command{R CMD INSTALL}.
install_dir = function(src, build = TRUE, build_opts = NULL, install_opts = NULL) {
  desc = file.path(src, 'DESCRIPTION')
  pv = read.dcf(desc, fields = c('Package', 'Version'))
  # delete existing tarballs
  unlink(sprintf('%s_*.tar.gz', pv[1, 1]))
  pkg = if (build) {
    Rcmd(c('build', build_opts, shQuote(src)))
    sprintf('%s_%s.tar.gz', pv[1, 1], pv[1, 2])
  } else src
  res = Rcmd(c('INSTALL', install_opts, pkg))
  if (build) unlink(pkg)
  if (res != 0) stop('Failed to install the package ', pkg)
  invisible(res)
}

install_brew_deps = function(pkg = .packages(TRUE)) {
  con = url('https://macos.rbind.io/bin/macosx/sysreqsdb.rds')
  on.exit(close(con), add = TRUE)
  inst = installed.packages()
  pkg = intersect(pkg, pkg_needs_compilation(inst))
  deps = readRDS(con)
  deps = deps[c(pkg, pkg_dep(pkg, inst, recursive = TRUE))]
  deps = paste(na.omit(unique(unlist(deps))), collapse = ' ')
  if (deps != '') system(paste('brew install', deps))
}

pkg_needs_compilation = function(db = installed.packages()) {
  pkgs = unname(db[tolower(db[, 'NeedsCompilation']) == 'yes', 'Package'])
  pkgs[!is.na(pkgs)]
}

#' An alias of \code{remotes::install_github()}
#'
#' This alias is to make autocomplete faster via \code{xfun::install_github},
#' because most \code{remotes::install_*} functions are never what I want. I
#' only use \code{install_github} and it is inconvenient to autocomplete it,
#' e.g. \code{install_git} always comes before \code{install_github}, but I
#' never use it. In RStudio, I only need to type \code{xfun::ig} to get
#' \code{xfun::install_github}.
#' @param ... Arguments to be passed to
#'   \code{remotes::\link[remotes]{install_github}()}.
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

base_pkgs = function() rownames(installed.packages(priority = 'base'))

# update one package (from source by default)
pkg_update_one = function(pkg, type = 'source') {
  opts = options(repos = c(CRAN = 'https://cran.r-project.org'))
  on.exit(options(opts), add = TRUE)
  if (is.null(pkgs <- old.packages(type = type)) || !pkg %in% rownames(pkgs)) return()
  install.packages(pkg, pkgs[pkg, 'LibPath'], type = type, INSTALL_opts = '--no-staged-install')
  NULL
}
