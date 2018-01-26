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
#' \code{requireNamespace(quietly = TRUE)}.
#'
#' These are convenience functions that aim to solve these common problems: (1)
#' We often need to attach or load multiple packages, and it is tedious to type
#' several \code{library()} calls; (2) We are likely to want to install the
#' packages when attaching/loading them but they have not been installed.
#' @param ... Package names (character vectors, and must always be quoted).
#' @param install Whether to automatically install packages that are not
#'   available using \code{\link{install.packages}()}. You are recommended to
#'   set a CRAN mirror in the global option \code{repos} via
#'   \code{\link{options}()} if you want to automatically install packages.
#' @return \code{pkg_attach()} returns \code{NULL} invisibly. \code{pkg_load()}
#'   returns a logical vector, indicating whether the packages can be loaded.
#' @import utils
#' @export
#' @examples library(xfun)
#' pkg_attach('stats', 'graphics')
#' # pkg_attach2('servr')  # automatically install servr if it is not installed
#'
#' (pkg_load('stats', 'graphics'))
pkg_attach = function(..., install = FALSE) {
  for (i in c(...)) {
    if (install && !loadable(i)) install.packages(i)
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
    if (install && !res[i]) {
      install.packages(p); res[i] = loadable(p)
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
  if (new_session) {
    Rscript(c('-e', shQuote(sprintf('library("%s")', pkg))), stdout = FALSE, stderr = FALSE) == 0
  } else {
    if (strict) requireNamespace(pkg, quietly = TRUE) else pkg %in% .packages(TRUE)
  }
}

#' @rdname pkg_attach
#' @export
pkg_attach2 = function(...) pkg_attach(..., install = TRUE)

#' @rdname pkg_attach
#' @export
pkg_load2 = function(...) pkg_load(..., install = TRUE)


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
  if (res != 0) stop('Failed to install the package ', pkg)
  invisible(res)
}

#' Run \command{R CMD check} on the reverse dependencies of a package
#'
#' Install the source package, figure out the reverse dependencies on CRAN,
#' download all of their source packages, and run \command{R CMD check} on them
#' in parallel.
#'
#' Everything occurs under the current working directory, and you are
#' recommended to call this function under a separate directory, especially when
#' the number of reverse dependencies is large, because all source packages will
#' be downloaded to this directory, and all \file{*.Rcheck} directories will be
#' generated under this directory, too.
#'
#' If a source tarball of the expected version exists in the current directory,
#' it will not be downloaded again (to save time and bandwidth).
#'
#' After a package has been checked, the associated \file{*.Rcheck} directory
#' will be deleted if the check was successful (no warnings or errors, and
#' optionally, no notes), which means if you see a \file{*.Rcheck} directory, it
#' means the check failed, and you need to take a look at the log files under
#' that directory.
#'
#' The time to finish the check is recorded for each package. As the check goes
#' on, the total remaing time will be roughly estimated via \code{n *
#' mean(times)}, where \code{n} is the number of packages remaining to be
#' checked, and \code{times} is a vector of elapsed time of packages that have
#' been checked.
#'
#' A recommended workflow is to use a special directory to run
#' \code{rev_check()}, set the global \code{\link{options}}
#' \code{xfun.rev_check.src_dir} and \code{repos} in the R startup (see
#' \code{?\link{Startup}}) profile file \code{.Rprofile} under this directory,
#' and (optionally) set \code{R_LIBS_USER} in \file{.Renviron} to use a special
#' library path (so that your usual library will not be cluttered). Then run
#' \code{xfun::rev_check(pkg)} once, investigate and fix the problems or (if you
#' believe it was not your fault) ignore broken packages in the file
#' \file{00ignore}, and run \code{xfun::rev_check(pkg)} again to recheck the
#' failed packages. Repeat this process until all \file{*.Rcheck} directories
#' are gone.
#'
#' As an example, I set \code{options(repos = c(CRAN =
#' 'https://cran.rstudio.com'), xfun.rev_check.src_dir = '~/Dropbox/repo')} in
#' \file{.Rprofile}, and \code{R_LIBS_USER=~/R-tmp} in \file{.Renviron}. Then I
#' can run, for example, \code{xfun::rev_check('knitr')} repeatedly under a
#' special directory \file{~/Downloads/revcheck}. Reverse dependencies and their
#' dependencies will be installed to \file{~/R-tmp}, and \pkg{knitr} will be
#' installed from \file{~/Dropbox/repo/kintr}.
#' @param pkg The package name.
#' @param recheck Whether to only check the failed packages from last time. By
#'   default, if there are any \file{*.Rcheck} directories, \code{recheck} will
#'   be automatically set to \code{TRUE} if missing.
#' @param ignore A vector of package names to be ignored in \command{R CMD
#'   check}. If this argument is missing and a file \file{00ignore} exists, the
#'   file will be read as a character vector and passed to this argument.
#' @param note Whether to treat \verb{NOTE} in the check log as failure.
#'   \verb{WARNING} and \verb{ERROR} are always treated as failure.
#' @param update Whether to update all packages before the check.
#' @param src The path of the source package directory.
#' @param src_dir The parent directory of the source package directory. This can
#'   be set in a global option if all your source packages are under a common
#'   parent directory.
#' @seealso \code{devtools::revdep_check()} is more sophisticated, but currently
#'   has a few major issues that affect me: (1) It always deletes the
#'   \file{*.Rcheck} directories
#'   (\url{https://github.com/hadley/devtools/issues/1395}), which makes it
#'   difficult to know more information about the failures; (2) It does not
#'   fully install the source package before checking its reverse dependencies
#'   (\url{https://github.com/hadley/devtools/pull/1397}); (3) I feel it is
#'   fairly difficult to iterate the check (ignore the successful packages and
#'   only check the failed packages); by comparison, \code{xfun::rev_check()}
#'   only requires you to run a short command repeatedly (failed packages are
#'   indicated by the existing \file{*.Rcheck} directories, and automatically
#'   checked again the next time).
#'
#'   \code{xfun::rev_check()} borrowed a very nice feature from
#'   \code{devtools::revdep_check()}: estimating and displaying the remaining
#'   time. This is particularly useful for packages with huge numbers of reverse
#'   dependencies.
#' @export
rev_check = function(
  pkg, recheck = FALSE, ignore = NULL, note = TRUE, update = TRUE,
  src = file.path(src_dir, pkg), src_dir = getOption('xfun.rev_check.src_dir')
) {
  if (length(src) != 1 || !dir.exists(src)) stop(
    'The package source dir (the "src" argument) must be an existing directory'
  )

  # rJava breaks occasionally (after I update Java or R)
  message('Running R CMD javareconf...')
  Rcmd('javareconf', stdout = FALSE)

  if (update) {
    message('Updating all R packages...')
    update.packages(ask = FALSE, checkBuilt = TRUE)
  }

  message('Installing the source package ', src)
  install_dir(path.expand(src))

  db = available.packages(type = 'source')

  dirs = list.files('.', '.+[.]Rcheck$')
  if (missing(recheck)) recheck = length(dirs) > 0
  pkgs = if (recheck) {
    gsub('.Rcheck$', '', dirs)
  } else {
    res = check_deps(pkg, db)
    message('Installing dependencies of reverse dependencies')
    print(system.time(
      plapply(res$install, function(p) if (!loadable(p)) {
        install = getOption('xfun.install.packages', install.packages)
        install(p)
      })
    ))
    res$check
  }

  f = tempfile('check-timing', fileext = '.rds')
  l = tempfile('check-lock'); on.exit(unlink(c(f, l)), add = TRUE)
  n = length(pkgs)
  if (n == 0) {
    message('No reverse dependencies to be check for the package ', pkg); return()
  }

  if (missing(ignore) && file.exists('00ignore')) ignore = scan('00ignore', 'character')
  if (length(ignore)) {
    message('Ignoring packages: ', paste(ignore, collapse = ' '))
    unlink(sprintf('%s.Rcheck', ignore), recursive = TRUE)
    pkgs = setdiff(pkgs, ignore)
  }

  message('Checking ', n, ' packages: ', paste(pkgs, collapse = ' '))

  err = sprintf('(%s)$', paste(c('WARNING', 'ERROR', if (note) 'NOTE'), collapse = '|'))
  plapply(pkgs, function(p) {
    message('Checking ', p)
    t0 = Sys.time()

    timing = function(na = FALSE) {
      # in case two packages finish at exactly the same time
      while (file.exists(l)) Sys.sleep(.1)
      file.create(l); on.exit(unlink(l), add = TRUE)
      info = if (file.exists(f)) readRDS(f) else numeric()
      t1 = Sys.time()
      info[p] = if (na) NA_real_ else as.numeric(t1 - t0)
      saveRDS(info, f)
      n2 = length(setdiff(pkgs, names(info)))  # remaining packages
      t2 = Sys.time() + n2 * mean(info, na.rm = TRUE)
      message(
        'Packages remaining: ', n2, '/', n, '; Expect to finish at ', t2,
        ' (', format(round(difftime(t2, t1))), ')'
      )
    }

    z = sprintf('%s_%s.tar.gz', p, db[p, 'Version'])
    d = sprintf('%s.Rcheck', p)
    # remove other versions of the package tarball
    unlink(setdiff(list.files('.', sprintf('^%s_.+.tar.gz', p)), z))
    if (!file.exists(z)) try(download.file(
      paste(db[p, 'Repository'], z, sep = '/'), z, mode = 'wb'
    ))
    if (!file.exists(z)) {
      timing(TRUE)
      return(dir.create(d, showWarnings = FALSE))
    }

    Rcmd(c('check', '--no-manual', shQuote(z)), stdout = FALSE)
    out = readLines(file.path(d, '00check.log'))
    if (length(grep(err, out)) == 0) unlink(d, recursive = TRUE)
    timing()
  })
  invisible()
}

check_deps = function(x, db = available.packages()) {
  dep = function(x, ...) {
    if (length(x)) unique(unlist(tools::package_dependencies(x, ...)))
  }
  # packages that reverse depend on me
  x1 = dep(x, db, 'all', reverse = TRUE)
  # to R CMD check x1, I have to install all their dependencies
  x2 = dep(x1, db, 'all')
  # and for those dependencies, I have to install the default dependencies
  x3 = dep(x2, db, recursive = TRUE)
  list(check = x1, install = unique(c(x1, x2, x3)))
}

# mclapply() with a different default for mc.cores
plapply = function(X, FUN, ...) {
  parallel::mclapply(X, FUN, ..., mc.cores = getOption('mc.cores', parallel::detectCores()))
}
