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
#' @param which Which types of reverse dependencies to check. See
#'   \code{tools::\link[tools]{package_dependencies}()} for possible values. The
#'   special value \code{'hard'} means the hard dependencies, i.e.,
#'   \code{c('Depends', 'Imports', 'LinkingTo')}.
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
  pkg, which = 'all', recheck = FALSE, ignore = NULL, note = TRUE, update = TRUE,
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
    res = check_deps(pkg, db, which)
    message('Installing dependencies of reverse dependencies')
    res$install = setdiff(res$install, ignore_deps())
    print(system.time({
      pkg_install(unlist(plapply(res$install, function(p) if (!loadable(p)) p)))
    }))
    res$check
  }

  f = tempfile('check-done', fileext = '.rds')
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

  t0 = Sys.time()
  message('Checking ', n, ' packages: ', paste(pkgs, collapse = ' '))

  res = plapply(pkgs, function(p) {
    d = sprintf('%s.Rcheck', p)
    if (!p %in% rownames(db)) {
      message('Checking ', p, ' (aborted since it is no longer on CRAN')
      unlink(d, recursive = TRUE)
      return()
    }
    message('Checking ', p)

    timing = function() {
      # in case two packages finish at exactly the same time
      while (file.exists(l)) Sys.sleep(.1)
      file.create(l); on.exit(unlink(l), add = TRUE)
      done = c(if (file.exists(f)) readRDS(f), p)
      saveRDS(done, f)
      n2 = length(setdiff(pkgs, done))  # remaining packages
      t1 = Sys.time(); t2 = Sys.time() + n2 * (t1 - t0) / (n - n2)
      message(
        'Packages remaining: ', n2, '/', n, '; Expect to finish at ', t2,
        ' (', format(round(difftime(t2, t1))), ')'
      )
    }

    z = sprintf('%s_%s.tar.gz', p, db[p, 'Version'])
    # remove other versions of the package tarball
    unlink(setdiff(list.files('.', sprintf('^%s_.+.tar.gz', p)), z))
    if (!file.exists(z)) try(download.file(
      paste(db[p, 'Repository'], z, sep = '/'), z, mode = 'wb'
    ))
    if (!file.exists(z)) {
      timing()
      return(dir.create(d, showWarnings = FALSE))
    }

    Rcmd(c('check', '--no-manual', shQuote(z)), stdout = FALSE)
    if (!clean_Rcheck(d, note = note)) {
      if (!dir.exists(d)) {dir.create(d); return(timing())}
      in_dir(d, {
        clean_log()
        # so that I can easily preview it
        file.exists('00install.out') && file.rename('00install.out', '00install.log')
        cran_check_page(p)
      })
    }
    timing()
    NULL
  })
  if (note) fix_missing_deps()
  res = Filter(function(x) !is.null(x), res)
  if (length(res)) res
}

# remove the OK lines in the check log
clean_log = function() {
  if (!file.exists(l <- '00check.log')) return()
  x = grep('^[*].+OK$', readLines(l), invert = TRUE, value = TRUE)
  writeLines(tail(x, -2), l)  # remove the first 2 lines (log dir name and R version)
}

error_pattern = function(note = TRUE) {
  sprintf('(%s)$', paste(c('WARNING', 'ERROR', if (note) 'NOTE'), collapse = '|'))
}

pkg_dep = function(x, ...) {
  if (length(x)) unique(unlist(tools::package_dependencies(x, ...)))
}

check_deps = function(x, db = available.packages(), which = 'all') {
  if (identical(which, 'hard')) which = c('Depends', 'Imports', 'LinkingTo')
  # packages that reverse depend on me
  x1 = pkg_dep(x, db, which, reverse = TRUE)
  # to R CMD check x1, I have to install all their dependencies
  x2 = pkg_dep(x1, db, 'all')
  # and for those dependencies, I have to install the default dependencies
  x3 = pkg_dep(x2, db, recursive = TRUE)
  list(check = x1, install = unique(c(x1, x2, x3)))
}

# mclapply() with a different default for mc.cores and disable prescheduling
plapply = function(X, FUN, ...) {
  parallel::mclapply(
    X, FUN, ..., mc.cores = getOption('mc.cores', parallel::detectCores()),
    mc.preschedule = FALSE
  )
}

pkg_install = function(pkgs) {
  if (length(pkgs) == 0) return()
  install = getOption('xfun.install.packages', install.packages)
  if (length(pkgs) > 1)
    message('Installing ', length(pkgs), ' packages: ', paste(pkgs, collapse = ' '))
  install(pkgs)
}

clean_Rcheck = function(dir, log = readLines(file.path(dir, '00check.log')), note = TRUE) {
  if (length(grep(error_pattern(note), log)) == 0) unlink(dir, recursive = TRUE)
  !dir.exists(dir)
}

ignore_deps = function() {
  if (file.exists('00ignore_deps')) scan('00ignore_deps', 'character')
}

fix_missing_deps = function(ignore = NULL) {
  if (is.null(ignore)) ignore = ignore_deps()
  dirs = list.files('.', '[.]Rcheck$')
  r = '^Packages? (suggested|required|which this enhances) but not available for checking:'
  pkgs = character()
  for (d in dirs) {
    if (!file.exists(l <- file.path(d, '00check.log'))) next
    x = readLines(l)
    if (length(i <- grep(r, x)) == 0) next
    if (unnecessary_suggests(d, x, i)) next
    ps = trimws(gsub(r, '', x[i])); k = i
    if (ps == '') for (k in (i + 1):length(x)) {
      if (grepl('^\\s', x[k])) ps = paste(ps, x[k]) else break
    }
    ps = gsub('[^a-zA-Z0-9.]', ' ', ps)
    ps = setdiff(unlist(strsplit(ps, '\\s+')), ignore)
    for (p in ps[ps != '']) {
      if (!loadable(p, new_session = TRUE)) pkg_install(p)
      # failed to install
      if (!loadable(p, new_session = TRUE)) pkgs = c(pkgs, p)
    }
  }
  if (length(pkgs)) {
    message('Failed to install packages', paste(pkgs, collapse = ' '))
  }
}

# check if the missing Suggests are really necessary; if the check log is okay
# without these Suggests (i.e. Suggests is the only problem), delete the check
# dir to mark R CMD check as successful
unnecessary_suggests = function(d, x, i) {
  if (length(i) != 1) {
    warning(
      'Expecting only one NOTE (ERROR) about missing dependencies but got multiple:\n',
      paste(x[i], collapse = '\n')
    )
    return(FALSE)
  }
  i2 = grep('^Status:', x)
  # R CMD check failed or not complete yet
  if (length(grep('(WARNING|ERROR)$', x)) || length(i2) == 0) return(FALSE)
  x2 = x[-c(i, i - 1, i2)]
  clean_Rcheck(d, x2)
  !dir.exists(d)
}

cran_check_page = function(pkg, con = '00check-cran.log') {
  u = sprintf('https://cran.rstudio.com/web/checks/check_results_%s.html', pkg)
  x = readLines(u)
  if (length(i <- grep('Check Details', x, ignore.case = TRUE)) == 0) return()
  x = x[i[1]:length(x)]
  x = gsub('<[^>]+>', '', x)
  x = gsub('&nbsp;', ' ', x)
  x = gsub('&gt;', '>', x)
  x = gsub('&lt;', '<', x)
  x = gsub('\\s+', ' ', x)
  x = paste(trimws(x), collapse = '\n')
  x = gsub('\n\n+', '\n\n', x)
  writeLines(x, con)
}

cran_check_pages = function() {
  dirs = list.files('.', '[.]Rcheck$')
  for (d in dirs) {
    if (dir.exists(d)) in_dir(d, cran_check_page(gsub('[.]Rcheck$', '', d)))
  }
}

error_txt = '00cran-errors.txt'

save_cran_summary = function() {
  u = 'https://cran.rstudio.com/web/checks/check_summary_by_package.html'
  h = readLines(u, n = 30)
  # do not download the full page if the update time has not changed
  if (length(i <- grep('Last updated on ', h)) == 1 && file.exists(error_txt)) {
    if (identical(readLines(error_txt, n = 1), h[i])) return()
  }
  x = readLines(u)
  x = gsub('<[^>]+>', '', x)
  x = gsub('&[^;]+;', '', x)
  x = gsub('\\s+', ' ', x)
  x = grep('^[a-zA-Z0-9.]+ [0-9.-]+', trimws(x), value = TRUE)
  writeLines(c(if (length(i) == 1) h[i], x), error_txt)
}

cran_check_summary = function() {
  if (!file.exists(error_txt)) return()
  x = readLines(error_txt)
  if (length(x) > 0) x = x[-1] else return()
  r = '^([a-zA-Z0-9.]+)(.+)'
  p = gsub(r, '\\1', x)  # package names
  s = gsub(r, '\\2', x)  # summary
  f = function(type) p[grep(type, s)]
  list(error = f('ERROR'), warning = f('WARNING'), note = f('NOTE'))
}

# kill a R CMD check process if it has been running for more then 60 minutes
kill_long_processes = function(etime = 60 * 60) {
  while (TRUE) {
    x = system('ps -ax -o pid,etime,command | grep "Rcmd check --no-manual"', intern = TRUE)
    x = grep('_[0-9.-]+[.]tar[.]gz$', trimws(x), value = TRUE)
    pids = unlist(lapply(strsplit(x, '\\s+'), function(z) {
      pid = z[1]; time = as.numeric(unlist(strsplit(z[2], '-|:')))
      time = sum(tail(c(rep(0, 4), time), 4) * c(24 * 3600, 3600, 60, 1))
      if (time > etime) pid
    }))
    if (length(pids)) {
      message('Killing processes: ', paste(pids, collapse = ' '))
      system2('kill', pids)
    }
    Sys.sleep(300)
  }
}

install_missing_latex = function() {
  dirs = list.files('.', '[.]Rcheck$')
  pkgs = NULL
  for (d in dirs) {
    if (dir.exists(d)) pkgs = c(pkgs, in_dir(
      d, tinytex::parse_packages('00check.log', quiet = c(TRUE, FALSE, FALSE))
    ))
  }
  tinytex::tlmgr_install(unique(pkgs))
}
