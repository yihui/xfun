#' Run \command{R CMD check} on the reverse dependencies of a package
#'
#' Install the source package, figure out the reverse dependencies on CRAN,
#' download all of their source packages, and run \command{R CMD check} on them
#' in parallel.
#'
#' Everything occurs under the current working directory, and you are
#' recommended to call this function under a designated directory, especially
#' when the number of reverse dependencies is large, because all source packages
#' will be downloaded to this directory, and all \file{*.Rcheck} directories
#' will be generated under this directory, too.
#'
#' If a source tarball of the expected version has been downloaded before (under
#' the \file{tarball} directory), it will not be downloaded again (to save time
#' and bandwidth).
#'
#' After a package has been checked, the associated \file{*.Rcheck} directory
#' will be deleted if the check was successful (no warnings or errors or notes),
#' which means if you see a \file{*.Rcheck} directory, it means the check
#' failed, and you need to take a look at the log files under that directory.
#'
#' The time to finish the check is recorded for each package. As the check goes
#' on, the total remaining time will be roughly estimated via `n *
#' mean(times)`, where `n` is the number of packages remaining to be
#' checked, and `times` is a vector of elapsed time of packages that have
#' been checked.
#'
#' If a check on a reverse dependency failed, its \file{*.Rcheck} directory will
#' be renamed to \file{*.Rcheck2}, and another check will be run against the
#' CRAN version of the package unless `options(xfun.rev_check.compare =
#' FALSE)` is set. If the logs of the two checks are the same, it means no new
#' problems were introduced in the package, and you can probably ignore this
#' particular reverse dependency. The function `compare_Rcheck()` can be
#' used to create a summary of all the differences in the check logs under
#' \file{*.Rcheck} and \file{*.Rcheck2}. This will be done automatically if
#' `options(xfun.rev_check.summary = TRUE)` has been set.
#'
#' A recommended workflow is to use a special directory to run
#' `rev_check()`, set the global [options()]
#' `xfun.rev_check.src_dir` and `repos` in the R startup (see
#' `?`[`Startup`]) profile file `.Rprofile` under this directory,
#' and (optionally) set `R_LIBS_USER` in \file{.Renviron} to use a special
#' library path (so that your usual library will not be cluttered). Then run
#' `xfun::rev_check(pkg)` once, investigate and fix the problems or (if you
#' believe it was not your fault) ignore broken packages in the file
#' \file{00ignore}, and run `xfun::rev_check(pkg)` again to recheck the
#' failed packages. Repeat this process until all \file{*.Rcheck} directories
#' are gone.
#'
#' As an example, I set `options(repos = c(CRAN =
#' 'https://cran.rstudio.com'), xfun.rev_check.src_dir = '~/Dropbox/repo')` in
#' \file{.Rprofile}, and `R_LIBS_USER=~/R-tmp` in \file{.Renviron}. Then I
#' can run, for example, `xfun::rev_check('knitr')` repeatedly under a
#' special directory \file{~/Downloads/revcheck}. Reverse dependencies and their
#' dependencies will be installed to \file{~/R-tmp}, and \pkg{knitr} will be
#' installed from \file{~/Dropbox/repo/kintr}.
#' @param pkg The package name.
#' @param which Which types of reverse dependencies to check. See
#'   [tools::package_dependencies()] for possible values. The
#'   special value `'hard'` means the hard dependencies, i.e.,
#'   `c('Depends', 'Imports', 'LinkingTo')`.
#' @param recheck A vector of package names to be (re)checked. If not provided
#'   and there are any \file{*.Rcheck} directories left by certain packages
#'   (this often means these packages failed the last time), `recheck` will
#'   be these packages; if there are no \file{*.Rcheck} directories but a text
#'   file \file{recheck} exists, `recheck` will be the character vector
#'   read from this file. This provides a way for you to manually specify the
#'   packages to be checked. If there are no packages to be rechecked, all
#'   reverse dependencies will be checked.
#' @param ignore A vector of package names to be ignored in \command{R CMD
#'   check}. If this argument is missing and a file \file{00ignore} exists, the
#'   file will be read as a character vector and passed to this argument.
#' @param update Whether to update all packages before the check.
#' @param src The path of the source package directory.
#' @param src_dir The parent directory of the source package directory. This can
#'   be set in a global option if all your source packages are under a common
#'   parent directory.
#' @param timeout Timeout in seconds for \command{R CMD check} to check each
#'   package. The (approximate) total time can be limited by the global option
#'   `xfun.rev_check.timeout_total`.
#' @return A named numeric vector with the names being package names of reverse
#'   dependencies; `0` indicates check success, `1` indicates failure,
#'   and `2` indicates that a package was not checked due to global
#'   timeout.
#' @seealso `devtools::revdep_check()` is more sophisticated, but currently
#'   has a few major issues that affect me: (1) It always deletes the
#'   \file{*.Rcheck} directories
#'   (<https://github.com/r-lib/devtools/issues/1395>), which makes it
#'   difficult to know more information about the failures; (2) It does not
#'   fully install the source package before checking its reverse dependencies
#'   (<https://github.com/r-lib/devtools/pull/1397>); (3) I feel it is
#'   fairly difficult to iterate the check (ignore the successful packages and
#'   only check the failed packages); by comparison, `xfun::rev_check()`
#'   only requires you to run a short command repeatedly (failed packages are
#'   indicated by the existing \file{*.Rcheck} directories, and automatically
#'   checked again the next time).
#'
#'   `xfun::rev_check()` borrowed a very nice feature from
#'   `devtools::revdep_check()`: estimating and displaying the remaining
#'   time. This is particularly useful for packages with huge numbers of reverse
#'   dependencies.
#' @export
rev_check = function(
  pkg, which = 'all', recheck = NULL, ignore = NULL, update = TRUE,
  timeout = getOption('xfun.rev_check.timeout', 15 * 60),
  src = file.path(src_dir, pkg), src_dir = getOption('xfun.rev_check.src_dir')
) {
  if (length(src) != 1 || !dir_exists(src)) stop(
    'The package source dir (the "src" argument) must be an existing directory'
  )

  message('Installing the source package ', src)
  install_dir(path.expand(src))

  db = available.packages(type = 'source')

  # install packages that are not loadable (testing in parallel)
  p_install = function(pkgs) {
    pkgs_up = NULL
    if (update) {
      message('Updating all R packages...')
      pkgs_up = intersect(old.packages(checkBuilt = TRUE)[, 'Package'], pkgs)
      pkg_install(pkgs_up)
    }
    pkgs = setdiff(pkgs, pkgs_up)  # don't install pkgs that were just updated
    print(system.time(
      pkg_install(unlist(plapply(pkgs, function(p) if (!loadable(p, new_session = TRUE)) p)))
    ))
  }

  unlink('*.Rcheck2', recursive = TRUE)
  if (missing(recheck)) {
    dirs = list.files('.', '.+[.]Rcheck$')
    pkgs = gsub('.Rcheck$', '', dirs)
    recheck = if (length(pkgs) == 0 && file_exists('recheck')) {
      scan('recheck', 'character')
    } else pkgs
  }
  pkgs = if (length(recheck)) {
    p_install(pkg_dep(recheck, db, which = 'all'))
    recheck
  } else {
    res = check_deps(pkg, db, which)
    message('Installing dependencies of reverse dependencies')
    res$install = setdiff(res$install, ignore_deps())
    print(system.time(p_install(res$install)))
    res$check
  }
  pkgs = intersect(pkgs, rownames(db))  # make sure the pkgs are on CRAN

  lib_cran = './library-cran'
  on.exit(unlink(lib_cran, recursive = TRUE), add = TRUE)
  dir.create(lib_cran, showWarnings = FALSE)
  pkg_install(pkg, lib = lib_cran)  # the CRAN version of the package

  f = tempfile('check-done', fileext = '.rds')
  l = tempfile('check-lock'); on.exit(unlink(c(f, l)), add = TRUE)
  n = length(pkgs)
  if (n == 0) {
    message('No reverse dependencies to be checked for the package ', pkg); return()
  }

  if (missing(ignore) && file_exists('00ignore')) ignore = scan('00ignore', 'character')
  if (length(ignore)) {
    message('Ignoring packages: ', paste(ignore, collapse = ' '))
    unlink(sprintf('%s.Rcheck', ignore), recursive = TRUE)
    pkgs = setdiff(pkgs, ignore)
    if ((n <- length(pkgs)) == 0) {
      message('No packages left to be checked'); return()
    }
  }

  message('Downloading tarballs')
  tars = download_tarball(pkgs, db, dir = 'tarball')
  tars = setNames(tars, pkgs)

  t0 = Sys.time()
  tt = getOption('xfun.rev_check.timeout_total', Inf)
  message('Checking ', n, ' packages: ', paste(pkgs, collapse = ' '))

  res = plapply(pkgs, function(p) {
    d = sprintf('%s.Rcheck', p)
    if (!p %in% rownames(db)) {
      message('Checking ', p, ' (aborted since it is no longer on CRAN')
      unlink(d, recursive = TRUE)
      return()
    }

    timing = function() {
      # in case two packages finish at exactly the same time
      while (file_exists(l)) Sys.sleep(.1)
      file.create(l); on.exit(unlink(l), add = TRUE)
      done = c(if (file_exists(f)) readRDS(f), p)
      saveRDS(done, f)
      n2 = length(setdiff(pkgs, done))  # remaining packages
      t1 = Sys.time(); t2 = Sys.time() + n2 * (t1 - t0) / (n - n2)
      message(
        'Packages remaining: ', n2, '/', n, '; Expect to finish at ', t2,
        ' (', format(round(difftime(t2, t1))), ')'
      )
      # 0 (FALSE): success; 1: failure
      setNames(as.integer(dir_exists(d)), p)
    }

    if (!file_exists(z <- tars[p])) {
      dir.create(d, showWarnings = FALSE)
      return(timing())
    }

    # timeout; package not checked
    if (difftime(Sys.time(), t0, units = 'secs') > tt) {
      return(setNames(2L, p))
    }

    check_it = function(args = NULL, ...) {
      system2(
        file.path(R.home('bin'), 'R'),
        c(args, 'CMD', 'check', '--no-manual', shQuote(z)),
        stdout = FALSE, stderr = FALSE, timeout = timeout, ...
      )
    }
    check_it()

    if (!clean_Rcheck(d)) {
      if (!dir_exists(d)) {dir.create(d); return(timing())}
      # try to install missing LaTeX packages for vignettes if possible, then recheck
      vigs = list.files(
        file.path(d, 'vign_test', p, 'vignettes'), '[.](Rnw|Rmd)$',
        ignore.case = TRUE, full.names = TRUE
      )
      pkg_load2('tinytex')
      if (length(vigs) && any(file_exists(with_ext(vigs, 'log')))) {
        if (tinytex::is_tinytex()) for (vig in vigs) in_dir(dirname(vig), {
          Rscript(shQuote(c('-e', 'if (grepl("[.]Rnw$", f <- commandArgs(T), ignore.case = T)) knitr::knit2pdf(f) else rmarkdown::render(f)', basename(vig))))
        })
        check_it()
        if (clean_Rcheck(d)) return(timing())
      }
      # if there are still missing LaTeX packages, install them and recheck
      l0 = tinytex::tl_pkgs()
      lapply(
        list.files(d, '[.]log$', full.names = TRUE, recursive = TRUE),
        tinytex::parse_install, quiet = TRUE
      )
      if (!identical(l0, tinytex::tl_pkgs())) {
        check_it()
        if (clean_Rcheck(d)) return(timing())
      }
      # clean up the check log, and recheck with the current CRAN version of pkg
      cleanup = function() in_dir(d, {
        clean_log()
        # so that I can easily preview it in the Finder on macOS
        file_exists('00install.out') && file.rename('00install.out', '00install.log')
      })
      # ignore vignettes that failed to build for unknown reasons
      cleanup()
      if (clean_Rcheck(d)) return(timing())
      # whether to check the package against the CRAN version?
      if (!getOption('xfun.rev_check.compare', TRUE)) return(timing())
      file.rename(d, d2 <- paste0(d, '2'))
      check_it('--no-environ', env = tweak_r_libs(lib_cran))
      if (!dir_exists(d)) file.rename(d2, d) else {
        cleanup()
        if (identical_logs(c(d, d2))) unlink(c(d, d2), recursive = TRUE)
      }
    }
    timing()
  })
  if (getOption('xfun.rev_check.summary', FALSE)) {
    html = compare_Rcheck(); if (isTRUE(grepl('[.]html$', html))) browseURL(html)
  }
  unlist(res)
}

# remove the OK lines in the check log
clean_log = function() {
  if (!file_exists(l <- '00check.log')) return()
  x = grep('^[*].+OK$', read_utf8(l), invert = TRUE, value = TRUE)
  # don't want diffs in random tempdir/tempfile paths when comparing check logs
  x[grep(dirname(tempdir()), x, fixed = TRUE)] = 'RANDOM TEMPDIR/TEMPFILE PATH DELETED'
  # delete the download progress
  x = grep('^\\s*\\[\\d+%] Downloaded \\d+ bytes...\\s*$', x, invert = TRUE, value = TRUE)
  # delete lines of the form "address 0x1067143eb, cause 'illegal opcode'"
  x = grep("address 0x[[:xdigit:]]+, cause '[^']+'", x, invert = TRUE, value = TRUE)
  x = recheck_vig(x)
  x = tail(x, -2)
  writeLines(x, l)  # remove the first 2 lines (log dir name and R version)
  x
}

# sometimes R CMD check fails to build vignettes for unknown reasons; try to
# recheck the package in this case
recheck_vig = function(x) {
  if (!any(i1 <- (x == '* checking re-building of vignette outputs ... WARNING')))
    return(x)

  i1 = which(i1)[1]
  i2 = which(x == 'Execution halted')
  i2 = i2[i2 > i1]
  if (length(i2) == 0) return(x)

  i3 = grep('^[*] checking ', x)  # next checking item
  i3 = i3[i3 > i1]
  if (length(i3)) {
    i2 = i2[i2 < i3[1]]  # 'Execution halted' needs to appear before next '* checking'
    if (length(i2) == 0) return(x)
  }

  # if no explicit errors were found in processing vignettes (except pandoc
  # error), remove the relevant log
  i2 = tail(i2, 1)
  if (length(grep('pandoc document conversion failed with error', x[i1:i2])) > 0 ||
      length(grep('Error: processing vignette .+ failed with diagnostics:', x[i1:i2])) == 0)
    x = x[-(i1:i2)]
  x
}

# are the check logs identical under a series of *.Rcheck directories?
identical_logs = function(dirs) {
  if (length(dirs) < 2) return(FALSE)
  if (!all(file_exists(logs <- file.path(dirs, '00check.log')))) return(FALSE)
  x = read_utf8(logs[1])
  for (i in 2:length(dirs)) if (!identical(x, read_utf8(logs[i]))) return(FALSE)
  TRUE
}

# delete files/dirs that are usually not helpful
clean_Rcheck2 = function(dir = '.') {
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  ds = list.files('.', '.+[.]Rcheck$')
  for (d in c(ds, paste0(ds, '2'))) {
    f1 = list.files(d, full.names = TRUE)
    f2 = file.path(d, c('00_pkg_src', '00check.log', '00install.log'), fsep = '/')
    unlink(setdiff(f1, f2), recursive = TRUE)
  }
}

# add a new library path to R_LIBS_USER
tweak_r_libs = function(new) {
  x = read_all(existing_files(c('~/.Renviron', '.Renviron')))
  x = grep('^\\s*#', x, invert = TRUE, value = TRUE)
  x = gsub('^\\s+|\\s+$', '', x)
  x = x[x != '']
  i = grep('^R_LIBS_USER=.+', x)
  if (length(i)) {
    x[i[1]] = sub('(="?)', path_sep('\\1', new), x[i[1]])
    x
  } else {
    v = Sys.getenv('R_LIBS_USER')
    v = if (v == '') new else path_sep(new, v)
    c(paste0('R_LIBS_USER=', v), x)
  }
}

# separate paths by the path separator on a specific platform
path_sep = function(...) paste(..., sep = .Platform$path.sep)

# a shorthand of tools::package_dependencies()
pkg_dep = function(x, ...) {
  if (length(x)) unique(unlist(tools::package_dependencies(x, ...)))
}

# calculate the packages required to check a package
check_deps = function(x, db = available.packages(), which = 'all') {
  if (identical(which, 'hard')) which = c('Depends', 'Imports', 'LinkingTo')
  x0 = db[, 'Package']  # all available packages
  # packages that reverse depend on me
  x1 = pkg_dep(x, db, which, reverse = TRUE)
  x1 = intersect(x1, x0)
  # only check a sample of soft reverse dependencies (useful if there are too many)
  if (identical(which, 'all') && (n <- getOption('xfun.rev_check.sample', 100)) >= 0) {
    x2 = pkg_dep(x, db, c('Suggests', 'Enhances'), reverse = TRUE)
    x2 = intersect(x2, x0)
    if (n < length(x2)) x1 = c(setdiff(x1, x2), sample(x2, n))
  }
  # to R CMD check x1, I have to install all their dependencies
  x2 = pkg_dep(x1, db, 'all')
  # and for those dependencies, I have to install the default dependencies
  x3 = pkg_dep(x2, db, recursive = TRUE)
  list(check = x1, install = intersect(c(x1, x2, x3), x0))
}

#' Submit check jobs to crandalf
#'
#' Check the reverse dependencies of a package using the crandalf service:
#' <https://github.com/yihui/crandalf>. If the number of reverse
#' dependencies is large, they will be split into batches and pushed to crandalf
#' one by one.
#'
#' Due to the time limit of a single job on Github Actions (6 hours), you will
#' have to split the large number of reverse dependencies into batches and check
#' them sequentially on Github (at most 5 jobs in parallel). The function
#' `crandalf_check()` does this automatically when necessary. It requires
#' the \command{git} command to be available.
#'
#' The function `crandalf_results()` fetches check results from Github
#' after all checks are completed, merge the results, and show a full summary of
#' check results. It requires `gh` (Github CLI:
#' <https://cli.github.com/manual/>) to be installed and you also need to
#' authenticate with your Github account beforehand.
#' @param pkg The package name of which the reverse dependencies are to be
#'   checked.
#' @param size The number of reverse dependencies to be checked in each job.
#' @param jobs The number of jobs to run in Github Actions (by default, all jobs
#'   are submitted, but you can choose to submit the first few jobs).
#' @param which The type of dependencies (see [rev_check()]).
#' @export
crandalf_check = function(pkg, size = 400, jobs = Inf, which = 'all') {
  git_test_branch()
  git_co('main')
  on.exit(git_co('main'), add = TRUE)
  git_test_branch()

  # do everything inside the check-pkg branch
  b = paste0('check-', pkg)
  if (git_co(b, stderr = FALSE) != 0) {
    git_co(c('-b', b))
    file.create('recheck')
    git(c('add', 'recheck'))
    git(c('commit', '-m', shQuote(paste('Revcheck', pkg))))
    git('push')
    message(
      'Please create a pull request from the branch ', b,
      ' on Github and re-run xfun::crandalf_check("', pkg, '").'
    )
    return(invisible())
  }
  git(c('merge', 'main'))

  x = check_deps(pkg, which = which)$check
  n = length(x)
  if (n <= size) {
    message('No need to split ', n, ' reverse dependencies into batches of size ', size, '.')
    if (any(grepl('Your branch is ahead of ', git('status', stdout = TRUE)))) {
      git('push')
    } else if (Sys.which('gh') != '') {
      gh(c('workflow', 'run', 'rev-check.yaml', '--ref', b))
      message('Triggering rev-check.yaml job against ', b, ' branch in crandalf repo on Github.')
    } else {
      message('Remember to re-run the last job for the package ', pkg, ' on Github.')
    }
    return(invisible())
  }

  b = ceiling(n/size)
  i = rep(seq_len(b), each = size)[seq_len(n)]
  k = 1
  # use an id in the commit so that I know which jobs are for the same pkg
  id = format(Sys.time(), '%Y%m%d%H%M')
  for (p in head(split(x, i), jobs)) {
    message('Batch ', k)
    writeLines(p, 'recheck')
    git(c('add', 'recheck'))
    git(c('commit', '-m', shQuote(paste(
      c(id, 'checking:', head(p, 3), '...'), collapse = ' '
    ))))
    git('push')
    Sys.sleep(10)
    k = k + 1
  }
}

#' @param repo The crandalf repo on Github (of the form `user/repo` such as
#'   `"yihui/crandalf"`). Usually you do not need to specify it, unless you
#'   are not calling this function inside the crandalf project, because
#'   \command{gh} should be able to figure out the repo automatically.
#' @param limit The maximum of records for \command{gh run list} to retrieve.
#'   You only need a larger number if the check results are very early in the
#'   Github Action history.
#' @param wait Number of seconds to wait if not all jobs have been completed on
#'   Github. By default, this function checks the status every 5 minutes until
#'   all jobs are completed. Set `wait` to 0 to disable waiting (and throw
#'   an error immediately when any jobs are not completed).
#' @rdname crandalf_check
#' @export
crandalf_results = function(pkg, repo = NA, limit = 200, wait = 5 * 60) {
  res = crandalf_jobs(pkg, repo, limit)
  if (NROW(res) == 0) {
    stop('Did not find check results for ', pkg, ' from Github Actions.')
  }
  if (any(res[, 1] != 'completed')) {
    if (wait <= 0) stop('Please wait till all jobs have been completed on Github Actions.')
    status = NULL
    repeat {
      res = crandalf_jobs(pkg, repo, limit)
      if (all(res[, 1] == 'completed')) break
      if (is.null(status) || !identical(status, table(res[, 1]))) {
        status = table(res[, 1])
        timestamp()
        print(status)
      }
      Sys.sleep(wait)
    }
  }
  ids = grep_sub('^(\\d+) checking: .+', '\\1', res[, 3])
  i = if (length(ids) > 0) grep(sprintf('^%s checking: ', ids[1]), res[, 3]) else {
    head(which(res[, 2] == 'failure'), 1)
  }
  res = res[i, , drop = FALSE]
  res = res[res[, 2] == 'failure', , drop = FALSE]
  if (NROW(res) == 0) {
    stop('Did not find any failed results on Github Actions.')
  }
  for (i in seq_len(nrow(res))) {
    message('Downloading check results (', i, '/', nrow(res), ')')
    gh_run('download', res[i, 7], '-D', tempfile('crandalf-', '.'), repo = repo)
  }
  if (interactive()) browseURL(crandalf_merge(pkg))
}

# retrieve the first N jobs info
crandalf_jobs = function(pkg, repo = NA, limit = 200) {
  res = gh_run('list', '-L', limit, '-w', 'rev-check', repo = repo)
  res = res[grep(paste0('rev-check\tcheck-', pkg), res)]
  do.call(rbind, strsplit(res, '\t'))
}

crandalf_merge = function(pkg) {
  unlink(list.files('.', '[.]Rcheck2?$'), recursive = TRUE)
  x1 = x2 = x3 = NULL
  f1 = '00check_diffs.html'; f3 = 'latex.txt'
  for (d in list.files('.', '^crandalf-.+')) {
    if (!dir_exists(d)) next
    p = file.path(d, 'macOS-rev-check-results')
    if (file_exists(f <- file.path(p, f1))) {
      x = read_utf8(f)
      x1 = if (length(x1) == 0) x else {
        i1 = grep('<body>', x)[1]
        i2 = tail(grep('</body>', x), 1)
        i3 = tail(grep('</body>', x1), 1)
        append(x1, x[(i1 + 1):(i2 - 1)], i3 - 1)
      }
      file.remove(f)
    }
    if (file_exists(f <- file.path(p, 'recheck2'))) {
      x2 = c(x2, read_utf8(f))
      file.remove(f)
    }
    cs = list.files(p, '[.]Rcheck[2]?$', full.names = TRUE)
    file.rename(cs, basename(cs))
    if (file_exists(f <- file.path(p, f3))) {
      x3 = c(x3, read_utf8(f))
      file.remove(f)
    }
    unlink(d, recursive = TRUE)
  }
  write_utf8(x1, f1)  # the full summary

  # store newly detected missing latex packages in latex.txt and commit/push
  git_co('main')
  append_unique(x3, f3)
  find_missing_latex()
  git(c('commit', '-m', shQuote('add more latex packages'), f3))
  git('push')

  git_co(paste0('check-', pkg))
  r = '[.]Rcheck2$'
  write_utf8(sort(unique(c(x2, gsub(r, '', list.files('.', r))))), 'recheck')
  f1
}

# mclapply() with a different default for mc.cores and disable prescheduling
plapply = function(X, FUN, ...) {
  parallel::mclapply(
    X, FUN, ..., mc.cores = getOption('mc.cores', parallel::detectCores()),
    mc.preschedule = FALSE
  )
}

# download the source package from CRAN
download_tarball = function(p, db = available.packages(type = 'source'), dir = '.', retry = 3) {
  if (!dir_exists(dir)) dir.create(dir, recursive = TRUE)
  z = file.path(dir, sprintf('%s_%s.tar.gz', p, db[p, 'Version']))
  mapply(function(p, z) {
    # remove other versions of the package tarball
    unlink(setdiff(list.files(dir, sprintf('^%s_.+.tar.gz', p), full.names = TRUE), z))
    for (i in seq_len(retry)) {
      if (file_exists(z)) break
      try(download.file(paste(db[p, 'Repository'], basename(z), sep = '/'), z, mode = 'wb'))
    }
  }, p, z, SIMPLIFY = FALSE)
  z
}

# clean up *.Rcheck if there are no warnings, errors, or notes in the log
clean_Rcheck = function(dir, log = read_utf8(file.path(dir, '00check.log'))) {
  # do not check the status line
  if (length(grep('^Status: ', tail(log, 1)))) log = head(log, -1)
  if (length(grep('(WARNING|ERROR|NOTE)$', log)) == 0 ||
      length(grep('[*] checking whether package .+ can be installed ... ERROR', log)))
    unlink(dir, recursive = TRUE)
  !dir_exists(dir)
}

#' @rdname rev_check
#' @param status_only If `TRUE`, only compare the final statuses of the
#'   checks (the last line of \file{00check.log}), and delete \file{*.Rcheck}
#'   and \file{*.Rcheck2} if the statuses are identical, otherwise write out the
#'   full diffs of the logs. If `FALSE`, compare the full logs under
#'   \file{*.Rcheck} and \file{*.Rcheck2}.
#' @param output The output Markdown file to which the diffs in check logs will
#'   be written. If the \pkg{markdown} package is available, the Markdown file
#'   will be converted to HTML, so you can see the diffs more clearly.
#' @export
compare_Rcheck = function(status_only = TRUE, output = '00check_diffs.md') {
  if (length(dirs <- list.files('.', '.+[.]Rcheck$')) == 0) {
    # clean up the `recheck` file
    if (file_exists('recheck')) writeLines(character(), 'recheck')
    return()
  }
  d2 = function(d) c(d, paste0(d, '2'))
  logs = function(d) file.path(d2(d), '00check.log')
  res = NULL
  if (!status_only && Sys.which('diff') == '')
    warning("The command 'diff' is not available; will not calculate exact diffs in logs.")
  for (d in dirs) {
    f = existing_files(logs(d))
    if (status_only && length(f) == 2) {
      status_line = function(file) {
        x = tail(read_utf8(file), 1)
        if (grepl('^Status: ', x)) x else {
          warning('The last line of ', file, ' is not the status.')
          NULL
        }
      }
      # if the check with current CRAN version of package also failed, or the
      # two statues are the same, chances are we are good to go
      s1 = status_line(f[1])
      if (length(grep('Status: .*\\d+ ERROR', s1)) || identical(s1, status_line(f[2]))) {
        unlink(d2(d), recursive = TRUE); next
      }
    }
    res = c(
      res, paste('##', p <- sans_ext(d)), '',
      sprintf('[CRAN version](https://cran.rstudio.com/package=%s) (-) vs current version (+):\n', p),
      '```diff', file_diff(f), '```', ''
    )
    if (length(res2 <- cran_check_page(p, NULL))) res = c(
      res, 'CRAN check logs:\n\n```', head_tail(unique(unlist(strsplit(res2, '\n')))), '```\n'
    )
  }
  if (length(res) == 0) return()
  xfun::write_utf8(res, output)
  if (!loadable('markdown')) return(output)
  markdown::mark_html(
    text = res,
    output = html_file <- with_ext(output, 'html')
  )
  if (!getOption('xfun.rev_check.keep_md', FALSE)) unlink(output)
  html_file
}

# keep the first and last n elements in x, and omit the middle
head_tail = function(x, n = 10) {
  if (length(x) <= 2 * n) return(x)
  c(head(x, n), '....', tail(x, n))
}

# compute the diffs of two files; if diffs too large, dedup them
file_diff = function(files, len = 200, use_diff = Sys.which('diff') != '') {
  n = length(files)
  if (n == 0) return()
  if (n == 1) {
    f = tempfile(); on.exit(unlink(f), add = TRUE); file.create(f)
    files = c(f, files)
  }
  d = if (use_diff) {
    suppressWarnings(system2('diff', shQuote(files), stdout = TRUE))
  } else {
    c(paste('<', read_utf8(files[1])), '---', paste('>', read_utf8(files[2])))
  }
  if (length(d) >= len) d = unique(d)
  gsub('^>', '+', gsub('^<', '-', d))
}

# specify a list of package names to be ignored when installing all dependencies
ignore_deps = function() {
  if (file_exists('00ignore_deps')) scan('00ignore_deps', 'character')
}

# download a check summary of a package from CRAN
cran_check_page = function(pkg, con = '00check-cran.log') {
  u = sprintf('https://cran.rstudio.com/web/checks/check_results_%s.html', pkg)
  x = read_utf8(u)
  if (length(i <- grep('Check Details', x, ignore.case = TRUE)) == 0) return()
  x = x[i[1]:length(x)]
  x = gsub('<[^>]+>', '', x)
  x = gsub('&nbsp;', ' ', x)
  x = gsub('&gt;', '>', x)
  x = gsub('&lt;', '<', x)
  x = gsub('\\s+', ' ', x)
  x = paste(trimws(x), collapse = '\n')
  x = gsub('\n\n+', '\n\n', x)
  if (length(con) == 1) writeLines(x, con) else x
}

# download CRAN check summaries of all failed packages
cran_check_pages = function() {
  dirs = list.files('.', '[.]Rcheck$')
  for (d in dirs) {
    if (dir_exists(d)) in_dir(d, cran_check_page(gsub('[.]Rcheck$', '', d)))
  }
}

# parse the check log for missing LaTeX packages and install them
find_missing_latex = function() {
  dirs = list.files('.', '[.]Rcheck2?$')
  pkgs = NULL
  for (d in dirs) {
    if (dir_exists(d)) pkgs = c(pkgs, in_dir(
      d, tinytex::parse_packages('00check.log', quiet = c(TRUE, FALSE, FALSE))
    ))
  }
  pkgs = unique(pkgs)
  if (file_exists(f <- 'latex.txt')) append_unique(pkgs, f)
  pkgs
}

# run revdepcheck::cloud_check()
cloud_check = function(pkgs = NULL, ...) {
  get_fun = function(name) getFromNamespace(name, 'revdepcheck')
  tgz = pkg_build()  # tarball
  pkg = gsub('_.*$', '', tgz)
  if (length(pkgs) == 0) pkgs = setdiff(get_fun('cran_revdeps')(pkg, bioc = TRUE), pkg)
  N = 9000  # max is 10000 packages per batch job
  jobs = broken = NULL
  rver = format(getRversion())
  check = function(...) {
    # make sure to check at least 2 packages
    if (length(pkgs) == 1) pkgs = c(pkgs, if (length(broken)) broken[1] else pkgs)
    try_check = function(...) {
      get_fun('cloud_check')(tarball = tgz, r_version = rver, revdep_packages = head(pkgs, N), ...)
    }
    jobs <<- c(jobs, tryCatch(
      try_check(...),
      error = function(e) {
        if (getRversion() != rver) stop(e)  # already tried a different version
        # if the current R version doesn't work, use the highest supported version
        r = ".*?\\[(('([0-9.]+)'(,\\s+)?)+)].*"
        x = grep(r, e$message, value = TRUE)
        x = gsub(r, '\\1', x)
        v = unlist(strsplit(x, "('|,\\s+)"))
        v = v[v != ''][1]
        if (length(v) != 1 || is.na(v)) stop(e)
        rver <<- v
        try_check(...)
      }
    ))
    pkgs <<- tail(pkgs, -N)
  }
  # if there are more than N revdeps, check the first N of them at one time
  while (length(pkgs) > 0) check(...)
  for (job in jobs) {
    assign('job_name', job, envir = get_fun('cloud_data'))
    get_fun('cloud_status')(update_interval = 60)
    if (length(res <- get_fun('cloud_broken')())) {
      get_fun('cloud_report')()
      for (p in res) print(get_fun('cloud_details')(revdep = p))
      fs = list.files(file.path('revdep/cloud.noindex', job), full.names = TRUE)
      # only keep results from broken packages
      unlink(fs[!basename(fs) %in% c(res, paste0(res, '.tar.gz'))], recursive = TRUE)
      broken = unique(c(res, broken))
    }
  }
  if (length(broken)) {
    stop('Package(s) broken: ', paste(broken, collapse = ' '))
  } else {
    message('All reverse dependencies are good!')
  }
}
