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
#' on, the total remaing time will be roughly estimated via \code{n *
#' mean(times)}, where \code{n} is the number of packages remaining to be
#' checked, and \code{times} is a vector of elapsed time of packages that have
#' been checked.
#'
#' If a check on a reverse dependency failed, its \file{*.Rcheck} directory will
#' be renamed to \file{*.Rcheck2}, and another check will be run against the
#' CRAN version of the package. If the logs of the two checks are the same, it
#' means no new problems were introduced in the package, and you can probably
#' ignore this particular reverse dependency. The function
#' \code{compare_Rcheck()} can be used to create a summary of all the
#' differences in the check logs under \file{*.Rcheck} and \file{*.Rcheck2}.
#' This will be done automatically if \code{options(xfun.rev_check.summary =
#' TRUE)} has been set.
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
#' @param recheck A vector of package names to be (re)checked. If not provided
#'   and there are any \file{*.Rcheck} directories left by certain packages
#'   (this often means these packages failed the last time), \code{recheck} will
#'   be these packages; if there are no \file{*.Rcheck} directories but a text
#'   file \file{recheck} exists, \code{recheck} will be the character vector
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
  pkg, which = 'all', recheck = NULL, ignore = NULL, update = TRUE,
  src = file.path(src_dir, pkg), src_dir = getOption('xfun.rev_check.src_dir')
) {
  if (length(src) != 1 || !dir.exists(src)) stop(
    'The package source dir (the "src" argument) must be an existing directory'
  )

  message('Installing the source package ', src)
  install_dir(path.expand(src))

  db = available.packages(type = 'source')

  unlink('*.Rcheck2', recursive = TRUE)
  if (missing(recheck)) {
    dirs = list.files('.', '.+[.]Rcheck$')
    pkgs = gsub('.Rcheck$', '', dirs)
    recheck = if (length(pkgs) == 0 && file.exists('recheck')) {
      scan('recheck', 'character')
    } else pkgs
  }
  pkgs = if (length(recheck)) recheck else {
    res = check_deps(pkg, db, which)
    pkgs_up = NULL
    if (update) {
      message('Updating all R packages...')
      pkgs_up = intersect(old.packages(checkBuilt = TRUE)[, 'Package'], res$install)
      pkg_install(pkgs_up)
    }
    message('Installing dependencies of reverse dependencies')
    res$install = setdiff(res$install, ignore_deps())
    res$install = setdiff(res$install, pkgs_up)  # don't install pkgs that were just updated
    print(system.time({
      pkg_install(unlist(plapply(res$install, function(p) if (!loadable(p)) p)))
    }))
    res$check
  }
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

  if (missing(ignore) && file.exists('00ignore')) ignore = scan('00ignore', 'character')
  if (length(ignore)) {
    message('Ignoring packages: ', paste(ignore, collapse = ' '))
    unlink(sprintf('%s.Rcheck', ignore), recursive = TRUE)
    pkgs = setdiff(pkgs, ignore)
  }

  message('Downloading tarballs')
  tars = unlist(lapply(pkgs, function(p) download_tarball(p, db, dir = 'tarball')))
  tars = setNames(tars, pkgs)

  t0 = Sys.time()
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

    if (!file.exists(z <- tars[p])) {
      timing()
      return(dir.create(d, showWarnings = FALSE))
    }

    check_it = function(args = NULL, ...) {
      system2(
        file.path(R.home('bin'), 'R'),
        c(args, 'CMD', 'check', '--no-manual', shQuote(z)), stdout = FALSE, stderr = FALSE, ...
      )
    }
    check_it()

    if (!clean_Rcheck(d)) {
      if (!dir.exists(d)) {dir.create(d); return(timing())}
      # try to install missing LaTeX packages for vignettes if possible, then recheck
      vigs = list.files(
        file.path(d, 'vign_test', p, 'vignettes'), '[.](Rnw|Rmd)',
        ignore.case = TRUE, full.names = TRUE
      )
      pkg_load2('tinytex')
      if (length(vigs) && any(file.exists(with_ext(vigs, 'log')))) {
        if (tinytex:::is_tinytex()) for (vig in vigs) in_dir(dirname(vig), {
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
        file.exists('00install.out') && file.rename('00install.out', '00install.log')
      })
      cleanup()
      file.rename(d, d2 <- paste0(d, '2'))
      check_it('--no-environ', env = tweak_r_libs(lib_cran))
      if (!dir.exists(d)) file.rename(d2, d) else {
        cleanup()
        if (identical_logs(c(d, d2))) unlink(c(d, d2), recursive = TRUE)
      }
    }
    timing()
    NULL
  })
  if (getOption('xfun.rev_check.summary', FALSE)) {
    html = compare_Rcheck(); if (isTRUE(grepl('[.]html$', html))) browseURL(html)
  }
  res = Filter(function(x) !is.null(x), res)
  if (length(res)) res
}

# remove the OK lines in the check log
clean_log = function() {
  if (!file.exists(l <- '00check.log')) return()
  x = grep('^[*].+OK$', read_utf8(l), invert = TRUE, value = TRUE)
  # don't want diffs in random tempdir/tempfile paths when comparing check logs
  x[grep(dirname(tempdir()), x, fixed = TRUE)] = 'RANDOM TEMPDIR/TEMPFILE PATH DELETED'
  writeLines(tail(x, -2), l)  # remove the first 2 lines (log dir name and R version)
}

# are the check logs identical under a series of *.Rcheck directories?
identical_logs = function(dirs) {
  if (length(dirs) < 2) return(FALSE)
  if (!all(file.exists(logs <- file.path(dirs, '00check.log')))) return(FALSE)
  x = read_utf8(logs[1])
  for (i in 2:length(dirs)) if (!identical(x, read_utf8(logs[i]))) return(FALSE)
  TRUE
}

# add a new library path to R_LIBS_USER
tweak_r_libs = function(new) {
  x = read_first(c('.Renviron', '~/.Renviron'))
  x = grep('^\\s*#', x, invert = TRUE, value = TRUE)
  x = gsub('^\\s+|\\s+$', '', x)
  x = x[x != '']
  i = grep('^R_LIBS_USER=.+', x)
  if (length(i)) {
    x[i[1]] = sub('(="?)', paste0('\\1', new, .Platform$path.sep), x[i[1]])
    x
  } else c(paste0('R_LIBS_USER=', new), x)
}

# read the first file that exists
read_first = function(files) {
  for (f in files) if (file.exists(f)) return(read_utf8(f))
}

# a shorthand of tools::package_dependencies()
pkg_dep = function(x, ...) {
  if (length(x)) unique(unlist(tools::package_dependencies(x, ...)))
}

# calculate the packages required to check a package
check_deps = function(x, db = available.packages(), which = 'all') {
  if (identical(which, 'hard')) which = c('Depends', 'Imports', 'LinkingTo')
  # packages that reverse depend on me
  x1 = pkg_dep(x, db, which, reverse = TRUE)
  # only check a sample of soft reverse dependencies (useful if there are too many)
  if (identical(which, 'all') && (n <- getOption('xfun.rev_check.sample', 100)) >= 0) {
    x2 = pkg_dep(x, db, c('Suggests', 'Enhances'), reverse = TRUE)
    if (n < length(x2)) x1 = c(setdiff(x1, x2), sample(x2, n))
  }
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

# download the source package from CRAN
download_tarball = function(p, db = available.packages(type = 'source'), dir = '.', retry = 3) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  z = file.path(dir, sprintf('%s_%s.tar.gz', p, db[p, 'Version']))
  # remove other versions of the package tarball
  unlink(setdiff(list.files(dir, sprintf('^%s_.+.tar.gz', p), full.names = TRUE), z))
  for (i in seq_len(retry)) {
    if (file.exists(z)) break
    try(download.file(paste(db[p, 'Repository'], basename(z), sep = '/'), z, mode = 'wb'))
  }
  z
}

# allow users to specify a custom install.packages() function via the global
# option xfun.install.packages
pkg_install = function(pkgs, ...) {
  if (length(pkgs) == 0) return()
  install = getOption('xfun.install.packages', install.packages)
  if (length(pkgs) > 1)
    message('Installing ', length(pkgs), ' packages: ', paste(pkgs, collapse = ' '))
  install(pkgs, ...)
}

# clean up *.Rcheck if there are no warnings, errors, or notes in the log
clean_Rcheck = function(dir, log = read_utf8(file.path(dir, '00check.log'))) {
  if (length(grep('(WARNING|ERROR|NOTE)$', log)) == 0) unlink(dir, recursive = TRUE)
  !dir.exists(dir)
}

#' @rdname rev_check
#' @param status_only If \code{TRUE}, only compare the final statuses of the
#'   checks (the last line of \file{00check.log}), and delete \file{*.Rcheck}
#'   and \file{*.Rcheck2} if the statuses are identical, otherwise write out the
#'   full diffs of the logs. If \code{FALSE}, compare the full logs under
#'   \file{*.Rcheck} and \file{*.Rcheck2}.
#' @param output The output Markdown file to which the diffs in check logs will
#'   be written. If the \pkg{markdown} package is available, the Markdown file
#'   will be converted to HTML, so you can see the diffs more clearly.
#' @export
compare_Rcheck = function(status_only = FALSE, output = '00check_diffs.md') {
  if (length(dirs <- list.files('.', '.+[.]Rcheck2$')) == 0) return()
  d2 = function(d) c(sub('2$', '', d), d)
  logs = function(d) file.path(d2(d), '00check.log')
  dirs = dirs[rowSums(matrix(file.exists(logs(dirs)), ncol = 2)) == 2]
  res = NULL
  if (!status_only && Sys.which('diff') == '')
    warning("The command 'diff' is not available; will not calculate exact diffs in logs.")
  for (d in dirs) {
    f = logs(d)
    if (status_only) {
      status_line = function(file) {
        x = tail(read_utf8(file), 1)
        if (!grepl('^Status: ', x)) stop('The last line of ', file, ' is not the status.')
        x
      }
      if (status_line(f[1]) == status_line(f[2])) {
        unlink(d2(d), recursive = TRUE); next
      }
    }
    res = c(
      res, paste('##', p <- sans_ext(d)), '',
      sprintf('[CRAN version](https://cran.rstudio.com/package=%s) (-) vs current version (+):\n', p),
      '```diff', file_diff(f), '```', ''
    )
    if (length(res2 <- cran_check_page(p, NULL))) res = c(
      res, 'CRAN check logs:\n\n```', unique(unlist(strsplit(res2, '\n'))), '```\n'
    )
  }
  writeLines(res, output)
  if (!loadable('markdown')) return(output)
  markdown::markdownToHTML(
    text = gsub('>', '+', gsub('^<', '-', res)),
    output = html_file <- with_ext(output, 'html'),
    header = c(
      "<link href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/styles/github.min.css' rel='stylesheet' type='text/css' />",
      "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.17.1/build/highlight.min.js'></script>",
      "<script>hljs.initHighlightingOnLoad();</script>"
    ), encoding = 'UTF-8'
  )
  unlink(output)
  html_file
}

# compute the diffs of two files; if diffs too large, dedup them
file_diff = function(files, len = 200, use_diff = Sys.which('diff') != '') {
  d = if (use_diff) {
    suppressWarnings(system2('diff', shQuote(files), stdout = TRUE))
  } else {
    c(paste('<', read_utf8(files[1])), '---', paste('>', read_utf8(files[2])))
  }
  if (length(d) >= len) unique(d) else d
}

# specify a list of package names to be ignored when installing all dependencies
ignore_deps = function() {
  if (file.exists('00ignore_deps')) scan('00ignore_deps', 'character')
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
    if (dir.exists(d)) in_dir(d, cran_check_page(gsub('[.]Rcheck$', '', d)))
  }
}

# kill a R CMD check process if it has been running for more then 30 minutes
kill_long_processes = function(etime = 30) {
  while (TRUE) {
    if (length(pids <- list_long_processes(etime))) {
      message('Killing processes: ', paste(pids, collapse = ' '))
      system2('kill', pids)
    }
    Sys.sleep(30)
  }
}

list_long_processes = function(etime = 15) {
  x = system('ps -ax -o pid,etime,command | grep "Rcmd check --no-manual"', intern = TRUE)
  x = grep('_[0-9.-]+[.]tar[.]gz$', trimws(x), value = TRUE)
  pids = unlist(lapply(strsplit(x, '\\s+'), function(z) {
    pid = z[1]; time = as.numeric(unlist(strsplit(z[2], '-|:')))
    time = sum(tail(c(rep(0, 4), time), 4) * c(24 * 3600, 3600, 60, 1))
    name = gsub('.*/', '', tail(z, 1))
    if (time > etime * 60) setNames(pid, name)
  }))
  pids
}

# parse the check log for missing LaTeX packages and install them
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

# return packages that haven't been updated for X days, and can be updated on CRAN
cran_updatable = function(days = 90, maintainer = 'Yihui Xie') {
  info = tools::CRAN_package_db()
  pkgs = info[grep(maintainer, info$Maintainer), 'Package']
  info = setNames(vector('list', length(pkgs)), pkgs)
  for (p in pkgs) {
    message('Processing ', p)
    x = readLines(u <- sprintf('https://cran.rstudio.com/web/packages/%s/', p))
    i = which(x == '<td>Published:</td>')
    if (length(i) == 0) stop('Cannot find the publishing date from ', u)
    d = as.Date(gsub('</?td>', '', x[i[1] + 1]))
    x = try(readLines(u <- sprintf('https://cran.r-project.org/src/contrib/Archive/%s/', p)))
    if (inherits(x, 'try-error')) {
      info[[p]] = d; next
    }
    r = '.+</td><td align="right">(\\d{4,}-\\d{2}-\\d{2}) .+'
    d = c(d, as.Date(gsub(r, '\\1', grep(r, x, value = TRUE))))
    info[[p]] = sort(d, decreasing = TRUE)
  }
  flag = unlist(lapply(info, function(d) {
    sum(d > Sys.Date() - 180) < 6 && d[1] < Sys.Date() - days
  }))
  names(which(flag))
}
