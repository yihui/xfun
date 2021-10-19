#' An alternative to sessionInfo() to print session information
#'
#' This function tweaks the output of \code{\link{sessionInfo}()}: (1) It adds
#' the RStudio version information if running in the RStudio IDE; (2) It removes
#' the information about matrix products, BLAS, and LAPACK; (3) It removes the
#' names of base R packages; (4) It prints out package versions in a single
#' group, and does not differentiate between loaded and attached packages.
#'
#' It also allows you to only print out the versions of specified packages (via
#' the \code{packages} argument) and optionally their recursive dependencies.
#' For these specified packages (if provided), if a function
#' \code{xfun_session_info()} exists in a package, it will be called and
#' expected to return a character vector to be appended to the output of
#' \code{session_info()}. This provides a mechanism for other packages to inject
#' more information into the \code{session_info} output. For example,
#' \pkg{rmarkdown} (>= 1.20.2) has a function \code{xfun_session_info()} that
#' returns the version of Pandoc, which can be very useful information for
#' diagnostics.
#' @param packages A character vector of package names, of which the versions
#'   will be printed. If not specified, it means all loaded and attached
#'   packages in the current R session.
#' @param dependencies Whether to print out the versions of the recursive
#'   dependencies of packages.
#' @return A character vector of the session information marked as
#'   \code{\link{raw_string}()}.
#' @export
#' @examplesIf interactive()
#' xfun::session_info()
#' if (xfun::loadable('MASS')) xfun::session_info('MASS')
session_info = function(packages = NULL, dependencies = TRUE) {
  res = sessionInfo()
  res$matprod = res$BLAS = res$LAPACK = NULL
  if (loadable('rstudioapi') && rstudioapi::isAvailable()) {
    res$running = paste0(res$running, ', RStudio ', rstudioapi::getVersion())
  }

  tweak_info = function(obj, extra = NULL) {
    res = capture.output(print(obj))
    i = grep('^(attached base packages|Matrix products):\\s*$', res, ignore.case = TRUE)
    if (length(i)) res = res[-c(i, i + 1)]
    res = gsubi('^\\s*locale:\\s*$', 'Locale:', res)
    res = gsub('^\\s*\\[[0-9]+]\\s*', '  ', res)  # remove vector indices like [1]
    res = gsubi('^\\s*other attached packages:\\s*$', 'Package version:', res)
    # print the locale info on a single line if possible
    if (length(i <- which(res == 'Locale:')) == 1 && res[i + 2] == '') {
      res[i] = paste(res[i], gsub('\\s*/\\s*', ' / ', gsub('^\\s+', '', res[i + 1])))
      res = res[-(i + 1)]
    }
    raw_string(c(res, extra))
  }

  version_info = function(pkgs) {
    res = lapply(pkgs, function(p) {
      list(Version = as.character(packageVersion(p)), Package = p)
    })
    as.list(setNames(res, pkgs))
  }

  res$basePkgs = raw_string(list())
  info = c(res$otherPkgs, res$loadedOnly)
  if (length(packages) > 0) {
    info = info[intersect(names(info), packages)]
    info = c(info, version_info(setdiff(packages, names(info))))
  }
  res$loadedOnly = NULL
  if (dependencies) {
    deps = pkg_dep(names(info), installed.packages(), recursive = TRUE)
    deps = sort(setdiff(deps, names(info)))
    info = c(info, version_info(deps))
  }
  if (length(packages) > 0 || dependencies) info = info[sort(names(info))]
  res$otherPkgs = info
  extra = unlist(lapply(packages, function(p) tryCatch(
    c('', getFromNamespace('xfun_session_info', p)()), error = function(e) NULL)
  ))

  tweak_info(res, extra)
}

#' Perform a task once in an R session
#'
#' Perform a task once in an R session, e.g., emit a message or warning. Then
#' give users an optional hint on how not to perform this task at all.
#' @param task Any R code expression to be evaluated once to perform a task,
#'   e.g., \code{warning('Danger!')} or \code{message('Today is ', Sys.Date())}.
#' @param option An R option name. This name should be as unique as possible in
#'   \code{\link{options}()}. After the task has been successfully performed,
#'   this option will be set to \code{FALSE} in the current R session, to
#'   prevent the task from being performed again the next time when
#'   \code{do_once()} is called.
#' @param hint A character vector to provide a hint to users on how not to
#'   perform the task or see the message again in the current R session. Set
#'   \code{hint = ""} if you do not want to provide the hint.
#' @return The value returned by the \code{task}, invisibly.
#' @export
#' @examples
#' do_once(message("Today's date is ", Sys.Date()), "xfun.date.reminder")
#' # if you run it again, it will not emit the message again
#' do_once(message("Today's date is ", Sys.Date()), "xfun.date.reminder")
#'
#' do_once({Sys.sleep(2); 1 + 1}, "xfun.task.1plus1")
#' do_once({Sys.sleep(2); 1 + 1}, "xfun.task.1plus1")
do_once = function(task, option, hint = c(
  'You will not see this message again in this R session.',
  'If you never want to see this message,',
  sprintf('you may set options(%s = FALSE) in your .Rprofile.', option)
)) {
  if (isFALSE(getOption(option))) return(invisible())
  task
  hint = paste(hint, collapse = ' ')
  if (hint != '') message(hint)
  options(setNames(list(FALSE), option))
  invisible(task)
}
