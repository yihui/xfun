# retrieve the release dates of packages
cran_pkg_dates = function(full = FALSE, maintainer = 'Yihui Xie') {
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
  if (full) info else sort(do.call(c, lapply(info, `[`, 1)), decreasing = TRUE)
}

# return packages that haven't been updated for X days, and can be updated on CRAN
cran_updatable = function(days = 90, maintainer = 'Yihui Xie') {
  info = cran_pkg_dates(TRUE, maintainer)
  flag = unlist(lapply(info, function(d) {
    sum(d > Sys.Date() - 180) < 6 && d[1] < Sys.Date() - days
  }))
  names(which(flag))
}


#' Some utility functions for checking packages
#'
#' Miscellaneous utility functions to obtain information about the package
#' checking environment.
#' @export
#' @keywords internal
is_R_CMD_check = function() {
  !is.na(check_package_name())
}

#' @rdname is_R_CMD_check
#' @export
is_CRAN_incoming = function() {
  isTRUE(as.logical(Sys.getenv('_R_CHECK_CRAN_INCOMING_REMOTE_')))
}

#' @rdname is_R_CMD_check
#' @export
check_package_name = function() {
  Sys.getenv('_R_CHECK_PACKAGE_NAME_', NA)
}

# is R CMD check running on a package that has a version lower or equal to `version`?
#' @rdname is_R_CMD_check
#' @export
check_old_package = function(name, version) {
  if (is.na(pkg <- check_package_name()) || pkg != name) return(FALSE)
  tryCatch(packageVersion(name) <= version, error = function(e) FALSE)
}

# return package maintainers (with email addresses)
pkg_maintainers = function(pkgs) {
  info = tools::CRAN_package_db()
  info = info[match(pkgs, info$Package), c('Package', 'Maintainer')]
  setNames(info$Maintainer, info$Package)
}
