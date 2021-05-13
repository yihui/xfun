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

#' Submit a source package to CRAN
#'
#' Build a source package and submit it to CRAN with the \pkg{curl} package.
#' @param file The path to the source package tarball. By default, the current
#'   working directory is treated as the package root directory, and
#'   automatically built into a tarball, which is deleted after submission. This
#'   means you should run \code{xfun::submit_cran()} in the root directory of a
#'   package project, unless you want to pass a path explicitly to the
#'   \code{file} argument.
#' @param comment Submission comments for CRAN. By default, if a file
#'   \file{cran-comments.md} exists, its content will be read and used as the
#'   comment.
#' @seealso \code{devtools::submit_cran()} does the same job, with a few more
#'   dependencies in addition to \pkg{curl} (such as \pkg{cli});
#'   \code{xfun::submit_cran()} only depends on \pkg{curl}.
#' @export
submit_cran = function(file = pkg_build(), comment = '') {
  # if the tarball is automatically created, delete it after submission
  if (missing(file)) on.exit(file.remove(file), add = TRUE)

  # read the maintainer's name/email
  dir_create(d <- tempfile())
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  desc = file.path(gsub('_.*', '', basename(file)), 'DESCRIPTION')
  untar(file, desc, exdir = d)
  info = read.dcf(file.path(d, desc), fields = 'Maintainer')[1, 1]
  info = unlist(strsplit(info, '( <|>)'))

  # read submission comments from cran-comments.md if exists
  if (missing(comment) && file_exists(f <- 'cran-comments.md')) {
    comment = file_string(f)
  }
  params = list(
    uploaded_file = curl::form_file(file), name = info[1], email = info[2],
    comment = comment, upload = 'Upload package'
  )
  server = 'https://xmpalantir.wu.ac.at/cransubmit/index2.php'

  # submit the form
  h = curl::new_handle()
  curl::handle_setform(h, .list = params)
  res = curl::curl_fetch_memory(server, h)

  # find the pkg_id from the response page
  id = grep_sub(
    '(.*<input name="pkg_id" type="hidden" value=")([^"]+)(".*)', '\\2',
    rawToChar(res$content)
  )
  if (length(id) != 1) stop('Failed to submit ', file, ' to CRAN')

  # skip the review and submit directly
  h = curl::new_handle()
  curl::handle_setform(h, .list = list(pkg_id = id, submit = 'Submit package'))
  res = curl::curl_fetch_memory(server, h)
  if (grepl('>Step 3<', rawToChar(res$content))) message(
    'The package has been submitted. Please confirm the submission in email: ',
    params$email
  ) else message('The submission may be unsuccessful.')
}
