#' Get the tags of Github releases of a repository
#'
#' Read the HTML source of the release page and parse the tags of the releases.
#' @param repo The repository name of the form \code{user/repo}, e.g.,
#'   \code{"yihui/xfun"}.
#' @param subpath A character string to be appended to the URL of Github
#'   releases (i.e., \verb{https://github.com/user/repo/releases/}). For
#'   example, you may use \code{subpath = "latest"} to get the tag of the latest
#'   release.
#' @param pattern A regular expression to extract the tags from the HTML source.
#'   It must contain a group (i.e., must have a pair of parentheses).
#' @export
#' @return A character vector of (GIT) tags.
#' @examples if (interactive()) xfun::github_releases('yihui/xfun')
github_releases = function(repo, subpath = '', pattern = '(v[0-9.]+)') {
  h = readLines(sprintf('https://github.com/%s/releases/%s', repo, subpath), warn = FALSE)
  r = sprintf('^.*?releases/tag/%s".*', pattern)
  v = gsub(r, '\\1', grep(r, h, value = TRUE))
  unique(v)
}
