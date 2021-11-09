#' Get the tags of Github releases of a repository
#'
#' Use the Github API to obtain the tags of the releases.
#' @param repo The repository name of the form \code{user/repo}, e.g.,
#'   \code{"yihui/xfun"}.
#' @param tag A tag as a character string. If provided, it will be returned if
#'   the tag exists. If \code{tag = "latest"}, the tag of the latest release is
#'   returned.
#' @param pattern A regular expression to match the tags.
#' @export
#' @return A character vector of (GIT) tags.
#' @examplesIf interactive()
#' xfun::github_releases('yihui/xfun')
#' xfun::github_releases('gohugoio/hugo')
github_releases = function(
  repo, tag = '', pattern = 'v[0-9.]+', use_jsonlite = loadable('jsonlite')
) {
  i = 1; v = character()
  repeat {
    h = sprintf('https://api.github.com/repos/%s/tags?per_page=100&page=%d', repo, i)
    v2 = unlist(if (use_jsonlite) {
      res = jsonlite::fromJSON(h, FALSE)
      lapply(res, `[[`, 'name')
    } else {
      res = read_utf8(h)
      m = gregexec('\\{"name":"([^"]+)",', res)
      lapply(regmatches(res, m), function(x) x[2, ])
    })
    if (length(v2) == 0) break
    if (tag == 'latest') return(v2[1])
    v = c(v, v2)
    if (tag %in% v) return(tag)
    if (length(v2) < 100) break  # not enough items for the next page
    i = i + 1
  }
  if (length(v)) return(grep(sprintf('^%s$', pattern), unique(v), value = TRUE))

  # the fallback method (read HTML source)
  h = read_utf8(sprintf('https://github.com/%s/releases/%s', repo, tag))
  r = sprintf('^.*?releases/tag/(%s)".*', pattern)
  v = grep_sub(r, '\\1', h)
  unique(v)
}

git = function(...) {
  if (Sys.which('git') == '') stop('git is not available')
  system2('git', ...)
}

git_co = function(args = NULL, ...) {
  git(c('checkout', args), ...)
}

git_test_branch = function() {
  if (length(d <- git(c('diff', '--name-only'), stdout = TRUE))) stop(
    'The current branch has changes not stated for commit:\n',
    paste(d, collapse = '\n')
  )
}

gh = function(...) {
  if (Sys.which('gh') == '') stop('Github CLI not found: https://cli.github.com')
  system2('gh', ...)
}

gh_run = function(..., repo = NA) {
  gh(c(if (!is.na(repo)) c('-R', repo), 'run', ...), stdout = TRUE)
}
