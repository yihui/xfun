#' Get the tags of Github releases of a repository
#'
#' Use the Github API to obtain the tags of the releases.
#' @param repo The repository name of the form \code{user/repo}, e.g.,
#'   \code{"yihui/xfun"}.
#' @param tag A tag as a character string. If provided, it will be returned if
#'   the tag exists. If \code{tag = "latest"}, the tag of the latest release is
#'   returned.
#' @param pattern A regular expression to match the tags.
#' @param use_jsonlite Whether to use \pkg{jsonlite} to parse the releases info.
#' @export
#' @return A character vector of (GIT) tags.
#' @examplesIf interactive()
#' xfun::github_releases('yihui/xfun')
#' xfun::github_releases('gohugoio/hugo')
github_releases = function(
  repo, tag = '', pattern = 'v[0-9.]+', use_jsonlite = loadable('jsonlite')
) {
  if (tag != '') return(github_releases2(repo, tag, pattern))

  i = 1; v = character()
  repeat {
    res = github_api(
      sprintf('/repos/%s/tags', repo), list(per_page = 100, page = i),
      raw = !use_jsonlite
    )
    v2 = unlist(if (use_jsonlite) {
      lapply(res, `[[`, 'name')
    } else {
      m = gregexec('\\{"name":"([^"]+)",', res)
      lapply(regmatches(res, m), function(x) x[2, ])
    })
    if (length(v2) == 0) break
    v = c(v, v2)
    if (length(v2) < 100) break  # not enough items for the next page
    i = i + 1
  }
  grep(sprintf('^%s$', pattern), unique(v), value = TRUE)
}

# the fallback method to retrieve release tags (read HTML source)
github_releases2 = function(repo, tag = '', pattern = '[^"&]+') {
  read = function() suppressWarnings(
    read_utf8(sprintf('https://github.com/%s/releases/%s', repo, tag))
  )
  h = if (tag == '') read() else tryCatch(read(), error = function(e) '')
  r = sprintf('^.*?%s/releases/tag/(%s)".*', repo, pattern)
  unique(grep_sub(r, '\\1', h))
}

# obtain results from Github API
github_api = function(
  endpoint, params = list(), headers = NULL, raw = !loadable('jsonlite')
) {
  token = unname(Sys.getenv(envs <- c('GITHUB_PAT', 'GITHUB_TOKEN', 'GH_TOKEN')))
  token = if (length(token <-  token[token != ''])) c(token = token[1]) else ''
  error = TRUE
  on.exit(if (error && token == '') message(
    'You may need to save a Github personal access token in one of the ',
    'environment variables: ', paste(envs, collapse = ', ')
  ))
  res = rest_api_raw('https://api.github.com', endpoint, token, params, headers)
  error = FALSE
  if (raw) res else jsonlite::fromJSON(res, FALSE)
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
