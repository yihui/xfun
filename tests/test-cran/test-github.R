library(testit)

assert('release_versions() handles X.Y and X.Y.0 formats', {
  r = release_versions('0.59')
  (r$next_ver %==% '0.60')
  (r$patch %==% '0.59.1')

  r = release_versions('1.2.0')
  (r$next_ver %==% '1.3.0')
  (r$patch %==% '1.2.1')

  r = release_versions('10.0')
  (r$next_ver %==% '10.1')
  (r$patch %==% '10.0.1')

  (has_error(release_versions('1.2.3')))
  (has_error(release_versions('1')))
  (has_error(release_versions('1.2.1')))
})

assert('news_tpl() detects header format and falls back correctly', {
  hdr = function(news, pkg, ver, v) sprintf(news_tpl(news, pkg, ver), pkg, v)

  (hdr(c('# CHANGES IN foo VERSION 1.0', '', 'text'), 'foo', '1.0', '1.1') %==% '# CHANGES IN foo VERSION 1.1')
  (hdr(c('# foo 1.0', '', 'text'), 'foo', '1.0', '1.1') %==% '# foo 1.1')
  (hdr(c('# foo (version 1.0)', '', 'text'), 'foo', '1.0', '1.1') %==% '# foo (version 1.1)')

  # pkg name must match as whole word (not substring of another word)
  (hdr(c('# CHANGES in GE 1.1'), 'GE', '1.1', '1.2') %==% '# CHANGES in GE 1.2')

  # fallback: empty NEWS or unrecognized heading
  (hdr(character(0), 'foo', '1.0', '1.1') %==% '# CHANGES IN foo VERSION 1.1')
  (hdr(c('# Some unrelated heading'), 'foo', '1.0', '1.1') %==% '# CHANGES IN foo VERSION 1.1')
})

assert('github_token() returns the token from environment variables', {
  # save original env vars
  orig = Sys.getenv(c('GITHUB_PAT', 'GITHUB_TOKEN', 'GH_TOKEN'), names = TRUE)
  on.exit({
    do.call(Sys.setenv, as.list(orig))
  }, add = TRUE)
  # unset all three
  Sys.unsetenv(c('GITHUB_PAT', 'GITHUB_TOKEN', 'GH_TOKEN'))
  (github_token() %==% '')

  Sys.setenv(GITHUB_PAT = 'pat_token')
  (github_token() %==% 'pat_token')

  Sys.unsetenv('GITHUB_PAT')
  Sys.setenv(GITHUB_TOKEN = 'token_val')
  (github_token() %==% 'token_val')

  Sys.unsetenv('GITHUB_TOKEN')
  Sys.setenv(GH_TOKEN = 'gh_token_val')
  (github_token() %==% 'gh_token_val')

  # priority: GITHUB_PAT > GITHUB_TOKEN > GH_TOKEN
  Sys.setenv(GITHUB_PAT = 'pat_1st', GITHUB_TOKEN = 'token_2nd', GH_TOKEN = 'gh_3rd')
  (github_token() %==% 'pat_1st')
})
