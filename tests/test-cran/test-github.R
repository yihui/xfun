library(testit)

assert('release_versions() handles X.Y and X.Y.0 formats', {
  rv = function(next_ver, patch) list(next_ver = next_ver, patch = patch)

  (release_versions('0.59') %==% rv('0.60', '0.59.1'))
  (release_versions('1.2.0') %==% rv('1.3.0', '1.2.1'))
  (release_versions('10.0') %==% rv('10.1', '10.0.1'))
  (release_versions('0') %==% rv('1', '0.1'))
  (release_versions('10.0.1', 2) %==% rv('10.1.0', '10.0.1.1'))
  (release_versions('1.2.3') %==% rv('1.2.4', '1.2.3.1'))
  (release_versions('1.0.0') %==% rv('1.1.0', '1.0.1'))
  (release_versions('1.2.0', 1) %==% rv('2.0.0', '1.2.1'))
  (release_versions('1.2.3', 9) %==% rv('1.2.4', '1.2.3.1'))
  (release_versions('10.0', patch = '.0001') %==% rv('10.1', '10.0.0001'))
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
