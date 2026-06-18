library(testit)

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
