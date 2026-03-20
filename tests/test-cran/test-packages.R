library(testit)

assert("loadable() works", {
  (loadable("base"))
  # (loadable("base", new_session = TRUE))
  (loadable("base", strict = FALSE))
  (has_error(loadable(c("base", "base2"))))
  (has_error(loadable(character())))
  (!loadable("#base"))
  # (!loadable("#base", new_session = TRUE))
  (!loadable("#base", strict = FALSE))
})

assert('pkg_load() loads packages and returns logical vector', {
  res = pkg_load('base', 'stats', error = FALSE)
  (res %==% c(TRUE, TRUE))
  res2 = pkg_load('#nonexistent#pkg', error = FALSE)
  (!res2)
})

assert('pkg_attach() attaches packages', {
  pkg_attach('stats')
  ('package:stats' %in% search())
})

assert('pkg_available() checks if a package with a version is available', {
  (pkg_available('base'))
  (pkg_available('base', '1.0.0'))
  (!pkg_available('#nonexistent#pkg'))
})

assert('base_pkgs() returns base package names', {
  pkgs = base_pkgs()
  (is.character(pkgs))
  ('base' %in% pkgs)
  ('stats' %in% pkgs)
})
