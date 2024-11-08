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
