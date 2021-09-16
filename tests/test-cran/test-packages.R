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

assert("major_mninor_smaller() works", {
  (major_minor_smaller(as.numeric_version("4.1.0"), as.numeric_version("4.2.0")))
  (!major_minor_smaller(as.numeric_version("4.1.0"), as.numeric_version("4.1.1")))
})
