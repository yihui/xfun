library(testit)

assert("loadable works", {
  (loadable("base") %==% TRUE)
  (loadable("base", new_session = TRUE) %==% TRUE)
  (loadable("base", strict = FALSE) %==% TRUE)
  (has_error(loadable(c("base", "base2"))))
  (has_error(loadable(character())))
  (loadable("#base") %==% FALSE)
  (loadable("#base", new_session = TRUE) %==% FALSE)
  (loadable("#base", strict = FALSE) %==% FALSE)
})
