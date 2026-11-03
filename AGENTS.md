# Repository Instructions

## Build and Test

``` bash
R CMD build .
R CMD INSTALL *_*.tar.gz
Rscript tests/*.R
```

Tests in `tests/testit/test-*.R`. Also `tests/test-cran/` (CRAN-safe) and
`tests/test-ci/` (CI only, gated by `tolower(Sys.getenv('CI')) == 'true'`).

Test form:

``` r
assert('message', {
  (actual %==% expected)
})
```

- Use `has_error()` for error testing, not `tryCatch()`
- Internal functions are auto-exposed by testit; call directly (no `:::`)

## R Code Style

- Assignment `=`, strings single-quoted, indent 2 spaces, wrap at 80 chars
- No `{}` for single-expression `if`
- No `else NULL` (implicit); no `return(NULL)` (use bare `return()`)
- Roxygen: no `@title`/`@description`/`@details`; write description directly
- Examples: double quotes; avoid `\dontrun{}`

## PR Checklist

1. Every change needs tests
2. Re-roxygenize: `roxygen2::roxygenize('.')` after roxygen changes, then
   run `sh .githooks/pre-commit` explicitly to post-process the changed `.Rd`
   files (it also runs on commit if `git config core.hooksPath .githooks` is
   set). Don't hand-edit that formatting; discard unrelated `man/*.Rd` churn
3. Run tests before pushing
4. Update `NEWS.md`: first heading form `# PKG x.y` (next CRAN release)
5. Clean up before committing: `rm -rf *.tar.gz *.Rcheck/ tests/R-lib-*`
6. Never create a branch/PR without confirming first
