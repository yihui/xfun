has_fun = function(name, envir = baseenv()) exists(name, envir, inherits = FALSE)

# for R < 4.0
if (!has_fun('isFALSE')) isFALSE = function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x) && !x
}
if (!has_fun('startsWith')) startsWith = function(x, s) {
  substring(x, 1, nchar(s)) == s
}
if (!has_fun('endsWith')) endsWith = function(x, s) {
  n = nchar(x)
  substring(x, n - nchar(s) + 1, n) == s
}
if (!has_fun('strrep')) strrep = function(x, times) {
  mapply(function(x, n) paste(rep(x, n), collapse = ''), x, times, USE.NAMES = FALSE)
}
if (!has_fun('packageDate', asNamespace('utils'))) packageDate = function(...) {
  as.Date(packageDescription(..., fields = 'Date/Publication'))
}

.onLoad = function(libname, pkgname) {
  # Register a finalizer so the background proxy is killed if the parent R
  # session exits without calling stop_app() (e.g., on crash or q()).
  reg.finalizer(asNamespace(pkgname), function(e) .onUnload(), onexit = TRUE)
}

.onUnload = function(libpath) {
  if (length(.proxy$apps) > 0L) stop_app()
  if (length(.proxy$help) > 0L) .stop_help_proxy()
}
