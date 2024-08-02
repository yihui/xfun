has_fun = function(name, envir = baseenv()) exists(name, envir, inherits = FALSE)

# for R < 4.0
if (!has_fun('startsWith')) startsWith = function(x, s) {
  substring(x, 1, nchar(s)) == s
}
if (!has_fun('endsWith')) endsWith = function(x, s) {
  n = nchar(x)
  substring(x, n - nchar(s) + 1, n) == s
}
if (!has_fun('anyNA')) anyNA = function(x) any(is.na(x))
if (!exists('trimws', baseenv(), inherits = FALSE)) trimws = function(x, which = 'both') {
  r = c(both = '^\\s+|\\s+$', right = '\\s+$', left = '^\\s+')[which]
  gsub(r, '', x)
}
if (!exists('file.size', baseenv(), inherits = FALSE)) file.size = function(...) {
  file.info(...)$size
}
if (!has_fun('strrep')) strrep = function(x, times) {
  mapply(function(x, n) paste(rep(x, n), collapse = ''), x, times, USE.NAMES = FALSE)
}
