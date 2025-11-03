if (getRversion() <= '3.2.1') for (m in c('wget', 'curl')) if (Sys.which(m) != '') {
  options(download.file.method = m)
  cat(sprintf('\noptions(download.file.method = "%s")\n', m), file = '~/.Rprofile', append = TRUE)
  break
}
# Install minimal dependencies needed for R CMD check
# xfun only imports grDevices, stats, tools (all base packages)
# so no additional packages needed for basic check
