if (getRversion() <= '3.2.1') for (m in c('wget', 'curl')) if (Sys.which(m) != '') {
  options(download.file.method = m)
  cat(sprintf('\noptions(download.file.method = "%s")\n', m), file = '~/.Rprofile', append = TRUE)
  break
}

# install all deps (including Suggests)
install.packages('xfun', dependencies = TRUE)
