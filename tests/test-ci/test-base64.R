library(testit)

assert('.mime_cmd() works', {
  # test some common file types
  fs = list.files(R.home('doc'), '[.](html|pdf|png)$', recursive = TRUE, full.names = TRUE)
  fs = fs[!duplicated(file_ext(fs))]
  m = list(html = 'text/html', pdf = 'application/pdf', png = 'image/png')
  (lapply(fs, .mime_cmd) %==% unname(m[file_ext(fs)]))
})
