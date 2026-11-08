library(testit)

assert('with_browser_tmp() isolates and cleans the browser temp dir', {
  old = Sys.getenv('TMPDIR', unset = NA)
  # during evaluation TMPDIR points at a fresh dir created for the browser
  seen = with_browser_tmp(Sys.getenv('TMPDIR'))
  (startsWith(basename(seen), 'browser-') %==% TRUE)
  # afterward that dir is removed and TMPDIR restored to its prior value
  (dir.exists(seen) %==% FALSE)
  (Sys.getenv('TMPDIR', unset = NA) %==% old)
})
