library(testit)

assert('invalid_utf8() works and respects NA', {
  (invalid_utf8(character()) %==% integer())
  x = 'fa\xE7ile'; Encoding(x) = 'latin1'
  (invalid_utf8(c('aaa', x, NA_character_, '', '\u4e2d\u6587')) %==% 2L)
})

assert('read/write_utf8() works', {
  ascii_txt = c('aa', 'bb', 'cc')
  utf8_txt = c('\u4e2d\u6587', '\u5927\u5bb6\u597d', '\u65e9\u996d')
  latin_txt = local({x = 'fa\xE7ile'; Encoding(x) = 'latin1'; x})
  mixed_txt = c(ascii_txt, latin_txt, utf8_txt)

  write_utf8(ascii_txt, con = (ascii_file = tempfile()))
  write_utf8(utf8_txt, con = (utf8_file = tempfile()))
  write_utf8(latin_txt, con = (latin_file = tempfile()))
  write_utf8(mixed_txt, con = (mixed_file = tempfile()))

  (read_utf8(ascii_file) %==% ascii_txt)
  (read_utf8(utf8_file) %==% utf8_txt)
  (read_utf8(latin_file) %==% latin_txt) # identical will not compare Encoding
  (Encoding(read_utf8(latin_file)) %==% 'UTF-8')
  (read_utf8(mixed_file) %==% mixed_txt)
  (Encoding(read_utf8(mixed_file)) %==% c(rep('unknown', 3), rep('UTF-8', 4)))

  mixed_file2 = tempfile()
  local({
    opts = options(encoding = 'native.enc'); on.exit(options(opts), add = TRUE)
    writeLines(mixed_txt, con = mixed_file2, useBytes = TRUE)
  })
  (suppressWarnings(read_utf8(mixed_file2)[4] != mixed_txt[4]))
  has_warning(read_utf8(mixed_file2))
  has_error(read_utf8(mixed_file2, error = TRUE))
})

