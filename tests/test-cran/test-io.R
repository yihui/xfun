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

assert('empty files produce character() via file_string()', {
  tmp = tempfile()
  writeLines(character(), tmp)
  (file_string(tmp) %==% raw_string(character()))
})

assert('file_string() concatenates non-empty file lines with newlines', {
  f = tempfile()
  write_utf8(c('a', 'b', 'c'), f)
  (file_string(f) %==% raw_string('a\nb\nc'))
  unlink(f)
})

assert('grep_sub() matches elements and do substitution on them', {
  (grep_sub('a([b]+)c', 'a\\U\\1c', c('abc', 'abbbc', 'addc', '123'), perl = TRUE) %==%
     c('aBc', 'aBBBc'))
})

assert('read_bin() reads binary files', {
  f = tempfile()
  writeBin(as.raw(1:10), f)
  (read_bin(f) %==% as.raw(1:10))
  unlink(f)
})

assert('append_utf8() appends content to a file and sorts it by default', {
  f = tempfile()
  write_utf8(c('b', 'a'), f)
  append_utf8('c', f)
  (read_utf8(f) %==% c('a', 'b', 'c'))
})

assert('append_unique() appends only lines not already in the file and sorts', {
  f = tempfile()
  write_utf8(c('b', 'a'), f)
  append_unique(c('a', 'c'), f)  # 'a' is already there; only 'c' is new
  (read_utf8(f) %==% c('a', 'b', 'c'))
})

assert('process_file() applies a function to file contents', {
  f = tempfile()
  write_utf8('Hello World', f)
  process_file(f, function(x) gsub('World', 'R', x))
  (read_utf8(f) %==% 'Hello R')
  unlink(f)
})

assert('process_file() returns transformed result when file is missing', {
  (process_file(fun = toupper, x = 'hello') %==% 'HELLO')
})

assert('sort_file() sorts lines in a file', {
  f = tempfile()
  write_utf8(c('b', 'a', 'c'), f)
  sort_file(f)
  (read_utf8(f) %==% c('a', 'b', 'c'))
  unlink(f)
})

assert('gsub_file() replaces patterns in a file', {
  f = tempfile()
  writeLines(c('hello', 'world'), f)
  gsub_file(f, 'world', 'R', fixed = TRUE)
  (readLines(f) %==% c('hello', 'R'))
  unlink(f)
})

assert('gsub_file() skips file silently when rw_error=FALSE and read fails', {
  # a file with invalid UTF-8 content cannot be read; with rw_error=FALSE it is silently skipped
  f = tempfile()
  writeBin(as.raw(c(0x68, 0x65, 0x6c, 0x6c, 0xc0, 0x80)), f)  # invalid UTF-8 bytes
  result = gsub_file(f, 'a', 'b', rw_error = FALSE)
  (is.null(result))
  unlink(f)
})

assert('read_all() reads and concatenates files', {
  f1 = tempfile(); f2 = tempfile()
  write_utf8(c('a', 'b'), f1)
  write_utf8(c('c', 'd'), f2)
  res = read_all(c(f1, f2))
  (as.character(res) %==% c('a', 'b', 'c', 'd'))
  # with before/after: 1 before + 2 content + 1 after = 4 per file, 8 total
  res2 = read_all(c(f1, f2), before = function(f) paste('#', f), after = '')
  (length(res2) %==% 8L)
  unlink(c(f1, f2))
})

assert('gsub_dir() and gsub_ext() replace patterns in files within a dir', {
  d = tempfile()
  dir.create(d)
  f1 = file.path(d, 'a.txt')
  f2 = file.path(d, 'b.R')
  writeLines('hello world', f1)
  writeLines('foo bar', f2)
  gsub_dir('world', 'R', dir = d, fixed = TRUE)
  (readLines(f1) %==% 'hello R')
  (readLines(f2) %==% 'foo bar')  # unchanged
  # gsub_ext only processes files with specified extension
  gsub_ext('R', 'bar', 'BAR', dir = d, fixed = TRUE)
  (readLines(f2) %==% 'foo BAR')
  # filter by mimetype: only text files
  gsub_dir('R', 'REPLACED', dir = d, fixed = TRUE, mimetype = '^text/')
  (readLines(f1) %==% 'hello REPLACED')
  unlink(d, recursive = TRUE)
})

assert('lazy_save() and lazy_load() round-trip objects', {
  d = tempdir()
  path = file.path(d, 'lazy-test-')
  x = 1:10
  y = list(a = 'hello')
  lazy_save(c('x', 'y'), path)
  # verify index file exists
  (file.exists(paste0(path, '0.rds')))
  # load into new env
  e = new.env(parent = emptyenv())
  lazy_load(path, envir = e)
  (e$x %==% x)
  (e$y %==% y)
  unlink(list.files(d, pattern = '^lazy-test-', full.names = TRUE))
})

assert('write_utf8() handles NULL input', {
  f = tempfile()
  write_utf8(NULL, f)
  (read_utf8(f) %==% character(0))
  unlink(f)
})

assert('gsub_file() with rw_error=FALSE does not stop on read errors', {
  # a non-existent file should return invisible with rw_error=FALSE
  f = tempfile()  # doesn't exist
  result = gsub_file(f, 'a', 'b', rw_error = FALSE)
  (is.null(result))
})

assert('io_method() returns a list method as-is', {
  m = list(name = 'custom', save = saveRDS, load = readRDS)
  (io_method(m, '.') %==% m)
})
