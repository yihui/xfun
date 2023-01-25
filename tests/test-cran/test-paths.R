library(testit)

assert('file_ext() and sans_ext() work', {
  p = c('abc.doc', 'def123.tex#', 'path/to/foo.Rmd', 'backup.ppt~', 'pkg.tar.xz')
  (file_ext(p) %==% c('doc', 'tex#', 'Rmd', 'ppt~', 'tar.xz'))
  (sans_ext(p) %==% c('abc', 'def123', 'path/to/foo', 'backup', 'pkg'))
  (file_ext(c('foo.bar.gz', 'foo', 'file.nb.html')) %==% c('gz', '', 'nb.html'))
  # some special extensions
  p = c('abc.c++', 'def.c--', 'ghi.e##', 'jkl.FB2K-COMPONENT', 'mno.WITNESS_CAMPAIGN', 'pqr.H!')
  (file_ext(p, '-+!_#') %==% c('c++', 'c--', 'e##', 'FB2K-COMPONENT', 'WITNESS_CAMPAIGN', 'H!'))
  # by default, these extensions are not recognized
  (file_ext(p) %==% character(length(p)))
})

assert('with_ext() works for corner cases', {
  (with_ext(character(), 'abc') %==% character())
  (with_ext('abc', character()) %==% 'abc')
  (with_ext(NA_character_, 'abc') %==% NA_character_)
  (has_error(with_ext('abc', NA_character_)))
  (with_ext('abc', c('d', 'e')) %==% c('abc.d', 'abc.e'))
  (has_error(with_ext(c('a', 'b'), c('d', 'e', 'f'))))
  (with_ext(c('a', 'b'), c('d', 'e')) %==% c('a.d', 'b.e'))
  (with_ext(c('a', 'b'), c('d')) %==% c('a.d', 'b.d'))
  (with_ext(c('a', 'b', 'c'), c('', '.d', 'e.e')) %==% c('a', 'b.d', 'c.e.e'))
})

assert('same_path() works', {
  (is.na(same_path('~/foo', NA_character_)))
  (is.na(same_path(NA_character_, '~/foo')))
  (same_path('~/foo', file.path(Sys.getenv('HOME'), 'foo')))
  (!same_path(tempdir(), 'foo'))
})

assert('normalize_path() works', {
  f1 = tempfile()
  writeLines('test symlink', f1)
  f2 = paste0(f1, '~')
  res = file.symlink(f1, f2)  # this may fail (on Windows), i.e., res = FALSE
  # resolve symlink by default
  (!res || basename(normalize_path(f2)) %==% basename(f1))
  # do not resolve symlink
  (!res || basename(normalize_path(f2, resolve_symlink = FALSE)) %==% basename(f2))
  # resolve_symlink = FALSE should work with inputs like . and ..
  (normalize_path(c('.', '..'), resolve_symlink = FALSE) %==% normalize_path(c('.', '..')))
})

assert('url_filename() returns the file names in URLs', {
  (url_filename('https://yihui.org/images/logo.png') %==% 'logo.png')
  (url_filename(c(
    'https://yihui.org/index.html',
    'https://yihui.org/index.html?foo=bar',
    'https://yihui.org/index.html#about'
  )) %==% rep('index.html', 3))
})

assert('is_abs_path() recognizes absolute paths on Windows and *nix', {
  (!is_abs_path('abc/def'))
  (is_abs_path(if (.Platform$OS.type == 'windows') {
    c('D:\\abc', '\\\\netdrive\\somewhere')
  } else '/abc/def'))
})

assert('del_empty_dir() correctly deletes empty dirs', {
  # do nothing is NULL
  (del_empty_dir(NULL) %==% NULL)
  # remove if empty
  dir.create(temp_dir <- tempfile())
  del_empty_dir(temp_dir)
  (!dir_exists(temp_dir))
  # do not remove if not empty
  dir.create(temp_dir <- tempfile())
  writeLines('test', tempfile(tmpdir = temp_dir))
  (del_empty_dir(temp_dir) %==% NULL)
  (dir_exists(temp_dir))
  unlink(temp_dir, recursive = TRUE)
})

assert('mark_dirs add trailing / when necessary', {
  local({
    dir.create(tmp_dir <- tempfile())
    tmp_dir_slash = paste0(tmp_dir, "/")
    file.create(tmp_file <- tempfile(tmpdir = tmp_dir))
    (mark_dirs(c(tmp_dir, tmp_file)) %==% c(tmp_dir_slash, tmp_file))
    (mark_dirs(c(tmp_dir_slash, tmp_file)) %==% c(tmp_dir_slash, tmp_file))
    unlink(tmp_dir, recursive = TRUE)
  })
})

assert("relative_path() works", {
  (relative_path(c('foo/bar.txt', 'foo/baz.txt'), 'foo/') %==% c("bar.txt", "baz.txt"))
  (relative_path('foo/bar.txt', 'foo') %==% "bar.txt")
})

assert("proj_root() works", {
  # detect .Rproj root
  dir.create(tmp_dir <- tempfile())
  tmp_dir_slash <- paste0(tmp_dir, "/")
  file.create(f1 <- file.path(tmp_dir, "test.Rproj"))
  writeLines(c("Version: 1.2.3", "test: 321"), f1)

  (same_path(proj_root(tmp_dir), tmp_dir) %==% TRUE)
  unlink(f1)

  # detect package root
  file.create(f2 <- file.path(tmp_dir, "DESCRIPTION"))
  writeLines(c("Package: abc", "test: 321"), f2)
  dir.create(tmp_dir_child <- tempfile(tmpdir = tmp_dir))

  (same_path(proj_root(tmp_dir_child), tmp_dir) %==% TRUE)
  unlink(tmp_dir, recursive = TRUE)
})
