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
    tmp_dir_slash = paste0(tmp_dir, '/')
    file.create(tmp_file <- tempfile(tmpdir = tmp_dir))
    (mark_dirs(c(tmp_dir, tmp_file)) %==% c(tmp_dir_slash, tmp_file))
    (mark_dirs(c(tmp_dir_slash, tmp_file)) %==% c(tmp_dir_slash, tmp_file))
    unlink(tmp_dir, recursive = TRUE)
  })
})

assert('relative_path() works', {
  (relative_path(c('foo/bar.txt', 'foo/baz.txt'), 'foo/') %==% c('bar.txt', 'baz.txt'))
  (relative_path('foo/bar.txt', 'foo') %==% 'bar.txt')
})

assert('proj_root() works', {
  # detect .Rproj root
  dir.create(tmp_dir <- tempfile())
  tmp_dir_slash <- paste0(tmp_dir, '/')
  file.create(f1 <- file.path(tmp_dir, 'test.Rproj'))
  writeLines(c('Version: 1.2.3', 'test: 321'), f1)

  (same_path(proj_root(tmp_dir), tmp_dir) %==% TRUE)
  unlink(f1)

  # detect package root
  file.create(f2 <- file.path(tmp_dir, 'DESCRIPTION'))
  writeLines(c('Package: abc', 'test: 321'), f2)
  dir.create(tmp_dir_child <- tempfile(tmpdir = tmp_dir))

  (same_path(proj_root(tmp_dir_child), tmp_dir) %==% TRUE)
  unlink(tmp_dir, recursive = TRUE)
})

assert('file_rename() works', {
  # work in temp dir
  dir.create(tmp_dir <- tempfile())
  owd = setwd(tmp_dir)

  # empty dir is not moved but deleted
  dir.create('dest')
  dir.create('empty')
  file_rename('empty', 'dest')
  (dir.exists(c('empty', 'dest')) %==% c(FALSE, TRUE))

  # files are moved correctly
  dir.create('filled')
  dummy_files = c('dummy1', 'dummy2')
  file.create(file.path('filled', dummy_files))
  file_rename('filled', 'dest1')
  (dir.exists('filled') %==% FALSE)
  (list.files('dest1') %==% dummy_files)

  # rename multiple dirs
  dir.create('dest2')
  file_rename(c('dest1', 'dest2'), c('dest3', 'dest4'))
  (dir.exists(sprintf('dest%d', 1:4)) %==% c(FALSE, FALSE, TRUE, TRUE))

  # rename files
  file_rename(file.path('dest3', dummy_files), dummy_files)
  (length(list.files('dest3'))  %==% 0L)
  (file.exists(dummy_files) %==% c(TRUE, TRUE))

  # remove temp dir
  setwd(owd)
  unlink(tmp_dir, recursive = TRUE)
})

assert('rename_seq() returns renamed file patterns', {
  dir.create(tmp_dir <- tempfile())
  owd = setwd(tmp_dir)
  # no matching files -> returns character(0)
  (rename_seq() %==% character(0))
  # create some files matching the pattern
  file.create(c('01-foo.Rmd', '02-bar.Rmd', '03-baz.Rmd'))
  res = rename_seq()
  (length(res) %==% 3L)
  (inherits(res, 'xfun_rename_seq'))
  # print method should work without error
  capture.output(print(res))
  # dry_run = FALSE actually renames
  file.create(c('01-x.Rmd', '02-y.Rmd'))
  rename_seq(pattern = '^[0-9]+-[xy][.]Rmd$', dry_run = FALSE)
  (all(file.exists(c('1-x.Rmd', '2-y.Rmd'))))
  setwd(owd)
  unlink(tmp_dir, recursive = TRUE)
})

assert('existing_files() with first = TRUE works', {
  f = tempfile(); writeLines('test', f)
  (existing_files(f, first = TRUE) %==% f)
  unlink(f)
  (has_error(existing_files(c('nonexistent1', 'nonexistent2'), first = TRUE)))
  (existing_files(c('nonexistent1', 'nonexistent2'), first = TRUE, error = FALSE) %==% character(0))
  (has_error(existing_files('nonexistent', first = TRUE, error = 'Custom error msg')))
})

assert('is_web_path() detects web paths', {
  (is_web_path(c('https://www.r-project.org', 'http://example.com',
    'ftp://files.example.com', 'ftps://secure.example.com')))
  (!is_web_path(c('www.r-project.org', '/local/path', 'relative/path')))
})

assert('is_rel_path() is the negation of is_abs_path()', {
  (is_rel_path('foo.txt'))
  (!is_rel_path(tempdir()))
})

assert('with_ext() removes extensions when ext is empty string', {
  (with_ext(c('a.txt', 'b.R'), '') %==% c('a', 'b'))
})

assert('relative_path() edge cases', {
  (has_error(relative_path('/tmp/some/file.txt', '/different/dir', use.. = FALSE)))
  d = normalize_path(tempdir())
  (relative_path(d, d) %==% '.')
})

assert('relative_path() with .. works across sibling directories', {
  d = tempfile(); dir.create(d)
  a = file.path(d, 'a'); dir.create(a)
  b = file.path(d, 'b'); dir.create(b)
  f = file.path(a, 'file.txt'); file.create(f)
  res = relative_path(f, b)
  (grepl('\\.\\.', res))
  unlink(d, recursive = TRUE)
})

assert('R_logo() finds an existing logo', {
  logo = R_logo()
  (length(logo) %==% 1L)
  (file.exists(logo))
})

assert('all_files() lists files recursively', {
  d = tempfile(); dir.create(d)
  sub = file.path(d, 'sub'); dir.create(sub)
  file.create(file.path(d, 'a.txt'))
  file.create(file.path(sub, 'b.R'))
  res = all_files('[.]R$', dir = d)
  (length(res) %==% 1L)
  (grepl('b.R$', res))
  unlink(d, recursive = TRUE)
})

assert('magic_path() finds a file recursively in subdirs', {
  d = tempfile(); dir.create(d)
  sub = file.path(d, 'a', 'b'); dir.create(sub, recursive = TRUE)
  f = file.path(sub, 'target.txt'); writeLines('found', f)
  res = magic_path('target.txt', root = d, relative = FALSE)
  (same_path(res, f))
  unlink(d, recursive = TRUE)
})

assert('magic_path() errors when file not found and error = TRUE', {
  d = tempfile(); dir.create(d)
  (has_error(magic_path('nonexistent.txt', root = d, error = TRUE)))
  unlink(d, recursive = TRUE)
})

assert('magic_path() returns input path when not found and error = FALSE', {
  d = tempfile(); dir.create(d)
  (magic_path('nonexistent.txt', root = d, error = FALSE) %==% 'nonexistent.txt')
  unlink(d, recursive = TRUE)
})

assert('magic_path() errors or warns on multiple matches', {
  d = tempfile(); dir.create(d)
  sub1 = file.path(d, 'a'); dir.create(sub1)
  sub2 = file.path(d, 'b'); dir.create(sub2)
  file.create(file.path(sub1, 'dup.txt'))
  file.create(file.path(sub2, 'dup.txt'))
  (has_error(magic_path('dup.txt', root = d, error = TRUE)))
  res = suppressMessages(magic_path('dup.txt', root = d, error = FALSE))
  (file.exists(res))
  unlink(d, recursive = TRUE)
})

assert('from_root() errors when root is NULL', {
  (has_error(from_root('foo.R', root = NULL)))
})

assert('from_root() returns relative path from project root', {
  d = tempfile(); dir.create(d)
  writeLines('Package: testpkg', file.path(d, 'DESCRIPTION'))
  sub = file.path(d, 'R'); dir.create(sub)
  file.create(file.path(sub, 'code.R'))
  owd = setwd(sub)
  res = from_root('R', 'code.R', root = d)
  (res %==% 'code.R')
  setwd(owd)
  unlink(d, recursive = TRUE)
})
