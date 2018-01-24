# CHANGES IN xfun VERSION 0.2 (unreleased)

## NEW FEATURES

- Added a `new_session` argument to `loadable()`.

- Added new functions `gsub_file()`, `gsub_files()`, `gsub_dir()`, and `gsub_ext()` to replace strings in files.

- Added new functions `Rscript` and `Rcmd` as wrappers of `system2('Rscript')` and `system2('R', 'CMD')`, respectively.

- Added a function `install_dir()` to install a source package from a directory.

# CHANGES IN xfun VERSION 0.1

## NEW FEATURES

- `attr()` as an abbreviation of `base::attr(exact = TRUE)`.

- `file_ext()`, `sans_ext()`, and `with_ext()` to manipulate extensions in filenames.

- `in_dir()` to evaluate an R expression in a directory.

- `isFALSE()` as an abbreviation of `identical(x, FALSE)`.

- `is_windows()`, `is_macos()`, `is_linux()`, and `is_unix()` to test operating systems.

- `native_encode()` to try to encode a character vector in the native encoding.

- `normalize_path()` as an abbreviation of `normalizePath(winslash = '/', mustWork = FALSE)`.

- `optipng()` to run the command `optipng` to optimize all PNG files under a directory.

- `parse_only()` parses R code without keeping the source references.

- `pkg_attach()` and `pkg_load()` to attach and load a vector of packages, respectively (and optionally, install the missing packages).

- `read_utf8()` and `write_utf8()` to read and write UTF-8 files, respectively.

- `same_path()` to test if two paths are the same.

- `strict_list()` is a version of `list()` that disables partial matching of the `$` operator.

- `tojson()` is a simple JSON serializer.

- `try_silent()` is an abbreviation of `try(silent = TRUE)`.
