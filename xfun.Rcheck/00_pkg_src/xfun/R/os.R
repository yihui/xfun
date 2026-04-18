#' Test for types of operating systems
#'
#' Functions based on `.Platform$OS.type` and `Sys.info()` to test if
#' the current operating system is Windows, macOS, Unix, or Linux.
#' @rdname os
#' @export
#' @examples
#' library(xfun)
#' # only one of the following statements should be true
#' is_windows()
#' is_unix() && is_macos()
#' is_linux()
#' # In newer Macs, CPU can be either Intel or Apple
#' is_arm64()  # TRUE on Apple silicone machines
is_windows = function() .Platform$OS.type == 'windows'

#' @rdname os
#' @export
is_unix = function() .Platform$OS.type == 'unix'

#' @rdname os
#' @export
is_macos = function() unname(Sys.info()['sysname'] == 'Darwin')

#' @rdname os
#' @export
is_linux = function() unname(Sys.info()['sysname'] == 'Linux')

#' @rdname os
#' @export
is_arm64 = function() Sys.info()[['machine']] %in% c('arm64', 'aarch64')

# open a file (optionally in an editor) or directory
open_path = function(x, edit = !dir_exists(x), ...) {
  if (edit) {
    tryCatch(rstudioapi::navigateToFile(x, ...), error = function(e) file.edit(x))
  } else {
    if (is_windows()) shell.exec(x) else {
      system2(if (is_macos()) 'open' else 'xdg-open', shQuote(x))
    }
  }
}
