#' Test for types of operating systems
#'
#' Functions based on \code{.Platform$OS.type} and \code{Sys.info()} to test if
#' the current operating system is Windows, macOS, Unix, or Linux.
#' @rdname os
#' @export
#' @examples
#' library(xfun)
#' # only one of the following statements should be true
#' is_windows()
#' is_unix() && is_macos()
#' is_linux()
is_windows = function() .Platform$OS.type == 'windows'

#' @rdname os
#' @export
is_unix = function() .Platform$OS.type == 'unix'

#' @rdname os
#' @export
is_macos = function() Sys.info()['sysname'] == 'Darwin'

#' @rdname os
#' @export
is_linux = function() Sys.info()['sysname'] == 'Linux'
