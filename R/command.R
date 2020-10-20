#' Run OptiPNG on all PNG files under a directory
#'
#' Calls the command \command{optipng} to optimize all PNG files under a
#' directory.
#' @param dir Path to a directory.
#' @references OptiPNG: \url{http://optipng.sourceforge.net}.
#' @export
optipng = function(dir = '.') {
  files = list.files(dir, '[.]png$', recursive = TRUE, full.names = TRUE)
  for (f in files) system2('optipng', shQuote(f))
}

#' Run the commands \command{Rscript} and \command{R CMD}
#'
#' Wrapper functions to run the commands \command{Rscript} and \command{R CMD}.
#' @param args A character vector of command-line arguments.
#' @param ... Other arguments to be passed to \code{\link{system2}()}.
#' @export
#' @return A value returned by \code{system2()}.
#' @examples library(xfun)
#' Rscript(c('-e', '1+1'))
#' Rcmd(c('build', '--help'))
Rscript = function(args, ...) {
  system2(file.path(R.home('bin'), 'Rscript'), args, ...)
}

#' @rdname Rscript
#' @export
Rcmd = function(args, ...) {
  system2(file.path(R.home('bin'), 'R'), c('CMD', args), ...)
}

#' Call a function in a new R session via \code{Rscript()}
#'
#' Save the argument values of a function in a temporary RDS file, open a new R
#' session via \code{\link{Rscript}()}, read the argument values, call the
#' function, and read the returned value back to the current R session.
#' @param fun A function, or a character string that can be parsed and evaluated
#'   to a function.
#' @param args A list of argument values.
#' @param ...,wait Arguments to be passed to \code{\link{system2}()}.
#' @param fail The desired error message when an error occurred in calling the
#'   function.
#' @export
#' @return The returned value of the function in the new R session.
#' @examples factorial(10)
#' # should return the same value
#' xfun::Rscript_call('factorial', list(10))
#'
#' # the first argument can be either a character string or a function
#' xfun::Rscript_call(factorial, list(10))
Rscript_call = function(
  fun, args = list(), ..., wait = TRUE,
  fail = sprintf("Failed to run '%s' in a new R session.", deparse(substitute(fun))[1])
) {
  f = replicate(2, tempfile(fileext = '.rds'))
  on.exit(unlink(if (wait) f else f[2]), add = TRUE)
  saveRDS(list(fun, args), f[1])
  Rscript(
    shQuote(c(system.file('scripts', 'call-fun.R', package = 'xfun'), f)),
    ..., wait = wait
  )
  if (wait) if (file.exists(f[2])) readRDS(f[2]) else stop(fail, call. = FALSE)
}

# call a function in a background process
Rscript_bg = function(fun, args = list(), timeout = 10) {
  pid = tempfile()  # to store the process ID of the new R session
  saveRDS(NULL, pid)

  Rscript_call(function() {
    saveRDS(Sys.getpid(), pid)
    # remove this pid file when the function finishes
    on.exit(unlink(pid), add = TRUE)
    do.call(fun, args)
  }, wait = FALSE)

  id = NULL  # read the above process ID into this R session
  res = list(pid = id, is_alive = function() FALSE)

  # check if the pid file still exists; if not, the process has ended
  if (!file_exists(pid)) return(res)

  t0 = Sys.time()
  while (difftime(Sys.time(), t0, units = 'secs') < timeout) {
    Sys.sleep(.1)
    if (!file_exists(pid)) return(res)
    if (length(id <- readRDS(pid)) == 1) break
  }
  if (length(id) == 0) stop(
    'Failed to launch the background process in ', timeout, ' seconds (timeout).'
  )

  list(pid = id, is_alive = function() file_exists(pid))
}

powershell = function(command) {
  if (Sys.which('powershell') == '') return()
  system2('powershell', c('-Command', shQuote(command)), stdout = TRUE)
}

# start a background process, and return its process ID
bg_process = function(command, args = character(), timeout = 30) {
  id = NULL

  if (is_windows()) {
    # format of task list: hugo.exe    4592 Console      1     35,188 K
    tasklist = function() system2('tasklist', stdout = TRUE)
    pid1 = tasklist()
    system2(command, args, wait = FALSE)

    get_pid = function(time) {
      # make sure the command points to an actual executable (e.g., resolve 'R'
      # to 'R.exe')
      if (!file.exists(command)) {
        if (Sys.which(command) != '') command = Sys.which(command)
      }
      cmd = basename(command)

      # use PowerShell to figure out the PID if possible:
      res = powershell(sprintf(
        'Get-CimInstance Win32_Process -Filter "name = \'%s\'" | select CommandLine, ProcessId | ConvertTo-Csv', cmd
      ))
      if (length(res) > 1) {
        res = read.csv(text = res, comment.char = '#', stringsAsFactors = FALSE)
        if (length(r1 <- res[, 'CommandLine']) && length(r2 <- res[, 'ProcessId'])) {
          cmd2 = paste(c(cmd, args), collapse = ' ')
          r2 = r2[grep(cmd2, r1, fixed = TRUE)]
          if (length(r2)) return(r2)
        }
      }

      # don't try this method until 1/5 of timeout has passed
      if (!is.null(res) && time < timeout/5) return()
      pid2 = setdiff(tasklist(), pid1)
      # the process's info should start with the command name
      pid2 = pid2[substr(pid2, 1, nchar(cmd)) == cmd]
      if (length(pid2) == 0) return()
      m = regexec('\\s+([0-9]+)\\s+', pid2)
      for (v in regmatches(pid2, m)) if (length(v) >= 2) return(v[2])
    }
  } else {
    pid = tempfile(); on.exit(unlink(pid), add = TRUE)
    code = paste(c(
      shQuote(c(command, args)),
      if (!getOption('xfun.bg_process.verbose', FALSE)) '> /dev/null',
      '& echo $! >', shQuote(pid)
    ), collapse = ' ')
    system2('sh', c('-c', shQuote(code)))
    get_pid = function(time) {
      if (file.exists(pid)) readLines(pid)
    }
  }

  t0 = Sys.time()
  while ((time <- difftime(Sys.time(), t0, units = 'secs')) < timeout) {
    if (length(id <- get_pid(time)) == 1) break
  }

  if (length(id) == 1) return(id)

  system2(command, args, timeout = timeout)  # see what the error is
  stop(
    'Failed to run the command in ', timeout, ' seconds (timeout): ',
    paste(shQuote(c(command, args)), collapse = ' ')
  )
}

#' Upload to an FTP server via \command{curl}
#'
#' Run the command \command{curl -T file server} to upload a file to an FTP
#' server. These functions require the system package (\emph{not the R package})
#' \command{curl} to be installed (which should be available on macOS by
#' default). The function \code{upload_win_builder()} uses \code{upload_ftp()}
#' to upload packages to the win-builder server.
#'
#' These functions were written mainly to save package developers the trouble of
#' going to the win-builder web page and uploading packages there manually. You
#' may also consider using \code{devtools::check_win_*}, which currently only
#' allows you to upload a package to one folder on win-builder each time, and
#' \code{xfun::upload_win_builder()} uploads to all three folders, which is more
#' likely to be what you need.
#' @param file Path to a local file.
#' @param server The address of the FTP server.
#' @param dir The remote directory to which the file should be uploaded.
#' @param version The R version(s) on win-builder.
#' @return Status code returned from \code{\link{system2}}.
#' @export
upload_ftp = function(file, server, dir = '') {
  if (dir != '') dir = gsub('/*$', '/', dir)
  system2('curl', shQuote(c('-T', file, paste0(server, dir))))
}

#' @rdname upload_ftp
#' @export
upload_win_builder = function(
  file, version = c("R-devel", "R-release", "R-oldrelease"),
  server = 'ftp://win-builder.r-project.org/'
) {
  res = unlist(lapply(version, upload_ftp, file = file, server = server))
  setNames(res, version)
}
