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
  # unset R_TESTS for the new R session: https://stackoverflow.com/a/27994299
  if (is_R_CMD_check()) {
    v = set_envvar(c(R_TESTS = NA)); on.exit(set_envvar(v), add = TRUE)
  }
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
#' @param options A character vector of options to passed to
#'   \code{\link{Rscript}}, e.g., \code{"--vanilla"}.
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
#'
#' # Run Rscript starting a vanilla R session
#' xfun::Rscript_call(factorial, list(10), options = c("--vanilla"))
Rscript_call = function(
  fun, args = list(), options = NULL, ..., wait = TRUE,
  fail = sprintf("Failed to run '%s' in a new R session.", deparse(substitute(fun))[1])
) {
  f = replicate(2, tempfile(fileext = '.rds'))
  on.exit(unlink(if (wait) f else f[2]), add = TRUE)
  saveRDS(list(fun, args), f[1])
  Rscript(
    c(options, shQuote(c(pkg_file('scripts', 'call-fun.R'), f)))
    ,..., wait = wait
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

#' Kill a process and (optionally) all its child processes
#'
#' Run the command \command{taskkill /f /pid} on Windows and \command{kill} on
#' Unix, respectively, to kill a process.
#' @param pid The process ID.
#' @param recursive Whether to kill the child processes of the process.
#' @param ... Arguments to be passed to \code{\link{system2}()} to run the
#'   command to kill the process.
#' @return The status code returned from \code{system2()}.
#' @export
proc_kill = function(pid, recursive = TRUE, ...) {
  if (is_windows()) {
    system2('taskkill', c(if (recursive) '/t', '/f', '/pid', pid), ...)
  } else {
    system2('kill', c(pid, if (recursive) child_pids(pid)), ...)
  }
}

# obtain pids of all child processes (recursively)
child_pids = function(id) {
  x = system2('sh', shQuote(c(pkg_file('scripts', 'child_pids.sh'), id)), stdout = TRUE)
  grep('^[0-9]+$', x, value = TRUE)
}

powershell = function(command) {
  if (Sys.which('powershell') == '') return()
  command = paste(command, collapse = ' ')
  system2('powershell', c('-Command', shQuote(command)), stdout = TRUE)
}

# start a background process via the PowerShell cmdlet and return its pid
ps_process = function(command, args = character(), verbose = FALSE) {
  powershell(c(
    'echo (Start-Process', '-FilePath', shQuote(command), '-ArgumentList',
    ps_quote(args), '-PassThru', '-WindowStyle',
    sprintf('%s).ID', if (verbose) 'Normal' else 'Hidden')
  ))
}

# quote PowerShell arguments properly
ps_quote = function(x) {
  x = gsub('"', '""', x)  # '""' mean a literal '"'
  # if an argument contains a space, surround it with escaped double quotes `"`"
  i = grep('\\s', x)
  x[i] = sprintf('`"%s`"', x[i])
  sprintf('"%s"', paste(x, collapse = ' '))
}

#' Start a background process
#'
#' Start a background process using the PowerShell cmdlet \command{Start-Process
#' -PassThru} on Windows or the ampersand \command{&} on Unix, and return the
#' process ID.
#' @param command,args The system command and its arguments. They do not need to
#'   be quoted, since they will be quoted via \code{\link{shQuote}()}
#'   internally.
#' @param verbose If \code{FALSE}, suppress the output from \verb{stdout} (and
#'   also \verb{stderr} on Windows). The default value of this argument can be
#'   set via a global option, e.g., \code{options(xfun.bg_process.verbose =
#'   TRUE)}.
#' @return The process ID as a character string.
#' @note On Windows, if PowerShell is not available, try to use
#'   \code{\link{system2}(wait = FALSE)} to start the background process
#'   instead. The process ID will be identified from the output of the command
#'   \command{tasklist}. This method of looking for the process ID may not be
#'   reliable. If the search is not successful in 30 seconds, it will throw an
#'   error (timeout). If a longer time is needed, you may set
#'   \code{options(xfun.bg_process.timeout)} to a larger value, but it should be
#'   very rare that a process cannot be started in 30 seconds. When you reach
#'   the timeout, it is more likely that the command actually failed.
#' @export
#' @seealso \code{\link{proc_kill}()} to kill a process.
bg_process = function(
  command, args = character(), verbose = getOption('xfun.bg_process.verbose', FALSE)
) {
  throw_error = function(...) stop(
    'Failed to run the command', ..., ' in the background: ',
    paste(shQuote(c(command, args)), collapse = ' '), call. = FALSE
  )

  # check the possible pid returned from system2()
  check_pid = function(res) {
    if (is.null(res)) return(res)
    if (!is.null(attr(res, 'status'))) throw_error()
    if (length(res) == 1 && grepl('^[0-9]+$', res)) return(res)
    throw_error()
  }

  if (is_windows()) {
    # first try 'Start-Process -PassThrough' to start a background process; if
    # PowerShell is unavailable, fall back to system2(wait = FALSE), and the
    # method to find out the pid is not 100% reliable
    if (length(pid <- check_pid(ps_process(command, args, verbose))) == 1) return(pid)

    message(
      'It seems you do not have PowerShell installed. The process ID may be inaccurate.'
    )
    # format of task list: hugo.exe    4592 Console      1     35,188 K
    tasklist = function() system2('tasklist', stdout = TRUE)
    pid1 = tasklist()
    system2(command, shQuote(args), wait = FALSE)

    get_pid = function() {
      # make sure the command points to an actual executable (e.g., resolve 'R'
      # to 'R.exe')
      if (!file.exists(command)) {
        if (Sys.which(command) != '') command = Sys.which(command)
      }
      cmd = basename(command)

      pid2 = setdiff(tasklist(), pid1)
      # the process's info should start with the command name
      pid2 = pid2[substr(pid2, 1, nchar(cmd)) == cmd]
      if (length(pid2) == 0) return()
      m = regexec('\\s+([0-9]+)\\s+', pid2)
      for (v in regmatches(pid2, m)) if (length(v) >= 2) return(v[2])
    }

    t0 = Sys.time(); id = NULL; timeout = getOption('xfun.bg_process.timeout', 30)
    while (difftime(Sys.time(), t0, units = 'secs') < timeout) {
      if (length(id <- get_pid()) > 0) break
    }

    if (length(id) > 0) return(id)

    system2(command, args, timeout = timeout)  # see what the error is
    throw_error(' in ', timeout, ' second(s)')
  } else {
    pid = tempfile(); on.exit(unlink(pid), add = TRUE)
    code = paste(c(
      shQuote(c(command, args)), if (!verbose) '> /dev/null', '& echo $! >', shQuote(pid)
    ), collapse = ' ')
    system2('sh', c('-c', shQuote(code)))
    return(check_pid(readLines(pid)))
  }
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
