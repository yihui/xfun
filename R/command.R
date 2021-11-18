#' Run \code{system2()} and mark its character output as UTF-8 if appropriate
#'
#' This is a wrapper function based on \code{system2()}. If \code{system2()}
#' returns character output (e.g., with the argument \code{stdout = TRUE}),
#' check if the output is encoded in UTF-8. If it is, mark it with UTF-8
#' explicitly.
#' @param ... Passed to \code{\link{system2}()}.
#' @return The value returned by \code{system2()}.
#' @export
#' @examplesIf interactive()
#' a = shQuote(c('-e', 'print(intToUtf8(c(20320, 22909)))'))
#' x2 = system2('Rscript', a, stdout = TRUE)
#' Encoding(x2)  # unknown
#'
#' x3 = xfun::system3('Rscript', a, stdout = TRUE)
#' # encoding of x3 should be UTF-8 if the current locale is UTF-8
#' !l10n_info()[['UTF-8']] || Encoding(x3) == 'UTF-8'  # should be TRUE
system3 = function(...) {
  res = system2(...)
  if (is.character(res)) {
    if (all(is_utf8(res))) Encoding(res) = 'UTF-8'
  }
  if (is.integer(res) && res == 0) invisible(res) else res
}

#' Run OptiPNG on all PNG files under a directory
#'
#' Call the command \command{optipng} via \code{system2()} to optimize all PNG
#' files under a directory.
#' @param dir Path to a directory.
#' @param files Alternatively, you can choose the specific files to optimize.
#' @param ... Arguments to be passed to \code{system2()}.
#' @references OptiPNG: \url{http://optipng.sourceforge.net}.
#' @export
optipng = function(
  dir = '.', files = list.files(dir, '[.]png$', recursive = TRUE, full.names = TRUE), ...
) {
  if (Sys.which('optipng') != '') for (f in files) system2('optipng', shQuote(f), ...)
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
  if (wait) if (file_exists(f[2])) readRDS(f[2]) else stop(fail, call. = FALSE)
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
  x = system2('sh', shQuote(c(pkg_file('scripts', 'child-pids.sh'), id)), stdout = TRUE)
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
      if (!file_exists(command)) {
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
#' The function \code{upload_ftp()} runs the command \command{curl -T file
#' server} to upload a file to an FTP server if the system command
#' \command{curl} is available, otherwise it uses the R package \pkg{curl}. The
#' function \code{upload_win_builder()} uses \code{upload_ftp()} to upload
#' packages to the win-builder server.
#'
#' These functions were written mainly to save package developers the trouble of
#' going to the win-builder web page and uploading packages there manually.
#' @param file Path to a local file.
#' @param server The address of the FTP server. For \code{upload_win_builder()},
#'   \code{server = 'https'} means uploading to
#'   \code{'https://win-builder.r-project.org/upload.aspx'}.
#' @param dir The remote directory to which the file should be uploaded.
#' @param version The R version(s) on win-builder.
#' @return Status code returned from \code{\link{system2}()} or
#'   \code{curl::curl_fetch_memory()}.
#' @export
upload_ftp = function(file, server, dir = '') {
  if (dir != '') dir = gsub('/*$', '/', dir)
  server = paste0(server, dir)
  if (Sys.which('curl') == '') {
    curl::curl_upload(file, server)$status_code
  } else {
    system2('curl', shQuote(c('-T', file, server)))
  }
}

#' @param solaris Whether to also upload the package to the Rhub server to check
#'   it on Solaris.
#' @rdname upload_ftp
#' @export
upload_win_builder = function(
  file = pkg_build(), version = c("R-devel", "R-release", "R-oldrelease"),
  server = c('ftp', 'https'), solaris = pkg_available('rhub')
) {
  if (missing(file)) on.exit(file.remove(file), add = TRUE)
  if (system2('git', 'status', stderr = FALSE) == 0) system2('git', 'pull')
  server = server[1]
  server = switch(
    server,
    'ftp'   = paste0(server, '://win-builder.r-project.org/'),
    'https' = paste0(server, '://win-builder.r-project.org/upload.aspx'),
    server
  )
  res = if (grepl('^ftp://', server)) {
    lapply(version, upload_ftp, file = file, server = server)
  } else {
    vers = c('R-devel' = 2, 'R-release' = 1, 'R-oldrelease' = 3)
    params = list(
      FileUpload = file,
      Button = 'Upload File',
      # perhaps we should read these tokens dynamically from
      # https://win-builder.r-project.org/upload.aspx
      `__VIEWSTATE` = '/wEPDwULLTE0OTY5NTg0MTUPZBYCAgIPFgIeB2VuY3R5cGUFE211bHRpcGFydC9mb3JtLWRhdGFkZFHMrNH6JjHTyJ00T0dAADGf4oa0',
      `__VIEWSTATEGENERATOR` = '69164837',
      `__EVENTVALIDATION` = '/wEWBQKksYbrBgKM54rGBgK7q7GGCAKF2fXbAwLWlM+bAqR2dARbCNfKVu0vDawqWYgB5kKI'
    )
    lapply(version, function(i) {
      names(params)[1:2] = paste0(names(params)[1:2], vers[i])
      if (Sys.which('curl') == '') {
        h = curl::new_handle()
        params[[1]] = curl::form_file(params[[1]])
        curl::handle_setform(h, .list = params)
        curl::curl_fetch_memory(server, h)$status_code
      } else {
        params[1] = paste0('@', params[1])
        system2('curl', shQuote(c(
          rbind('-F', paste(names(params), params, sep = '=')),
          server
        )), stdout = FALSE)
      }
    })
  }

  if (solaris) rhub::check_on_solaris(
    file, check_args = '--no-manual', show_status = FALSE,
    env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = 'false')
  )

  setNames(unlist(res), version)
}
