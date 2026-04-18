#' Print a web page to PDF/PNG/JPEG
#'
#' Print a web page to PDF or take a screenshot to PNG/JPEG via a headless
#' browser such as Chromium or Google Chrome.
#' @param input Path or URL to the HTML page to be printed.
#' @param output An output filename. If only an extension is provided, the
#'   filename will be inferred from `url` via [url_filename()]. Only `.pdf`,
#'   `.png`, and `.jpeg` are supported.
#' @param args Command-line arguments to be passed to the headless browser. The
#'   default arguments can be found in `xfun:::browser_args()`. You may pass
#'   additional arguments on top of these via, e.g., `c('default',
#'   '--no-pdf-header-footer')`, or completely override the default arguments by
#'   providing a vector of other arguments.
#' @param window_size The browser window size when taking a PNG/JPEG screenshot.
#'   Ignored when printing to PDF.
#' @param browser Path to the web browser. By default, the browser is found via
#'   `xfun:::find_browser()`. If it cannot be found, you may set the global
#'   option `options(xfun.browser = )` or environment variable `R_XFUN_BROWSER`
#'   to the path of the browser, or simply pass the path to the `browser`
#'   argument.
#' @return The `output` path if the web page is successfully printed.
#' @export
#' @examplesIf interactive()
#' xfun::browser_print('https://www.r-project.org')
browser_print = function(
  input, output = '.pdf', args = 'default', window_size = c(1280, 1024),
  browser = env_option('xfun.browser', find_browser())
) {
  if (!file.exists(browser)) browser = Sys.which(browser)
  if (!utils::file_test('-x', browser)) stop('The browser is not executable: ', browser)
  if (sans_ext(output) == '') output = with_ext(url_filename(input), output)
  to_pdf = tolower(file_ext(output)) == 'pdf'
  if (!to_pdf && !grepl('--window-size=', args))
    args = c(args, paste0('--window-size=', paste(window_size, collapse = ',')))
  if ('default' %in% args) {
    args = setdiff(c(
      args, browser_args(),
      sprintf('--%s="%s"', if (to_pdf) 'print-to-pdf' else 'screenshot', normalize_path(output)),
      shQuote(input)
    ), 'default')
  }
  if (system2(browser, args, stderr = FALSE) != 0) stop('Failed to print to ', output)
  output
}

browser_args = function() c(
  proxy_args(), if (is_windows()) '--no-sandbox', '--headless',
  '--no-first-run', '--no-default-browser-check', '--hide-scrollbars'
)

find_browser = function() {
  switch(
    .Platform$OS.type,
    windows = {
      res = unlist(lapply(c('ChromeHTML', 'MSEdgeHTM'), function(x) {
        res = tryCatch({
          unlist(utils::readRegistry(sprintf('%s\\shell\\open\\command', x), 'HCR'))
        }, error = function(e) '')
        res = unlist(strsplit(res, '"'))
        res = head(res[file.exists(res)], 1)
      }))
      if (length(res) == 0) stop(
        'Cannot find Google Chrome or Edge automatically from the Windows Registry Hive. ',
        "Please pass the full path of chrome.exe or msedge.exe to the 'browser' argument ",
        "or to the environment variable 'R_XFUN_BROWSER'."
      )
      res[1]
    },
    unix = {
      for (i in c(
        '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
        'google-chrome', 'chromium-browser', 'chromium', 'google-chrome-stable')) {
        if ((res <- Sys.which(i)) != '') break
      }
      if (res == '') stop('Cannot find Chromium or Google Chrome')
      res
    },
    stop('Your platform is not supported')
  )
}

proxy_args = function() {
  x = Sys.getenv(c('https_proxy', 'HTTPS_PROXY', 'http_proxy', 'HTTP_PROXY'))
  x = x[x != '']
  if (length(x) == 0) return()
  c(
    paste0('--proxy-server=', x[1]),
    paste0('--proxy-bypass-list=', paste(no_proxy(), collapse = ';'))
  )
}

no_proxy = function() {
  x = do.call(c, strsplit(Sys.getenv(c('no_proxy', 'NO_PROXY')), '[,;]'))
  x = c('localhost', '127.0.0.1', x)
  unique(x)
}
