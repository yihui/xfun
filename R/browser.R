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
#'   '--force-device-scale-factor=2')`, or completely override the default
#'   arguments by providing a vector of other arguments.
#' @param window_size The browser window size when taking a PNG/JPEG screenshot.
#'   Ignored when printing to PDF.
#' @param browser Path to the web browser. By default, the browser is found via
#'   `xfun:::find_browser()`. If it cannot be found, you may set the global
#'   option `options(xfun.browser = )` or environment variable `R_XFUN_BROWSER`
#'   to the path of the browser, or simply pass the path to the `browser`
#'   argument.
#' @param query A named list (or vector) of query parameters to be appended to
#'   the URL of the page, e.g., `list(paged = 1)`. If `input` is a file path, it
#'   will be converted to a `file://` URL first.
#' @param wait A JavaScript expression that must be true before the page is
#'   printed (or its DOM is dumped), e.g., `"window.ready"`. It is evaluated
#'   repeatedly until it returns a true value or the time is out. If it returns
#'   a [Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise),
#'   the page is ready when the promise is resolved (regardless of its value).
#'   The page is always waited for until it is loaded.
#'   The command-line flags of the browser can only wait for a fixed time, so
#'   `query` and `wait` make the function control the browser via the Chrome
#'   DevTools Protocol instead, which is more reliable for pages that update
#'   themselves after they are loaded.
#' @param timeout The maximum number of seconds to wait for the browser and the
#'   page (when `query` or `wait` is used).
#' @return The `output` path if the web page is successfully printed.
#' @export
#' @examplesIf interactive()
#' xfun::browser_print("https://www.r-project.org")
#'
#' # wait for a promise that is resolved when the page is ready
#' # xfun::browser_print("index.html", wait = "window.pageReady")
browser_print = function(
  input, output = '.pdf', args = 'default', window_size = c(1280, 1024),
  browser = NULL, query = NULL, wait = NULL, timeout = 60
) {
  browser = check_browser(browser)
  if (sans_ext(output) == '') output = with_ext(url_filename(input), output)
  to_pdf = tolower(file_ext(output)) == 'pdf'
  cdp = length(query) || !is.null(wait)
  if (!to_pdf && !any(grepl('--window-size=', args)))
    args = c(args, paste0('--window-size=', paste(window_size, collapse = ',')))
  if ('default' %in% args) {
    args = setdiff(c(
      args, browser_args(),
      # Chromium otherwise stamps the date, page title, and file path into the
      # PDF's header and footer, which is just noise for programmatic printing.
      # (this flag puts the browser in the print-only mode, so not for DevTools)
      if (to_pdf && !cdp) '--no-pdf-header-footer'
    ), 'default')
  }
  if (cdp) {
    type = switch(tolower(file_ext(output)), pdf = 'pdf', jpeg = , jpg = 'jpeg', 'png')
    x = browser_cdp(
      browser, args, browser_url(input, query), wait, timeout,
      if (!to_pdf) window_size, type
    )
    writeBin(x, output)
    return(output)
  }
  args = c(args, sprintf(
    '--%s="%s"', if (to_pdf) 'print-to-pdf' else 'screenshot', normalize_path(output)
  ), shQuote(input))
  run_browser(browser, args, paste('Failed to print to', output))
  output
}

#' Execute JavaScript on an HTML page via a headless browser and dump the DOM
#'
#' Load an HTML page in a headless browser (Chromium/Chrome/Edge), execute all
#' JavaScript on it, and return (or save) the rendered DOM as HTML.
#' @param input Path or URL to the HTML page.
#' @param output An optional output file path. If not provided, the rendered
#'   HTML is returned as a character string.
#' @param fragment Whether to return only the inner body content instead of the
#'   full HTML document.
#' @inheritParams browser_print
#' @return If `output` is `NULL`, the rendered HTML as a character string;
#'   otherwise the `output` path (invisibly).
#' @export
#' @examplesIf interactive()
#' f = tempfile(fileext = ".html")
#' writeLines(c(
#'   '<html><body><div id="out"></div>',
#'   '<script>document.getElementById("out").textContent = "hello";</script>',
#'   '</body></html>'
#' ), f)
#' xfun::browser_dom(f)
browser_dom = function(
  input, output = NULL, fragment = FALSE, browser = NULL, query = NULL,
  wait = NULL, timeout = 60
) {
  browser = check_browser(browser)
  html = if (length(query) || !is.null(wait)) {
    browser_cdp(
      browser, browser_args(), browser_url(input, query), wait, timeout, NULL, 'dom'
    )
  } else {
    args = c(browser_args(), '--dump-dom', '--log-level=3', input)
    one_string(run_browser(browser, args, 'Failed to dump DOM'))
  }
  html = gsub('\\s*<script[^>]*>(?s).*?</script>\\s*', '', html, perl = TRUE)
  if (fragment)
    html = gsub('^(?s).*?<body[^>]*>\\s*|\\s*</body>(?s).*$', '', html, perl = TRUE)
  if (is.null(output)) raw_string(html) else write_utf8(html, output)
}

# Evaluate `expr` (a headless-browser command) with the browser pointed at a
# private, disposable temp directory. Chromium scatters scratch files (e.g.
# 'com.google.Chrome.*') into TMPDIR and does not always remove them when it
# exits; confining them to a directory we delete afterward keeps the temp dir
# clean (e.g. so R CMD check does not report detritus).
with_browser_tmp = function(expr) {
  d = tempfile('browser-'); dir.create(d)
  old = set_envvar(c(TMPDIR = d))
  on.exit({ set_envvar(old); unlink(d, recursive = TRUE, force = TRUE) }, add = TRUE)
  expr
}

# Run the browser; on failure, stop() with its exit status, version, and
# stderr (otherwise lost), which is key to diagnosing remote failures.
run_browser = function(browser, args, msg) {
  err = tempfile(); on.exit(unlink(err))
  out = with_browser_tmp(system2(browser, args, stdout = TRUE, stderr = err))
  if (is.null(status <- attr2(out, 'status'))) return(out)
  browser_fail(browser, msg, err, status)
}

# Chromium logs repeat a lot, so collapse lines that differ only in the
# '[pid:tid:...]' prefix and keep the last 30.
browser_fail = function(browser, msg, err, status = NULL) {
  ver = tryCatch(
    system2(browser, '--version', stdout = TRUE, stderr = TRUE),
    error = function(e) ''
  )
  x = if (file.exists(err)) read_utf8(err) else character()
  k = sub('^\\[[^]]*\\]\\s*', '', x)
  n = table(k)[unique(k)]
  x = sprintf('%s%s', x[!duplicated(k)], ifelse(n > 1, sprintf(' (x%d)', n), ''))
  stop2(one_string(c(
    if (is.null(status)) paste0(msg, '.') else sprintf('%s (exit status %s).', msg, status),
    '', paste('Browser:', browser, ver), '', tail(x, 30)
  )))
}

check_browser = function(browser) {
  if (is.null(browser)) browser = env_option('xfun.browser', find_browser())
  if (!file.exists(browser)) browser = Sys.which(browser)
  if (!utils::file_test('-x', browser)) stop('The browser is not executable: ', browser)
  browser
}

browser_args = function() c(
  proxy_args(), if (is_windows()) '--no-sandbox', '--headless',
  '--no-first-run', '--no-default-browser-check', '--hide-scrollbars',
  # avoid spawning the component/CRX URL fetcher, which leaves temp files
  # (e.g., 'com.google.Chrome.chromecrx_chrome_url_fetcher_*') behind
  '--disable-background-networking', '--disable-component-update'
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
      app_path = function(app) sprintf('/Applications/%s.app/Contents/MacOS/%s', app, app)
      for (i in c(
        app_path(c('Google Chrome', 'Microsoft Edge')),
        'google-chrome', 'chromium-browser', 'chromium', 'google-chrome-stable',
        'microsoft-edge-stable', 'microsoft-edge')) {
        if ((res <- Sys.which(i)) != '') break
      }
      if (res == '') stop('Cannot find Chromium, Google Chrome, or Edge')
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
    shQuote(paste0('--proxy-bypass-list=', paste(no_proxy(), collapse = ';')))
  )
}

no_proxy = function() {
  x = do.call(c, strsplit(Sys.getenv(c('no_proxy', 'NO_PROXY')), '[,;]'))
  x = c('localhost', '127.0.0.1', x)
  unique(x)
}

