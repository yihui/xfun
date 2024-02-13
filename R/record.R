#' Run R code and record the results
#'
#' Run R code and capture various types of output, including text output, plots,
#' messages, warnings, and errors.
#' @param code A character vector of R source code.
#' @param dev A graphics device. It can be a function name, a function, or a
#'   character string that can be evaluated to a function to open a graphics
#'   device.
#' @param dev.path A base file path for plots (by default, a temporary path
#'   under the current working directory). Actual plot filenames will be this
#'   base path plus incremental suffixes. For example, if `dev.path = "foo"`,
#'   the plot files will be `foo-1.png`, `foo-2.png`, and so on. If `dev.path`
#'   is not character (e.g., `FALSE`), plots will not be recorded.
#' @param dev.ext The file extension for plot files. By default, it will be
#'   inferred from the first argument of the device function if possible.
#' @param dev.args Extra arguments to be passed to the device. The default
#'   arguments are `list(units = 'in', onefile = FALSE, width = 7, height = 7,
#'   res = 96)`. If any of these arguments is not present in the device
#'   function, it will be dropped.
#' @param error Whether to record errors. If `TRUE`, errors will not stop the
#'   execution and error messages will be recorded. If `FALSE`, errors will be
#'   thrown normally.
#' @param verbose `2` means to always print the value of each expression in the
#'   code, no matter if the value is [invisible()] or not; `1` means to always
#'   print the value of the last expression; `0` means no special handling
#'   (i.e., print only when the value is visible).
#' @param envir An environment in which the code is evaluated.
#' @return `record()` returns a list of the class `xfun_record_results` that
#'   contains elements with these possible classes: `record_source` (source
#'   code), `record_output` (text output), `record_plot` (plot file paths),
#'   `record_message` (messages), `record_warning` (warnings), and
#'   `record_error` (errors, only when the argument `error = TRUE`).
#' @import grDevices
#' @export
#' @examples
#' code = c('# a message test', '1:2 + 1:3', 'par(mar = c(4, 4, 1, .2))', 'barplot(5:1, col = 2:6, horiz = TRUE)', 'head(iris)', "sunflowerplot(iris[, 3:4], seg.col = 'purple')", "if (TRUE) {\n  message('Hello, xfun::record()!')\n}", '# throw an error', "1 + 'a'")
#' res = xfun::record(code, dev.args = list(width = 9, height = 6.75), error = TRUE)
#' xfun::tree(res)
#' format(res)
#' # find and clean up plot files
#' plots = Filter(function(x) inherits(x, 'record_plot'), res)
#' file.remove(unlist(plots))
record = function(
    code = NULL, dev = 'png', dev.path = tempfile('record-', '.'),
    dev.ext = dev_ext(dev), dev.args = list(), error = FALSE,
    verbose = getOption('xfun.record.verbose', 0), envir = parent.frame()
) {
  new_record = function(x = list()) structure(x, class = 'xfun_record_results')
  res = new_record()
  if (length(code) == 0) return(res)
  code = split_lines(code)

  add_result = function(x, type, pos = length(res), insert = TRUE) {
    # insert a whole element or append to an existing element in res
    if (!insert) x = c(res[[pos]], x)
    el = structure(x, class = paste0('record_', type))
    N = length(res)
    if (insert) {
      if (N == pos) res[[N + 1]] <<- el else res <<- append(res, el, pos)
    } else {
      res[[pos]] <<- el
    }
  }

  # dev.off() will set the current device to next instead of previous device,
  # but we often want to restore to the previous instead
  dev_cur = dev.cur()
  dev_reset = function() if (dev_cur != 1 && dev_cur %in% dev.list()) dev.set(dev_cur)

  # look for all possible ${dev.path}-%d.${dev.ext} files created by the device
  get_plots = function() {
    files = sprintf(dev.path, seq_along(list.files(dirname(dev.path))))
    files[file_exists(files)]
  }
  # open a graphics device
  dev_num = if (is.character(dev.path)) {
    if (!is.function(dev)) dev = tryCatch(match.fun(dev), error = function(e) {
      eval(parse(text = dev), envir = envir)
    })
    # normalize \ to / and remove the leading ./
    if (is_windows()) dev.path = gsubf('\\', '/', dev.path)
    dev.path = sub('^[.]/+', '', dev.path)
    # add extension
    dev.path = with_ext(paste0(dev.path, if (!grepl('/$', dev.path)) '-', '%d'), dev.ext)
    dir_create(dirname(dev.path))
    # clean up existing plots before opening the device
    if (any(i <- !file.remove(old_plots <- get_plots()))) stop(
      'Failed to delete existing plot file(s): ',
      paste("'", old_plots[i], "'", collapse = ', ')
    )
    dev_open(dev, dev.path, dev.args)
  }
  dev_old = dev.list()  # keep track of current devices
  dev_off = function() {
    if (length(dev_num) && dev_num %in% dev.list()) dev.off(dev_num)
    dev_reset()
    dev_num <<- NULL  # prevent dev.off() again by accident
  }
  on.exit(dev_off(), add = TRUE)

  # check if new plots are generated
  handle_plot = local({
    old_files = NULL  # previously existing plots
    old_plot = recordPlot()
    function(last = FALSE) {
      # if dev.list() has changed, no longer record graphics, except for the last plot
      if (!last) {
        if (!(length(dev_num) && identical(dev.list(), dev_old))) return()
        dev.set(dev_num)
      }
      files = get_plots()
      # on Windows, an empty plot file is created upon opening a device
      files = files[file.size(files) > 0]

      if (!last) {
        new_plot = recordPlot()
        if (!identical(old_plot, new_plot)) {
          # add a placeholder for new plots, which may not have been created by
          # the device until the next new plot is drawn
          add_result(character(), 'plot')
          old_plot <<- new_plot
        }
      }

      if (length(files) == 0) return()

      plots = setdiff(files, old_files)
      old_files <<- files
      if ((n <- length(plots)) == 0) return()

      # indices of plots in results
      i = which(vapply(res, inherits, logical(1), 'record_plot'))
      N = length(i)
      # the last plot should always be appended the last plot block
      if (last) {
        add_result(plots, 'plot', i[N], FALSE)
        return()
      }

      # for the newly generated plots, append the first one (which should be the
      # last plot of a previous code chunk, since it will be created only when
      # the next code chunk produces a new plot) to the previous plot block; the
      # rest are for the current block
      if (N > 1) {
        add_result(plots[1], 'plot', i[N - 1], FALSE)
        if (n > 1) add_result(plots[2:n], 'plot', i[N], FALSE)
      } else {
        # if there exists only one plot block, add plots to that block
        add_result(plots, 'plot', i[N], FALSE)
      }
    }
  })

  handle = if (error) try_silent else identity
  # split code into individual expressions
  codes = handle(split_source(code, merge_comments = TRUE, line_number = TRUE))
  # code may contain syntax errors
  if (is_error(codes)) {
    add_result(code, 'source'); add_result(attr(codes, 'condition')$message, 'error')
    return(new_record(res))
  }

  handle_message = function(type) {
    mf = sub('^(.)', 'muffle\\U\\1', type, perl = TRUE)
    function(e) {
      add_result(e$message, type)
      if (type %in% c('message', 'warning')) invokeRestart(mf)
    }
  }
  handle_m = handle_message('message')
  handle_w = handle_message('warning')
  handle_e = handle_message('error')

  n = length(codes)
  for (i in seq_len(n)) {
    add_result(code <- codes[[i]], 'source')
    expr = parse_only(code)
    if (length(expr) == 0) next
    # verbose = 1: always print the last value; verbose = 2: print all values
    if (verbose == 2 || (verbose == 1 && i == n)) {
      expr = parse_only(c('(', code, ')'))
    }
    # TODO: replace capture.output() with a custom version of sink() +
    # withVisible() so we can support a custom printing function like knit_print()
    out = handle(withCallingHandlers(
      capture.output(eval(expr, envir)),
      message = handle_m, warning = handle_w, error = handle_e
    ))
    if (length(out) && !is_error(out)) add_result(out, 'output')
    handle_plot()
  }
  # shut off the device to write out the last plot if there exists one
  dev_off()
  handle_plot(TRUE)

  # remove empty blocks
  res = Filter(length, res)
  # merge neighbor elements of the same class
  if (length(res) > 1) {
    k = NULL
    for (i in seq_along(res)) {
      if (i == 1) next
      r1 = res[[i - 1]]; c1 = class(r1); r2 = res[[i]]; c2 = class(r2)
      if (!identical(c1, c2)) next
      res[[i]] = c(r1, r2)
      attributes(res[[i]]) = attributes(r1)
      k = c(k, i - 1)
    }
    if (length(k)) res = res[-k]
  }

  new_record(res)
}

dev_open = function(dev, file, args) {
  m = names(formals(dev))
  a = list(units = 'in', onefile = FALSE, width = 8, height = 8, res = 84)
  for (i in names(a)) {
    if (i %in% m && is.null(args[[i]])) args[[i]] = a[[i]]
  }
  do.call(dev, c(list(file), args))
  dev.control('enable')
  dev.cur()
}

# infer the filename extension from a device's first argument
dev_ext = function(dev) {
  # the first argument could be a string or an expression
  if (!is.character(name <- formals(dev)[[1]])) name = gsub('"', '', deparse(name))
  file_ext(name)
}

is_error = function(x) inherits(x, 'try-error')

#' @param to The output format (text or html).
#' @param encode For HTML output, whether to base64 encode plots.
#' @param template For HTML output, whether to embed the formatted results in an
#'   HTML template. Alternatively, this argument can take a file path, i.e.,
#'   path to an HTML template that contains the variable `$body$`. If `TRUE`,
#'   the default template in this package will be used
#'   (`xfun:::pkg_file('resources', 'record.html')`).
#' @rdname record
#' @exportS3Method
#' @return The `format()` method returns a character vector of plain-text output
#'   or HTML code for displaying the results.
format.xfun_record_results = function(
    x, to = c('text', 'html'), encode = FALSE, template = FALSE, ...
) {
  if (to[1] == 'text') {
    res = unlist(lapply(x, function(z) {
      if (!inherits(z, 'record_source')) z = paste('#>', z)
      gsub('\n*$', '\n', one_string(z))
    }))
    return(raw_string(res))
  }
  res = unlist(lapply(x, function(z) {
    cls = sub('^record_', '', class(z))
    if (cls == 'plot') {
      sprintf(
        '<p class="%s"><img src="%s" alt="A plot recorded by xfun::record()" /></p>',
        cls, if (encode) vapply(z, base64_uri, '') else URLencode(z)
      )
    } else {
      paste0(
        sprintf(
          '<pre class="%s"><code>',
          if (cls == 'source') paste0('language-r" data-start="', attr(z, 'line_start')) else cls
        ),
        escape_html(one_string(z)), '</code></pre>'
      )
    }
  }))
  if (isTRUE(template)) template = pkg_file('resources', 'record.html')
  if (is.character(template)) {
    res = sub('$body$', one_string(res), read_utf8(template), fixed = TRUE)
    if (length(x) > 0) res = sub('$title$', make_title(x[[1]]), res, fixed = TRUE)
  }
  raw_string(res)
}

# generate a title from the first line of a character vector
make_title = function(x) {
  if (length(x) == 0) return('')
  x = gsub('^#+\\s*', '', x[1])  # remove possible comment chars
  x = gsub('\\s*[.]*$', '... | ', x)  # add ... to the end
  escape_html(x)
}

#' @param x An object returned by `record()`.
#' @param browse Whether to browse the results on an HTML page.
#' @param ... Currently ignored.
#' @rdname record
#' @exportS3Method
#' @return The `print()` method prints the results as plain text or HTML to the
#'   console or displays the HTML page.
print.xfun_record_results = function(
  x, browse = interactive(), to = if (browse) 'html' else 'text', template = TRUE, ...
) {
  res = format(x, to, encode = browse, template = template, ...)
  if (browse && to == 'html') {
    viewer = getOption('viewer', utils::browseURL)
    f = tempfile('record-', fileext = '.html')
    write_utf8(res, f)
    viewer(f)
  } else {
    cat(res, sep = '\n')
  }
  invisible(x)
}
