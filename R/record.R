#' Run R code and record the results
#'
#' Run R code and capture various types of output, including text output, plots,
#' messages, warnings, and errors.
#' @param code A character vector of R source code.
#' @param dev A graphics device. It can be a function name, a function, or a
#'   character string that can be evaluated to a function to open a graphics
#'   device.
#' @param dev.path A base file path for plots. Actual plot filenames will be
#'   this base path plus incremental suffixes. For example, if `dev.path =
#'   "foo"`, the plot files will be `foo-1.png`, `foo-2.png`, and so on. If
#'   `dev.path` is not character (e.g., `FALSE`), plots will not be recorded.
#' @param dev.ext The file extension for plot files. By default, it will be
#'   inferred from the first argument of the device function if possible.
#' @param dev.args Extra arguments to be passed to the device. The default
#'   arguments are `list(units = 'in', onefile = FALSE, width = 7, height = 7,
#'   res = 96)`. If any of these arguments is not present in the device
#'   function, it will be dropped.
#' @param message,warning,error If `TRUE`, record and store messages / warnings
#'   / errors in the output. If `FALSE`, suppress them. If `NA`, do not process
#'   them (messages will be emitted to the console, and errors will halt the
#'   execution).
#' @param cache A list of options for caching. See the `path`, `id`, and `...`
#'   arguments of [cache_exec()].
#' @param print A (typically S3) function that takes the value of an expression
#'   in the code as input and returns output. The default is [record_print()].
#'   If a non-function value (e.g., `NA`) is passed to this argument, [print()]
#'   (or [show()] for S4 objects) will be used.
#' @param print.args A list of arguments for the `print` function. By default,
#'   the whole list is not passed directly to the function, but only an element
#'   in the list with a name identical to the first class name of the returned
#'   value of the expression, e.g., `list(data.frame = list(digits = 3), matrix
#'   = list())`. This makes it possible to apply different print arguments to
#'   objects of different classes. If the whole list is intended to be passed to
#'   the print function directly, wrap the list in [I()].
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
#' code = c('# a warning test', '1:2 + 1:3', 'par(mar = c(4, 4, 1, .2))', 'barplot(5:1, col = 2:6, horiz = TRUE)', 'head(iris)', "sunflowerplot(iris[, 3:4], seg.col = 'purple')", "if (TRUE) {\n  message('Hello, xfun::record()!')\n}", '# throw an error', "1 + 'a'")
#' res = xfun::record(code, dev.args = list(width = 9, height = 6.75), error = TRUE)
#' xfun::tree(res)
#' format(res)
#' # find and clean up plot files
#' plots = Filter(function(x) inherits(x, 'record_plot'), res)
#' file.remove(unlist(plots))
record = function(
  code = NULL, dev = 'png', dev.path = 'xfun-record', dev.ext = dev_ext(dev),
  dev.args = list(), message = TRUE, warning = TRUE, error = NA, cache = list(),
  print = record_print, print.args = list(),
  verbose = getOption('xfun.record.verbose', 0), envir = parent.frame()
) {
  new_result = function(x = list()) structure(x, class = 'xfun_record_results')
  res = new_result()
  if (length(code) == 0) return(res)

  # use cached results if cache exists
  use_cache = FALSE
  ret = cache_code(code, envir, use_cache <- TRUE, cache)
  if (use_cache) return(ret)

  code = split_lines(code)

  add_result = function(x, type = NULL, pos = length(res), insert = TRUE) {
    # default type is output
    if (is.null(type) && !inherits(x, .record_classes)) type = 'output'
    # insert a whole element or append to an existing element in res
    if (!insert) x = c(res[[pos]], x)
    el = if (is.null(type)) x else new_record(x, type)
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
    # don't record plots if no device was opened
    if (length(dev_num) == 0) return(function(...) {})
    old_files = character(); old_times = NULL  # previous plot files and file mtimes
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

      # old files that have been modified should be removed from existing records
      k = file.mtime(old_files) > old_times
      if (any(k)) for (i in seq_along(res)) {
        if (inherits(res[[i]], 'record_plot'))
          res[[i]] <<- new_record(setdiff(res[[i]], old_files[k]), 'plot')
      }
      old_files = old_files[!k]

      plots = setdiff(files, old_files)
      old_files <<- files
      old_times <<- file.mtime(files)
      if ((n <- length(plots)) == 0) return()

      # indices of plots in results
      i = which(vapply(res, inherits, logical(1), 'record_plot'))
      N = length(i)
      # the last plot should always be appended to the last plot block
      if (last) {
        # N = 0 means the code didn't produce any plots
        if (N == 0) file.remove(plots) else add_result(plots, 'plot', i[N], FALSE)
        return()
      }

      # for the newly generated plots, append the first one (which should be the
      # last plot of a previous code chunk, since it will be created only when
      # the next code chunk produces a new plot) to the previous plot block; the
      # rest are for the current block
      if (N > 1) {
        add_result(plots[1], 'plot', i[N - 1], FALSE)
        if (n > 1) add_result(plots[2:n], 'plot', i[N], FALSE)
      } else if (N == 1) {
        # if there exists only one plot block, add plots to that block
        add_result(plots, 'plot', i[N], FALSE)
      }
    }
  })

  handle = if (is.na(error)) identity else try_silent
  # split code into individual expressions
  codes = handle(split_source(code, merge_comments = TRUE, line_number = TRUE))
  # code may contain syntax errors
  if (is_error(codes)) {
    add_result(code, 'source'); add_result(attr2(codes, 'condition')$message, 'error')
    return(new_result(res))
  }

  handle_message = function(type, add = TRUE) {
    mf = sub('^(.)', 'muffle\\U\\1', type, perl = TRUE)
    function(e) {
      if (is.na(add)) return()
      if (isTRUE(add)) add_result(e$message, type)
      if (type %in% c('message', 'warning')) invokeRestart(mf)
    }
  }
  handle_m = handle_message('message', message)
  handle_w = handle_message('warning', warning)
  handle_e = handle_message('error', error)

  # don't use withCallingHandlers() if message/warning/error are all NA
  handle_eval = function(expr) {
    handle(if (is.na(message) && is.na(warning) && is.na(error)) expr else {
      withCallingHandlers(
        expr, message = handle_m, warning = handle_w, error = handle_e
      )
    })
  }
  # a simplified version of capture.output()
  handle_output = function(expr) {
    out = NULL
    con = textConnection('out', 'w', local = TRUE)
    on.exit(close(con))
    sink(con); on.exit({ sink(); close(con) })
    expr  # lazy evaluation
    on.exit()  # if no error occurred, clear up previous on-exit calls
    sink()
    close(con)
    if (length(out)) add_result(out, 'output')
    expr
  }
  n = length(codes)
  for (i in seq_len(n)) {
    add_result(code <- codes[[i]], 'source')
    expr = parse_only(code)
    if (length(expr) == 0) next
    # evaluate the code and capture output
    out = handle_output(handle_eval(withVisible(eval(expr, envir))))
    # verbose = 1: always print the last value; verbose = 2: print all values
    if (verbose == 2 || (verbose == 1 && i == n)) {
      # make invisible values visible unless they are NULL (which is often not useful)
      if (!is_error(out) && !out$visible && !is.null(out$value)) out$visible = TRUE
    }
    # print value (via record_print()) if visible
    if (!is_error(out) && out$visible) {
      if (is.null(print)) print = record_print
      if (!is.function(print)) print = record_print.default
      p_args = print.args
      val = out$value
      # index print args by first class of out unless args are wrapped in I()
      if (!inherits(p_args, 'AsIs')) p_args = p_args[[class(val)[1]]]
      out = handle_eval(if (length(p_args) == 0) print(val) else do.call(
        print, c(list(val), p_args), quote = inherits(val, c('name', 'call'))
      ))
      if (length(out) && !is_error(out)) {
        if (is.list(out)) lapply(out, add_result) else add_result(out)
      }
    }
    handle_plot()
  }
  # shut off the device to write out the last plot if there exists one
  dev_off()
  handle_plot(TRUE)

  # remove empty blocks
  res = Filter(length, res)
  res = merge_record(res)

  new_result(res)
}

# merge neighbor elements of the same class
merge_record = function(x) {
  n = length(x)
  if (n <= 1) return(x)
  k = NULL
  for (i in 2:n) {
    r1 = x[[i - 1]]; c1 = class(r1); r2 = x[[i]]; c2 = class(r2)
    if (!identical(c1, c2)) next
    # concatenate messages to a single string (consider message(appendLF = FALSE))
    x[[i]] = if ('record_message' %in% c1) paste(c(r1, r2), collapse = '') else c(r1, r2)
    attributes(x[[i]]) = attributes(r1)
    k = c(k, i - 1)
  }
  if (length(k)) x = x[-k]
  x
}

#' Print methods for `record()`
#'
#' An S3 generic function to be called to print visible values in code when the
#' code is recorded by [record()]. It is similar to [knitr::knit_print()]. By
#' default, it captures the normal [print()] output and returns the result as a
#' character vector. The `knitr_kable` method is for printing [knitr::kable()]
#' output. Users and package authors can define other S3 methods to extend this
#' function.
#' @param x For `record_print()`, the value to be printed. For `new_record()`, a
#'   character vector to be included in the printed results.
#' @param ... Other arguments to be passed to `record_print()` methods.
#' @return A `record_print()` method should return a character vector or a list
#'   of character vectors. The original classes of the vector will be discarded,
#'   and the vector will be treated as console output by default (i.e.,
#'   `new_record(class = "output")`). If it should be another type of output,
#'   wrap the vector in [new_record()] and specify a class name.
#' @export
record_print = function(x, ...) {
  UseMethod('record_print')
}

#' @rdname record_print
#' @export
record_print.default = function(x, ...) {
  # the default print method is just print()/show()
  capture.output(if (isS4(x)) methods::show(x, ...) else print(x, ...))
}

#' @rdname record_print
#' @export
record_print.record_asis = function(x, ...) {
  x
}

#' @param class A class name. Possible values are `xfun:::.record_cls`.
#' @rdname record_print
#' @export
new_record = function(x, class) structure(x, class = paste0('record_', class))

# all possible classes for record() results at the moment
.record_cls = c('source', 'output', 'message', 'warning', 'error', 'plot', 'asis')
.record_classes = paste0('record_', .record_cls)

# default device arguments
dev_args = list(units = 'in', onefile = FALSE, width = 8, height = 8, res = 84)

dev_open = function(dev, file, args) {
  m = names(formals(dev))
  for (i in names(dev_args)) {
    if (i %in% m && is.null(args[[i]])) args[[i]] = dev_args[[i]]
  }
  n = length(dev.list())
  do.call(dev, c(list(file), args))
  if (length(dev.list()) <= n) stop('Failed to open device for recording plots.')
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

#' @param to The output format (text, markdown, or html).
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
    x, to = c('text', 'markdown', 'html'), encode = FALSE, template = FALSE, ...
) {
  alt = 'A plot recorded by xfun::record()'
  if (to[1] != 'html') {
    is_md = to[1] == 'markdown'
    res = unlist(lapply(x, function(z) {
      if (length(z) == 0) return()
      cls_all = sub('^record_', '', class(z)); cls = cls_all[1]
      if (cls != 'asis') {
        if (is_md && cls == 'plot') {
          z = sprintf('![%s](<%s>)', alt, z)
        } else {
          o = attr2(z, 'opts'); a = o$attr
          z = gsub('^(\\s*\n)+|\n\\s*$', '', one_string(z))  # trim blank lines
          if (cls != 'source') z = paste0(o$comment %||% '#> ', split_lines(z))
          if (is_md) {
            cls_all = if (any(c('message', 'warning', 'error') %in% cls_all)) {
              c('plain', cls_all)
            } else if (cls == 'source') {
              replace(cls_all, cls_all == 'source', 'r')
            } else {
              if (length(a)) a = c(sub('^[.]', '', a), 'plain')
              setdiff(c(a, cls_all), 'output')
            }
            z = fenced_block(z, sprintf('.%s', cls_all))
          }
        }
      }
      z = one_string(z)
      if (!is_md) z = gsub('\n*$', '\n', z)
      z
    }))
    return(raw_string(res, lang = if (is_md) '.md'))
  }
  res = unlist(lapply(x, function(z) {
    cls = sub('^record_', '', class(z))
    if (cls == 'asis') z else if (cls == 'plot') {
      sprintf(
        '<p class="%s"><img src="%s" alt="%s" /></p>',
        cls, if (encode) vapply(z, base64_uri, '') else URLencode(z), alt
      )
    } else {
      paste0(
        sprintf(
          '<pre class="%s"><code>',
          if (cls == 'source') paste0('language-r" data-start="', attr2(z, 'lines')[1]) else cls
        ),
        html_escape(one_string(z)), '</code></pre>'
      )
    }
  }))
  if (isTRUE(template)) template = pkg_file('resources', 'record.html')
  if (is.character(template)) {
    res = sub('$body$', one_string(res), read_utf8(template), fixed = TRUE)
    if (length(x) > 0) res = sub('$title$', make_title(x[[1]]), res, fixed = TRUE)
  }
  raw_string(res, lang = '.html')
}

# generate a title from the first line of a character vector
make_title = function(x) {
  if (length(x) == 0) return('')
  x = gsub('^#+\\s*', '', x[1])  # remove possible comment chars
  x = gsub('\\s*[.]*$', '... | ', x)  # add ... to the end
  html_escape(x)
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
