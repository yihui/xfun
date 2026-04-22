library(testit)

assert('attr2() is strict', {
  z = structure(list(a = 1), foo = 2)
  (attr2(z, 'foo') %==% 2)
  (attr2(z, 'f') %==% NULL)
})

assert('in_dir() preserves the working directory', {
  owd = getwd()
  in_dir('.', setwd(tempdir()))
  (same_path(owd, getwd()))
})

assert('set_envvar() sets and restores environment variables', {
  old = set_envvar(c(XFUN_TEST_VAR_123 = 'hello'))
  (Sys.getenv('XFUN_TEST_VAR_123') %==% 'hello')
  set_envvar(old)
  (is.na(old['XFUN_TEST_VAR_123']))
  (Sys.getenv('XFUN_TEST_VAR_123') %==% '')
})

assert('env_option() reads from options() and env vars', {
  # clean state
  options(xfun.test.opt = NULL)
  Sys.unsetenv('R_XFUN_TEST_OPT')
  (is.null(env_option('xfun.test.opt')))
  (env_option('xfun.test.opt', 42) %==% 42)

  Sys.setenv(R_XFUN_TEST_OPT = 'from_env')
  (env_option('xfun.test.opt') %==% 'from_env')

  options(xfun.test.opt = 'from_opt')
  (env_option('xfun.test.opt') %==% 'from_opt')

  options(xfun.test.opt = NULL)
  Sys.unsetenv('R_XFUN_TEST_OPT')
})

assert('parse_only() parses R code without keeping source', {
  (parse_only(character(0)) %==% expression())
  (parse_only('1+1') %==% expression(1+1))
  (parse_only(c('y~x', '1:5')) %==% expression(y~x, 1:5))
})

assert('try_silent() suppresses errors', {
  z = try_silent(stop('oops'))
  (inherits(z, 'try-error'))
  (try_silent(1 + 1) %==% 2)
})

assert('try_error() returns TRUE on error, FALSE otherwise', {
  (try_error(stop('foo')))
  (!try_error(1:10))
})

assert('bump_version() increments the last digit', {
  (bump_version('0.1') %==% as.numeric_version('0.2'))
  (bump_version('7.23.9') %==% as.numeric_version('7.23.10'))
  (bump_version(c('0.1', '91.2.14')) %==% as.numeric_version(c('0.2', '91.2.15')))
})

assert('strip_html() removes HTML tags and comments', {
  (strip_html('<a href="#">Hello <!-- comment -->world!</a>') %==% 'Hello world!')
  (strip_html('<b>bold</b> and <i>italic</i>') %==% 'bold and italic')
  (strip_html('no tags') %==% 'no tags')
})

assert('html_escape() escapes special HTML characters', {
  (html_escape('& < >') %==% '&amp; &lt; &gt;')
  (html_escape('" & < > \r \n') %==% '" &amp; &lt; &gt; \r \n')
  (html_escape('" & < > \r \n', attr = TRUE) %==% '&quot; &amp; &lt; &gt; &#13; &#10;')
  (html_escape('plain text') %==% 'plain text')
})

assert('html_tag() generates correct HTML', {
  (inherits(html_tag('br'), 'xfun_html'))
  (c(html_tag('br')) %==% '<br />')
  (c(html_tag('p', 'hello')) %==% '<p>hello</p>')
  (c(html_tag('a', 'click', href = 'https://example.com')) %==% '<a href="https://example.com">click</a>')
  (c(html_tag('p', '<b>bold</b>')) %==% '<p>&lt;b&gt;bold&lt;/b&gt;</p>')
  # html_value content should not be escaped
  (c(html_tag('p', html_value('<b>bold</b>'))) %==% '<p><b>bold</b></p>')
  (has_error(html_tag('p', .attrs = list(1))))
  # NULL attribute value: only the attribute name is emitted
  (c(html_tag('input', type = 'checkbox', checked = NULL)) %==% '<input type="checkbox" checked />')
})

assert('format_bytes() formats byte counts', {
  res = format_bytes(c(1, 1024))
  (res %==% c('1 bytes', '1 Kb'))
})

assert('msg_cat() prints messages suppressibly', {
  out = capture.output(suppressMessages(msg_cat('hello')))
  (length(out) %==% 0L)
  out = capture.output(msg_cat('world'))
  (out %==% 'world')
})

assert('%|% returns x if non-empty, otherwise y', {
  (('hello' %|% 'world') %==% 'hello')
  ((character(0) %|% 'fallback') %==% 'fallback')
  ((NULL %|% 'fallback') %==% 'fallback')
  ((1:3 %|% 99) %==% 1:3)
})

assert('stop2() and warning2() do not include the call', {
  err = tryCatch(stop2('test error'), error = identity)
  (conditionMessage(err) %==% 'test error')
  (is.null(err$call))
  wrn = tryCatch(warning2('test warn'), warning = identity)
  (conditionMessage(wrn) %==% 'test warn')
  (is.null(wrn$call))
})

assert('gsubi() performs case-insensitive substitution', {
  (gsubi('hello', 'Hi', 'Hello World') %==% 'Hi World')
  (gsubi('world', 'R', 'hello WORLD') %==% 'hello R')
})

assert('retry() retries a failing function', {
  i = 0L
  # succeeds on the 2nd call
  res = retry(function() { i <<- i + 1L; if (i < 2L) stop('fail'); i }, .times = 3, .pause = 0)
  (res %==% 2L)
  # fails all .times times and stops
  (has_error(retry(function() stop('always'), .times = 2, .pause = 0)))
})

assert('handle_error() calls handler and shows message on error', {
  msgs = c()
  tryCatch(
    withCallingHandlers(
      handle_error(stop('oops'), function(loc) c('caught', loc)),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m)); invokeRestart('muffleMessage')
      }
    ),
    error = function(e) NULL
  )
  (any(grepl('caught', msgs)))
})

assert('is_windows/unix/macos/linux/arm64 return logical(1)', {
  (is.logical(is_windows()) && length(is_windows()) == 1L)
  (is.logical(is_unix()) && length(is_unix()) == 1L)
  (is.logical(is_macos()) && length(is_macos()) == 1L)
  (is.logical(is_linux()) && length(is_linux()) == 1L)
  (is.logical(is_arm64()) && length(is_arm64()) == 1L)
  # exactly one of windows / (unix && macos) / (unix && linux) / other unix is true
  (is_windows() || is_unix())
})

assert('set_envvar() errors on unnamed or partially unnamed vars', {
  (has_error(set_envvar(c(1, 2))))        # unnamed
  (has_error(set_envvar(c(A = '1', '')))  # partial names
  )
})

assert('tree() produces a character tree diagram', {
  x = list(a = 1, b = list(c = 2, d = 3))
  out = tree(x)
  (inherits(out, 'xfun_raw_string'))
  (is.character(out))
  (length(out) > 0)
})

assert('connect_pipes() fills vertical pipes in tree diagrams', {
  # early return for m < 3 (too few rows)
  (connect_pipes(c('|- a', '  ')) %==% c('|- a', '  '))
  # pipe connection: blank rows between two |-, first column gets filled with '|'
  x = c('|- a', '  ', '  ', '|- c')
  res = connect_pipes(x)
  (substr(res[2], 1, 1) %==% '|')
  (substr(res[3], 1, 1) %==% '|')
  # k is cleared when a non-matching cell is found (line 272 path)
  x2 = c('|- a', '  ', 'xy', '|- c')
  res2 = connect_pipes(x2)
  (substr(res2[2], 1, 1) %==% ' ')
  # k is cleared when last row reached with all spaces (line 267 path)
  x3 = c('|- a', '  ', '  ', '  ')
  res3 = connect_pipes(x3)
  (substr(res3[2], 1, 1) %==% ' ')
})

assert('func_name() returns the name of the calling function', {
  f = function() func_name(sys.nframe())
  (f() %==% 'f')
})
