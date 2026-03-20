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
  old = set_envvar(c(XFUN_TEST_VAR = 'hello'))
  (Sys.getenv('XFUN_TEST_VAR') %==% 'hello')
  set_envvar(old)
  (is.na(old['XFUN_TEST_VAR']))
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
  (length(parse_only(character(0))) %==% 0L)
  (length(parse_only('1+1')) %==% 1L)
  (length(parse_only(c('y~x', '1:5'))) %==% 2L)
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
  (html_escape('" & < > \r \n', attr = TRUE) %==% '&quot; &amp; &lt; &gt; &#13; &#10;')
  (html_escape('plain text') %==% 'plain text')
})

assert('html_tag() generates correct HTML', {
  (inherits(html_tag('br'), 'xfun_html'))
  (capture.output(html_tag('br')) %==% '<br />')
  (capture.output(html_tag('p', 'hello')) %==% '<p>hello</p>')
  (capture.output(html_tag('a', 'click', href = 'https://example.com')) %==% '<a href="https://example.com">click</a>')
  (capture.output(html_tag('p', '<b>bold</b>')) %==% '<p>&lt;b&gt;bold&lt;/b&gt;</p>')
  # html_value content should not be escaped
  (capture.output(html_tag('p', html_value('<b>bold</b>'))) %==% '<p><b>bold</b></p>')
  (has_error(html_tag('p', .attrs = list(1))))
})

assert('format_bytes() formats byte counts', {
  res = format_bytes(c(1, 1024))
  (length(res) %==% 2L)
  (is.character(res))
})

assert('msg_cat() prints messages suppressibly', {
  out = capture.output(suppressMessages(msg_cat('hello')))
  (length(out) %==% 0L)
  out = capture.output(msg_cat('world'))
  (out %==% 'world')
})
