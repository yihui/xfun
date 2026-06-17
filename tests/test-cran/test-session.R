library(testit)

assert('do_once() executes the task exactly once', {
  opt = 'xfun.test.do_once.exec'
  options(setNames(list(NULL), opt))  # ensure clean state
  # use an env to share a counter with the nested expression
  e = new.env(parent = emptyenv()); e$count = 0L
  suppressMessages(do_once({ e$count = e$count + 1L }, opt))
  (e$count %==% 1L)
  # second call is skipped
  suppressMessages(do_once({ e$count = e$count + 1L }, opt))
  (e$count %==% 1L)
  options(setNames(list(NULL), opt))
})

assert('do_once() returns the task value invisibly', {
  opt = 'xfun.test.do_once.ret'
  options(setNames(list(NULL), opt))
  val = suppressMessages(do_once({
    v = 42L; v
  }, opt))
  (val %==% 42L)
  options(setNames(list(NULL), opt))
})

assert('do_once() emits a hint message and sets option to FALSE', {
  opt = 'xfun.test.do_once.hint'
  options(setNames(list(NULL), opt))
  msgs = character()
  withCallingHandlers(
    do_once(invisible(NULL), opt),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart('muffleMessage')
    }
  )
  # the hint message should mention the option name
  (any(grepl(opt, msgs, fixed = TRUE)))
  # option is now FALSE so subsequent calls are no-ops
  (identical(getOption(opt), FALSE))
  options(setNames(list(NULL), opt))
})

assert('do_once() is a no-op when option is already FALSE', {
  opt = 'xfun.test.do_once.noop'
  options(setNames(list(FALSE), opt))
  e = new.env(parent = emptyenv()); e$count = 0L
  res = do_once({ e$count = e$count + 1L; 99L }, opt)
  (e$count %==% 0L)
  (is.null(res))
  options(setNames(list(NULL), opt))
})

assert('do_once() with hint = "" emits no extra message', {
  opt = 'xfun.test.do_once.nohint'
  options(setNames(list(NULL), opt))
  msgs = character()
  withCallingHandlers(
    do_once(invisible(NULL), opt, hint = ''),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart('muffleMessage')
    }
  )
  (length(msgs) %==% 0L)
  options(setNames(list(NULL), opt))
})

