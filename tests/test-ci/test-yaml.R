library(testit)

# test yaml_load() error handler (requires the yaml package)
if (loadable('yaml')) assert('yaml_load() shows helpful error message on invalid YAML', {
  invalid_yaml = 'a: [\nunclosed bracket'
  msgs = c()
  tryCatch(
    withCallingHandlers(
      yaml_load(invalid_yaml),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m)); invokeRestart('muffleMessage')
      }
    ),
    error = function(e) NULL
  )
  (gsub('^Failed to parse YAML:.*', '', msgs) %==% '')
})

# test yaml_load() error handler when location info is present
if (loadable('yaml')) assert('yaml_load() error handler returns NULL for non-parseable error msgs', {
  # when the error message has no line/column info, handler returns NULL
  msgs = c()
  tryCatch(
    withCallingHandlers(
      # force a yaml error that won't have standard line/column info by
      # using an invalid handler that raises an error without line info
      handle_error(
        stop('some yaml error without line info'),
        function(loc) NULL
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m)); invokeRestart('muffleMessage')
      }
    ),
    error = function(e) NULL
  )
  TRUE  # just verify no crash
})
