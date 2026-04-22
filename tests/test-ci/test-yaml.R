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
