library(testit)

assert("tojson() works", {
  (.tojson(NULL) %==% "null")
  (.tojson(list()) %==% "{}")
  (.tojson(NA) %==% 'null')
  (.tojson(NA_character_) %==% 'null')
  (.tojson(1:10) %==% "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
  (.tojson(TRUE) %==% "true")
  (.tojson(FALSE) %==% "false")

  x = list(a = 1, b = list(c = 1:3, d = "abc"))
  out = '{\n  "a": 1,\n  "b": {\n    "c": [1, 2, 3],\n    "d": "abc"\n  }\n}'
  (.tojson(x) %==% out)

  x = list(c("a", "b"), 1:5, TRUE)
  out = '[\n  ["a", "b"],\n  [1, 2, 3, 4, 5],\n  true\n]'
  (.tojson(x) %==% out)

  (.tojson(list('"a b"' = 'quotes "\'')) %==% '{\n  "\\"a b\\"": "quotes \\"\'"\n}')

  # data frames
  (.tojson(data.frame()) %==% '{}')
  # by column for named data frames
  x = data.frame(a = 1:3, b = c('x', 'y', 'z'))
  (.tojson(x) %==% '{\n  "a": [1, 2, 3],\n  "b": ["x", "y", "z"]\n}')
  # length 1 columns must be arrays, too
  x = data.frame(a = 1, b = 'x')
  (.tojson(x) %==% '{\n  "a": [1],\n  "b": ["x"]\n}')
  # by row for unnamed data frames
  x = unname(data.frame(a = 1:3, b = c('x', 'y', 'z')))
  (.tojson(x) %==% '[\n  [1, "x"],\n  [2, "y"],\n  [3, "z"]\n]')

  x = list(a = 1:5, b = js("function() {return true;}"))
  out = '{\n  "a": [1, 2, 3, 4, 5],\n  "b": function() {return true;}\n}'
  (.tojson(x) %==% out)

  res = tojson(list(NULL, 1:10, TRUE, FALSE))
  # passing a json object through tojson() should return it unchanged
  (tojson(res) %==% res)
})

assert('json_vector() converts atomic vectors to JSON', {
  (json_vector(c('a', 'b'), to_array = TRUE) %==% '["a", "b"]')
  (json_vector(c('a', 'b'), to_array = FALSE) %==% c('"a"', '"b"'))
  (json_vector(1:3, to_array = TRUE, quote = FALSE) %==% '[1, 2, 3]')
  # NA becomes null
  (json_vector(c('a', NA_character_), to_array = TRUE) %==% '["a", null]')
})

assert('json_atomic() handles Date, POSIXct, and factor', {
  d = as.Date('2024-01-15')
  (json_atomic(d) %==% 'new Date("2024-01-15")')
  f = factor(c('a', 'b', 'a'))
  (json_atomic(f) %==% '["a", "b", "a"]')
})

assert('.tojson() handles arrays', {
  m = matrix(1:4, nrow = 2)
  out = .tojson(m)
  (grepl('\\[', out))  # should produce nested arrays
})
