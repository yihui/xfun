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
  # NA becomes null (character and numeric)
  (json_vector(c('a', NA_character_), to_array = TRUE) %==% '["a", null]')
  (json_vector(c(1, NA, 3), to_array = TRUE, quote = FALSE) %==% '[1, null, 3]')
  # Inf/-Inf become Infinity/-Infinity (valid JS)
  (json_vector(c(1, Inf, -Inf), to_array = TRUE, quote = FALSE) %==% '[1, Infinity, -Infinity]')
  (json_vector(c(Inf, NA), to_array = TRUE, quote = FALSE) %==% '[Infinity, null]')
})

assert('json_atomic() handles Date, POSIXct, and factor', {
  d = as.Date('2024-01-15')
  (json_atomic(d) %==% 'new Date("2024-01-15")')
  # Date/POSIXt in data.frames (I() must not trigger format.AsIs truncation)
  t = as.POSIXct(c('2024-01-15 09:30:00', '2024-01-15 17:45:00'), tz = 'UTC')
  (.tojson(data.frame(t = t)) %==%
    '{\n  "t": [new Date("2024-01-15 09:30:00"), new Date("2024-01-15 17:45:00")]\n}')
  # single-row Date data.frame still produces an array
  (.tojson(data.frame(d = d)) %==% '{\n  "d": [new Date("2024-01-15")]\n}')
  f = factor(c('a', 'b', 'a'))
  (json_atomic(f) %==% '["a", "b", "a"]')
})

assert('.tojson() handles arrays', {
  # each matrix row is an array element
  (.tojson(matrix(1:4, 2)) %==% '[\n  [1, 3],\n  [2, 4]\n]')
  (.tojson(matrix(1:6, 2)) %==% '[\n  [1, 3, 5],\n  [2, 4, 6]\n]')
  # single-column matrices still emit one array per row
  (.tojson(matrix(1:3, 3)) %==% '[\n  [1],\n  [2],\n  [3]\n]')
  # single-row matrices produce a single array element
  (.tojson(matrix(1:3, 1)) %==% '[\n  [1, 2, 3]\n]')
  # character matrices are quoted and NA becomes null
  (.tojson(matrix(c('a', NA, 'c', 'd'), 2)) %==%
    '[\n  ["a", "c"],\n  [null, "d"]\n]')
  # Inf/-Inf in numeric matrices
  (.tojson(matrix(c(1, Inf, -Inf, 2), 2)) %==%
    '[\n  [1, -Infinity],\n  [Infinity, 2]\n]')
  # higher-dimensional arrays recurse (nested one level deeper per dimension)
  (.tojson(array(1:8, c(2, 2, 2))) %==% paste0(
    '[\n  [\n    [1, 5],\n    [3, 7]\n  ],\n',
    '  [\n    [2, 6],\n    [4, 8]\n  ]\n]'
  ))
})

assert('json_vector() escapes control characters in strings', {
  # newline, backspace, formfeed, carriage return, tab must all be escaped
  (json_vector('\n', to_array = FALSE) %==% '"\\n"')
  (json_vector('\b', to_array = FALSE) %==% '"\\b"')
  (json_vector('\f', to_array = FALSE) %==% '"\\f"')
  (json_vector('\r', to_array = FALSE) %==% '"\\r"')
  (json_vector('\t', to_array = FALSE) %==% '"\\t"')
  # backslash and double-quote must be escaped (via quote_string)
  (json_vector('a\\b', to_array = FALSE) %==% '"a\\\\b"')
  (json_vector('say "hi"', to_array = FALSE) %==% '"say \\"hi\\""')
})

assert('quote_string() returns character(0) for empty input', {
  (quote_string(character(0)) %==% character(0))
})

assert('quote_string() escapes </script to keep output inline-<script> safe', {
  # a literal </script> would close an inline <script> block early
  (quote_string('a</script>b') %==% '"a<\\/script>b"')
  (json_vector('</script>', to_array = FALSE) %==% '"<\\/script>"')
  # case-insensitive like the HTML parser, preserving the original case
  (quote_string('</SCRIPT>') %==% '"<\\/SCRIPT>"')
  # other </ sequences and bare / are left alone (e.g. a regex, </style>)
  (quote_string('</style>') %==% '"</style>"')
  (quote_string('if (a</b && /</.test(s))') %==% '"if (a</b && /</.test(s))"')
  (quote_string('a/b') %==% '"a/b"')
})

assert('tojson(factor = ) chooses string vs dict encoding', {
  f = factor(c('b', 'a', 'b', NA, 'a'))
  # default: array of character values
  (.tojson(f) %==% '["b", "a", "b", null, "a"]')
  # dict: runnable JS expression, 0-based codes into a levels array; the ?? null
  # restores NA (which is the code null) because levels[null] is undefined in JS
  (.tojson(f, factor = 'dict') %==%
    '[1, 0, 1, null, 0].map(i => ["a", "b"][i] ?? null)')
  # no NA -> bare .map(), no ?? null tail
  (.tojson(factor(c('b', 'a', 'b')), factor = 'dict') %==%
    '[1, 0, 1].map(i => ["a", "b"][i])')
  # dict threads through list/data-frame columns
  (.tojson(list(x = f, y = 1:3), factor = 'dict') %==% paste0(
    '{\n  "x": [1, 0, 1, null, 0].map(i => ["a", "b"][i] ?? null),\n',
    '  "y": [1, 2, 3]\n}'
  ))
})
