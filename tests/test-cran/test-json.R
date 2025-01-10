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

  JS = function(x) structure(x, class = "JS_EVAL")
  x = list(a = 1:5, b = JS("function() {return true;}"))
  out = '{\n  "a": [1, 2, 3, 4, 5],\n  "b": function() {return true;}\n}'
  (.tojson(x) %==% out)
})
