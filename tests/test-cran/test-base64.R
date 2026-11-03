library(testit)

assert('base64_encode() encodes the string correctly', {
  ref = c(
    "AQ==", "AQI=", "AQID", "AQIDBA==", "AQIDBAU=", "AQIDBAUG",
    "AQIDBAUGBw==", "AQIDBAUGBwg=", "AQIDBAUGBwgJ", "AQIDBAUGBwgJCg=="
  )
  (sapply(1:10, function(i) base64_encode(as.raw(1:i))) == ref)

  ref = c(
    "/w==", "//4=", "//79", "//79/A==", "//79/Ps=", "//79/Pv6",
    "//79/Pv6+Q==", "//79/Pv6+fg=", "//79/Pv6+fj3", "//79/Pv6+fj39g=="
  )
  (sapply(255:246, function(i) base64_encode(as.raw(255:i))) == ref)
})

assert('base64_decode() decodes the string correctly', {
  (sapply(c(1:10, 255:246), function(i) {
    input = as.raw(1:i)
    output = base64_encode(input)
    input2 = base64_decode(output)
    input %==% input2
  }))
})

assert('base64_decode() will not make R crash if the input is not valid', {
  (has_error(base64_decode("lz..??")))
})

assert('base64_encode_r() returns the same result as base64_encode()', {
  f = R_logo()
  (base64_encode_r(f) %==% base64_encode(f))
})

assert('base64_decode() does not accept a non-string input', {
  (has_error(base64_decode(x = 42)))
})

assert('base64_decode() does not accept both string and file input', {
  f = tempfile()
  (has_error(base64_decode(x = 'Kg==', from = f)))
})

assert('base64_decode() returns the same result when the same string is used as an input directly or from a file connection', {
  f = tempfile()
  writeLines(text = "Kg==", con = f, sep = "")
  (base64_decode(from = f) %==% base64_decode(x = 'Kg=='))
})

assert('base64_uri() returns proper data type', {
  f = R_logo()
  (!grepl('[.]svg$', f) || strsplit(base64_uri(f), split = ';')[[1]][1] %==% 'data:image/svg+xml')
})

assert('base64_encode_r() handles all remainder cases (n mod 3)', {
  # n mod 3 == 1 (e.g., 1 byte): 1 data byte -> 2 base64 chars + "=="
  (base64_encode_r(as.raw(0xff)) %==% base64_encode(as.raw(0xff)))
  # n mod 3 == 2 (e.g., 2 bytes): 2 data bytes -> 3 base64 chars + "="
  (base64_encode_r(as.raw(1:2)) %==% base64_encode(as.raw(1:2)))
  # n mod 3 == 0 (e.g., 3 bytes): no padding
  (base64_encode_r(as.raw(1:3)) %==% base64_encode(as.raw(1:3)))
})

assert('mime_type() returns correct types', {
  if (loadable('mime')) {
    f = tempfile(fileext = '.png'); file.create(f)
    (mime_type(f) %==% 'image/png')
    unlink(f)
  }
})

assert('mime_type() without mime package uses system command', {
  f = tempfile(fileext = '.css'); writeLines('body {}', f)
  m = mime_type(f, use_mime = FALSE)
  (m %==% 'text/css')
  unlink(f)
})
