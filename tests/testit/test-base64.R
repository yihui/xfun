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
  has_error(base64_decode("lz..??"))
})

assert('base64_encode_r() returns the same result as base64_encode()', {
  f = R_logo()
  base64_encode_r(f) %==% base64_encode(f)
})
