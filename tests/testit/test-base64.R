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
