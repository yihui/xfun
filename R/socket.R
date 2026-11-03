# Small helpers shared by the HTTP/TCP code (app.R and cdp.R).

# connect to a TCP server (by default on the local machine)
local_socket = function(port, host = '127.0.0.1', blocking = TRUE, timeout = 10) {
  socketConnection(
    host, port = port, open = 'r+b', blocking = blocking, timeout = timeout
  )
}

# whether something accepts connections on the port
port_open = function(port, host = '127.0.0.1', timeout = 1) {
  con = suppressWarnings(tryCatch(
    local_socket(port, host, timeout = timeout), error = function(e) NULL
  ))
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

# turn raw bytes into a UTF-8 string
raw_to_utf8 = function(x) {
  x = rawToChar(x)
  Encoding(x) = 'UTF-8'
  x
}

# read the header lines of an HTTP message (up to the blank line); NULL if the
# connection is closed before the blank line
read_headers = function(con) {
  x = character()
  repeat {
    h = readLines(con, 1, warn = FALSE)
    if (!length(h)) return()
    if (!nzchar(trimws(h))) return(x)
    x = c(x, h)
  }
}

# the value of a header (NULL if absent) in a character vector of header lines
header_value = function(headers, name) {
  i = grep(paste0('^', name, ':'), headers, ignore.case = TRUE)
  if (length(i)) trimws(sub('^[^:]*:', '', headers[i[1]]))
}

# the Content-Length of a message, or NA if absent or invalid
content_length = function(headers) {
  v = header_value(headers, 'content-length')
  if (length(v) && grepl('^[0-9]+$', v)) as.integer(v) else NA_integer_
}
