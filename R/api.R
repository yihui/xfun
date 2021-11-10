# fetch data from a REST API
rest_api = function(...) {
  res = rest_api_raw(...)
  jsonlite::fromJSON(res, simplifyVector = FALSE)
}

# read raw data from a REST API, and optionally with (authorization) headers
rest_api_raw = function(root, endpoint, token = '', params = list(), headers = NULL)  {
  if (is.null(names(token))) names(token) = 'Basic'
  endpoint = sub('^/?', '/', endpoint)  # make sure it has a leading /
  con = url(
    paste0(root, endpoint, query_params(.list = params)), encoding = 'UTF-8',
    headers = c(
      headers, if (token != '') c(Authorization = sprintf('%s %s', names(token), token))
    )
  )
  on.exit(close(con), add = TRUE)
  suppressWarnings(read_utf8(con))
}
