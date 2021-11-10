#' Get data from a REST API
#'
#' Read data from a REST API and optionally with an authorization token in the
#' request header. The function \code{rest_api_raw()} returns the raw text of
#' the response, and \code{rest_api()} will parse the response with
#' \code{jsonlite::fromJSON()} (assuming that the response is in the JSON
#' format).
#'
#' These functions are simple wrappers based on \code{\link{url}()} and
#' \code{\link{read_utf8}()}. Specifically, the \code{headers} argument is
#' passed to \code{url()}, and \code{read_utf8()} will send a \samp{GET} request
#' to the API server. This means these functions only support the \samp{GET}
#' method. If you need to use other HTTP methods (such as \samp{POST}), you have
#' to use other packages such as \pkg{curl} and \pkg{httr}.
#' @param ... Arguments to be passed to \code{rest_api_raw()}.
#' @return A character vector (the raw JSON response) or an R object parsed from
#'   the JSON text.
#' @export
#' @examplesIf interactive()
#' # a normal GET request
#' xfun::rest_api('https://httpbin.org', '/get')
#' xfun::rest_api_raw('https://httpbin.org', '/get')
#'
#' # send the request with an auth header
#' xfun::rest_api('https://httpbin.org', '/headers', 'OPEN SESAME!')
#'
#' # with query parameters
#' xfun::rest_api('https://httpbin.org', '/response-headers', params = list(foo = 'bar'))
#'
#' # get the rate limit info from Github
#' xfun::github_api('/rate_limit')
rest_api = function(...) {
  res = rest_api_raw(...)
  jsonlite::fromJSON(res, simplifyVector = FALSE)
}

#' @param root The API root URL.
#' @param endpoint The API endpoint.
#' @param token A named character string (e.g., \code{c(token = "xxxx")}), which
#'   will be used to create an authorization header of the form
#'   \samp{Authorization: NAME TOKEN} for the API call, where \samp{NAME} is the
#'   name of the string and \samp{TOKEN} is the string. If the string does not
#'   have a name, \samp{Basic} will be used as the default name.
#' @param params A list of query parameters to be sent with the API call.
#' @param headers A named character vector of HTTP headers, e.g., \code{c(Accept
#'   = "application/vnd.github.v3+json")}.
#' @rdname rest_api
#' @export
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
  raw_string(suppressWarnings(read_utf8(con)))
}
