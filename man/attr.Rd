% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{attr}
\alias{attr}
\title{Obtain an attribute of an object without partial matching}
\usage{
attr(...)
}
\arguments{
\item{...}{Passed to \code{base::\link[base]{attr}()} (without the
\code{exact} argument).}
}
\description{
An abbreviation of \code{base::\link[base]{attr}(exact = TRUE)}.
}
\examples{
z = structure(list(a = 1), foo = 2)
base::attr(z, "f")  # 2
xfun::attr(z, "f")  # NULL
xfun::attr(z, "foo")  # 2
}
