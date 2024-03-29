% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{format_bytes}
\alias{format_bytes}
\title{Format numbers of bytes using a specified unit}
\usage{
format_bytes(x, units = "auto", ...)
}
\arguments{
\item{x}{A numeric vector (each element represents a number of bytes).}

\item{units, ...}{Passed to \code{\link[=format.object_size]{format()}}.}
}
\value{
A character vector.
}
\description{
Call the S3 method \code{format.object_size()} to format numbers of bytes.
}
\examples{
xfun::format_bytes(c(1, 1024, 2000, 1e+06, 2e+08))
xfun::format_bytes(c(1, 1024, 2000, 1e+06, 2e+08), units = "KB")
}
