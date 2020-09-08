% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string.R
\name{split_source}
\alias{split_source}
\title{Split source lines into complete expressions}
\usage{
split_source(x)
}
\arguments{
\item{x}{A character vector of R source code.}
}
\value{
A list of character vectors, and each vector contains a complete R
  expression.
}
\description{
Parse the lines of code one by one to find complete expressions in the code,
and put them in a list.
}
\examples{
xfun::split_source(c("if (TRUE) {", "1 + 1", "}", "print(1:5)"))
}