% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{is_sub_path}
\alias{is_sub_path}
\title{Test if a path is a subpath of a dir}
\usage{
is_sub_path(x, dir, n = nchar(dir))
}
\arguments{
\item{x}{A vector of paths.}

\item{dir}{A vector of directory paths.}

\item{n}{The length of \code{dir} paths.}
}
\value{
A logical vector.
}
\description{
Check if the path starts with the dir path.
}
\note{
You may want to normalize the values of the \code{x} and \code{dir} arguments
first (with \code{\link[=normalize_path]{normalize_path()}}), to make sure the path separators
are consistent.
}
\examples{
xfun::is_sub_path("a/b/c.txt", "a/b")  # TRUE
xfun::is_sub_path("a/b/c.txt", "d/b")  # FALSE
xfun::is_sub_path("a/b/c.txt", "a\\\\b")  # FALSE (even on Windows)
}
