% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yaml.R
\name{yaml_body}
\alias{yaml_body}
\title{Partition the YAML metadata and the body in a document}
\usage{
yaml_body(x, ...)
}
\arguments{
\item{x}{A character vector of the document content.}

\item{...}{Arguments to be passed to \code{yaml_load()}.}
}
\value{
A list of components \code{yaml} (the parsed YAML data), \code{lines} (starting
and ending line numbers of YAML), and \code{body} (a character vector of the
body text). If YAML metadata does not exist in the document, the components
\code{yaml} and \code{lines} will be missing.
}
\description{
Split a document into the YAML metadata (which starts with \verb{---} in the
beginning of the document) and the body. The YAML metadata will be parsed.
}
\examples{
xfun::yaml_body(c("---", "title: Hello", "output: litedown::html_format", "---",
    "", "Content."))
}
