% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_methods.R
\name{list_of_matrices}
\alias{list_of_matrices}
\title{Checks that an object is a list of matrices}
\usage{
list_of_matrices(list_to_check, allow_null = TRUE)
}
\arguments{
\item{list_to_check}{The potential list to check}

\item{allow_null}{If an element is \code{NULL}, is that OK?
Default \code{TRUE}}
}
\value{
Logical
}
\description{
Checks that an object is a list, and also
that the list is a collection of matrices.
Currently returns \code{FALSE} on an empty list
}
\keyword{internal}
