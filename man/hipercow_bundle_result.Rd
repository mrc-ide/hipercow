% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bundle.R
\name{hipercow_bundle_result}
\alias{hipercow_bundle_result}
\title{Fetch bundle results}
\usage{
hipercow_bundle_result(bundle, follow = TRUE, root = NULL)
}
\arguments{
\item{bundle}{Either a \link{hipercow_bundle} object, or the name of a
bundle.}

\item{follow}{Logical, indicating if we should follow any retried
tasks.}

\item{root}{A hipercow root, or path to it. If \code{NULL} we search up
your directory tree.}
}
\value{
An unnamed list, with each element being the result for
each a task in the bundle, in the same order.
}
\description{
Fetch all bundle results
}