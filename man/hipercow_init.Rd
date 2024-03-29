% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/root.R
\name{hipercow_init}
\alias{hipercow_init}
\title{Create a hipercow root}
\usage{
hipercow_init(root = ".", driver = NULL, ...)
}
\arguments{
\item{root}{The path to the root, defaulting the current
directory.}

\item{driver}{Optionally, the name of a driver to configure}

\item{...}{Arguments passed through to \link{hipercow_configure} if
\code{driver} is non-NULL.}
}
\value{
Invisibly, the root object
}
\description{
Create a hipercow root.  This marks the directory where your task
information will be saved, along with a local copy of your R
packages (a "library" for the cluster).  Immediately after running
this the first time, you probably want to run
\code{\link[=hipercow_configure]{hipercow_configure()}} in order to control how we set up
your projects network paths and R version.
}
\examples{

# Create an empty root
path <- withr::local_tempfile()
hipercow_init(path)
}
