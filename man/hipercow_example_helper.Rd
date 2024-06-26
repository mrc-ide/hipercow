% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{hipercow_example_helper}
\alias{hipercow_example_helper}
\title{Example helper}
\usage{
hipercow_example_helper(
  runner = TRUE,
  with_logging = FALSE,
  new_directory = TRUE,
  initialise = TRUE
)
}
\arguments{
\item{runner}{Start a runner?  If \code{TRUE} (the default) we start a
background process with \code{callr::r_bg} that will pick tasks off a
queue and run them.  Or pass an integer, and start more than one
runner.}

\item{with_logging}{Run each task with logging; this is quite a
bit slower, but enables examples that use \link{task_log_show} etc.
Only has an effect if \code{runner} is \code{TRUE}.}

\item{new_directory}{Create new empty (temporary) directory?  If
\code{FALSE} then we just use the current directory.  This is used in
the vignettes where the directory is set already.}

\item{initialise}{Initialise? If \code{FALSE} then no initialisation
is done.  This is intended for examples that will use
\code{\link[=hipercow_init]{hipercow_init()}} later.}
}
\value{
A function that can be called (with no arguments) to
return to the original working directory and clean up all files
created for the example.
}
\description{
A helper used in running examples and docs.  This function will
change your working directory into a new temporary hipercow root,
and start a worker process that will quietly run tasks.  This is
not intended for people to use outside of running examples!
}
\keyword{internal}
