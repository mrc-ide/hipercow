% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task.R
\name{task_wait}
\alias{task_wait}
\title{Wait for a task to complete}
\usage{
task_wait(
  id,
  for_start = FALSE,
  timeout = NULL,
  poll = 1,
  progress = NULL,
  follow = TRUE,
  root = NULL
)
}
\arguments{
\item{id}{The task identifier}

\item{for_start}{Logical value, indicating if we only want to wait for
the task to \emph{start} rather than complete. This will block until
the task moves away from \code{submitted}, and will return when it
takes the status \code{running} or any terminal status (\code{success},
\code{failure}, \code{cancelled}). Note that this does not guarantee that
your task will still be running by the time \code{task_wait} exits,
your task may have finished by then!}

\item{timeout}{The time to wait for the task to complete. The
default is to wait forever.}

\item{poll}{Time, in seconds, used to throttle calls to the status
function. The default is 1 second}

\item{progress}{Logical value, indicating if a progress spinner
should be used. The default \code{NULL} uses the option
\code{hipercow.progress}, and if unset displays a progress bar in an
interactive session.}

\item{follow}{Logical, indicating if we should follow any retried
tasks.}

\item{root}{A hipercow root, or path to it. If \code{NULL} we search up
your directory tree.}
}
\value{
Logical value, \code{TRUE} if the task completed successfully,
\code{FALSE} otherwise.
}
\description{
Wait for a single task to complete (or to start).  This function
is very similar to \link{task_log_watch}, except that it errors if the
task does not complete (so that it can be used easily to ensure a
task has completed) and does not return any logs.
}
\details{
The progress spinners here come from the cli package and will
respond to cli's options. In particular \code{cli.progress_clear} and
\code{cli.progress_show_after}.
}
\examples{
cleanup <- hipercow_example_helper()

id <- task_create_expr(sqrt(2))
task_wait(id)

cleanup()
}
