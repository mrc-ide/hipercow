% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bundle.R
\name{hipercow_bundle_retry}
\alias{hipercow_bundle_retry}
\title{Retry task bundle}
\usage{
hipercow_bundle_retry(bundle, if_status_in = NULL, driver = NULL, root = NULL)
}
\arguments{
\item{bundle}{Either a \code{hipercow_bundle} object, or the name of a
bundle.}

\item{if_status_in}{Optionally, a character vector of task
statuses for which we should retry tasks.  For example, pass
\code{if_status_in = c("cancelled", "failure")} to retry cancelled
and failed tasks.  Can only be terminal statuses (\code{cancelled},
\code{failure}, \code{success}).}

\item{driver}{Name of the driver to use to submit the task.  The
default (\code{NULL}) depends on your configured drivers; if you have
no drivers configured no submission happens (or indeed is
possible).  If you have exactly one driver configured we'll
submit your task with it.  If you have more than one driver
configured, then we will error, though in future versions we may
fall back on a default driver if you have one configured.  If
you pass \code{FALSE} here, submission is prevented even if you have
no driver configured.}

\item{root}{A hipercow root, or path to it. If \code{NULL} we search up
your directory tree.}
}
\value{
Invisibly, a logical vector, indicating which of the tasks
within the bundle were retried. This means that it's not
immediately obvious how you can get the new id back from the
tasks, but typically that is unimportant, as all bundle
functions follow retries by default.
}
\description{
Retry tasks in a bundle.  This has slightly different semantics to
\code{\link[=task_retry]{task_retry()}}, which errors if a retry is not possible.  Here, we
anticipate that much of the time you will be interested in
retrying some fraction of your bundle and so don't need to wait
until all tasks have finished in order to retry failed tasks.
}
\examples{
cleanup <- hipercow_example_helper()
bundle <- task_create_bulk_expr(rnorm(1, x), data.frame(x = 1:5))
hipercow_bundle_wait(bundle)

retried <- hipercow_bundle_retry(bundle)
retried
hipercow_bundle_wait(bundle)
hipercow_bundle_result(bundle, follow = FALSE)
hipercow_bundle_result(bundle, follow = TRUE)

cleanup()
}
