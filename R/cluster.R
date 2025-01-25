##' Describe information about the cluster.  This is (naturally) very
##' dependent on the cluster but some details of the value are
##' reliable; see Value for details.
##'
##' @title Describe cluster
##'
##' @param driver The driver to use, which determines the cluster to
##'   fetch information from (depending on your configuration).  If no
##'   driver is configured, an error will be thrown.
##'
##' @param platform The operating system you are intending to run
##'   jobs on. By default this is `windows`, which we have traditionally
##'   supported, but the MS-HPC cluster also can support `linux` nodes.
##'   Where hipercow cluster drivers support multiple operating systems,
##'   you can specify which platform you would like information on here.
##'
##' @inheritParams hipercow_configure
##'
##' @return A list describing the cluster.  The details depend on the
##'   driver, and are subject to change.  We expect to see elements:
##'
##' * resources: Describes the computational resources on the cluster,
##'   which is used by [hipercow_resources_validate].  Currently this
##'   is a simple list with elements `max_ram` (max RAM available, in
##'   GB), `max_cores` (max number of cores you can request), `queues`
##'   (character vector of available queues), `nodes` (character
##'   vector of available nodes), `default_queue` (the default queue).
##'   These details are subject to change but the contents should
##'   always be informative and fairly self explanatory.
##' * redis_url: The URL of the redis server to communicate with from
##'   outside of the cluster (i.e., from your computer), in a form
##'   suitable for use with redux::hiredis
##' * r_versions: A vector of R versions, as `numeric_vector` objects
##'
##' @export
##' @examples
##' cleanup <- hipercow_example_helper()
##' hipercow_cluster_info()
##' cleanup()
hipercow_cluster_info <- function(driver = NULL, root = NULL,
                                  platform = "windows") {
  root <- hipercow_root(root)
  driver <- hipercow_driver_select(driver, TRUE, root, call)
  cluster_info(driver, root, platform)
}


cluster_info <- function(driver, root, platform = "windows") {
  dat <- hipercow_driver_prepare(driver, root, rlang::current_env())
  dat$driver$cluster_info(dat$config, root$path$root, platform)
}
