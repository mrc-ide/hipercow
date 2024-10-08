##' Create an rrq controller for your queue, and set it as the default
##' controller.  Use this to interact with workers created with
##' [hipercow_rrq_workers_submit()].  Proper docs forthcoming, all
##' interfaces are subject to some change.
##'
##' @title Create an rrq controller
##'
##' @param ... Additional arguments passed through to
##'   [rrq::rrq_controller()]; currently this is `follow` and
##'   `timeout_task_wait`.
##'
##' @param driver Name of the driver to use.  The default (`NULL`)
##'   depends on your configured drivers; if you have no drivers
##'   configured we will error as we lack information required to
##'   proceed.  If you have exactly one driver configured we'll submit
##'   your task with it.  If you have more than one driver configured,
##'   then we will error, though in future versions we may fall back
##'   on a default driver if you have one configured.
##'
##' @param set_as_default Set the rrq controller to be the default;
##'   this is usually what you want.
##'
##' @param queue_id The rrq queue id to use. You shouldn't need to pass a value
##'   for this: the queue id can be found from the hipercow state directory, or
##'   a new one is created if needed.
##'
##' @inheritParams task_create_expr
##'
##' @return An [rrq::rrq_controller] object.
##'
##' @export
hipercow_rrq_controller <- function(..., set_as_default = TRUE, driver = NULL,
                                    queue_id = NULL, root = NULL) {
  root <- hipercow_root(root)
  call <- rlang::current_env()
  if (is.null(queue_id)) {
    driver <- hipercow_driver_select(driver, TRUE, root, call)
    r <- rrq_prepare(driver, root, ..., call = call)
  } else {
    cli::cli_alert_success("Connecting to rrq queue '{queue_id}' from task")
    r <- rrq::rrq_controller(queue_id, con = redux::hiredis(), ...)
  }
  if (set_as_default) {
    rrq::rrq_default_controller_set(r)
  }
  r
}


##' Submit workers to the cluster, use this in conjunction with
##' [hipercow_rrq_controller].  A worker may sit on a single core or a
##' whole node depending on how you set up `resources`.  We use the
##' `rrq` environment if it exists ([hipercow_environment_create])
##' otherwise we'll use the `default` environment.
##'
##' @title Submit rrq workers
##'
##' @param n The number of workers to submit. This is the only
##'   required argument.
##'
##' @inheritParams hipercow_rrq_controller
##'
##' @param resources A list generated by [hipercow_resources] giving
##'   the cluster resource requirements to run your task.
##'
##' @param envvars Environment variables as generated by
##'   [hipercow_envvars], which you might use to control your task.
##'
##' @param parallel Parallel configuration as generated by
##'   [hipercow_parallel], which defines which method, if any, will be
##'   used to initialise your worker for parallel execution (which
##'   means you have to think about parallelism at three levels at
##'   least, a diagram may help here).
##'
##' @param timeout Time to wait for workers to appear.
##'
##' @param progress Should we display a progress bar?
##'
##' @inheritParams task_eval
##'
##' @return A data.frame with information about the launch, with columns:
##'
##' * `queue_id`: the rrq queue id (same for all workers)
##' * `worker_id`: the rrq worker identifier
##' * `task_id`: the hipercow task identifier
##' * `bundle_name`: the hipercow bundle name (same for all workers)
##'
##' @export
hipercow_rrq_workers_submit <- function(n,
                                        driver = NULL, resources = NULL,
                                        envvars = NULL, parallel = NULL,
                                        timeout = NULL, progress = NULL,
                                        root = NULL) {
  root <- hipercow_root(root)
  call <- rlang::current_env()
  assert_scalar_integer(n, call = call)

  driver <- hipercow_driver_select(driver, TRUE, root, call)
  r <- rrq_prepare(driver, root, call = call)

  progress <- show_progress(progress, call)
  timeout <- timeout_value(timeout, call)

  path <- relative_workdir(root$path$root)
  if (path != ".") {
    cli::cli_abort(
      c("Can't submit workers from below hipercow root",
        i = paste("Your path relative to the hipercow root is '{path}' but",
                  "for now we need to start all workers from the root,",
                  "otherwise it creates issues for saving data."),
        i = paste("We plan on relaxing this soon; please let us know",
                  "that you have seen this message")))
  }

  queue_id <- r$queue_id
  worker_ids <- sprintf("rrq-%s-%s",
                        sub("^rrq:", "", queue_id),
                        ids::random_id(n, bytes = 6))
  args <- data.frame(queue_id = queue_id, worker_id = worker_ids)
  ## The messages here are mostly quite confusing, we might want to
  ## suppress them, I think.
  grp <- task_create_bulk_call(hipercow_rrq_worker,
                               args,
                               environment = "empty",
                               envvars = envvars,
                               parallel = parallel,
                               resources = resources,
                               driver = driver,
                               root = root)

  rrq::rrq_worker_wait(worker_ids, timeout = timeout, progress = progress,
                       controller = r)
  args$task_id <- grp$ids
  args$bundle_name <- grp$name
  args
}


rrq_prepare <- function(driver, root, ..., call = NULL) {
  ensure_package("rrq")
  ensure_package("redux")
  driver <- hipercow_driver_select(driver, TRUE, root, call)
  info <- cluster_info(driver, root)
  if (is.null(info$redis_url)) {
    cli::cli_abort("No redis support for '{driver}'")
  }
  con <- redux::hiredis(url = info$redis_url)
  path_queue_id <- file.path(root$path$rrq, driver)
  if (file.exists(path_queue_id)) {
    queue_id <- readLines(path_queue_id)
    cli::cli_alert_success("Using existing rrq queue '{queue_id}'")
    return(rrq::rrq_controller(queue_id, con, ...))
  }

  queue_id <- paste0("rrq:", ids::random_id(bytes = 4))

  ## TODO: Some hard coding here that needs a bit of work, though
  ## practically these can all be worked around after initialisation
  ## easily enough, except for store_max_size, which is fixed, I think
  ## (at least for now).
  store_max_size <- 100000 # 100k
  timeout_idle <- 300 # 5 minutes
  heartbeat_period <- 60 # one minute

  ## We can't _generally_ use an offload like this, though we can with
  ## the windows cluster.  This is something we'll have to think about
  ## fairly carefully, and it might be worth holding off until we have
  ## the linux cluster working properly, really.  We could switch this
  ## on for the windows and example driver and nothing else perhaps,
  ## but probably best if that is within the cluster info I think so
  ## we can look it up.
  withr::local_dir(root$path$root)
  offload_path <- file.path(root$path$rrq, "offload")
  rrq::rrq_configure(queue_id, con,
                     store_max_size = store_max_size,
                     offload_path = offload_path)

  r <- rrq::rrq_controller(queue_id, con, ...)

  cfg <- rrq::rrq_worker_config(timeout_idle = timeout_idle,
                                heartbeat_period = heartbeat_period,
                                verbose = FALSE)
  rrq::rrq_worker_config_save("hipercow", cfg, controller = r)

  ## We're going to hit the same issues here with making sure that any
  ## remote worker can read paths as we have with submitting general
  ## tasks; this will be easiest to think about once we have the ssh
  ## drivers all working.
  rrq::rrq_worker_envir_set(hipercow_rrq_envir, notify = FALSE, controller = r)
  fs::dir_create(dirname(path_queue_id))
  cli::cli_alert_success("Created new rrq queue '{queue_id}'")
  writeLines(queue_id, path_queue_id)
  r
}


hipercow_rrq_worker <- function(queue_id, worker_id) {
  ## nocov start
  w <- rrq::rrq_worker$new(queue_id,
                           name_config = "hipercow",
                           worker_id = worker_id)
  w$loop()
  ## nocov end
}


## TODO: we need a way of easily flagging that the environment should
## be reloaded, especially in cases where 'rrq' was created as an
## environment after 'default'; for later though.
hipercow_rrq_envir <- function(e) {
  root <- hipercow_root()
  name <- if (hipercow_environment_exists("rrq")) "rrq" else "default"
  environment_apply(name, e, root)
}
