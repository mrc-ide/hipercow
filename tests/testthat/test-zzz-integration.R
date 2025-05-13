test_that("can provision a library", {
  testthat::skip_if_offline()
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  writeLines('install.packages("R6")', file.path(path_here, "provision.R"))
  suppressMessages(hipercow_provision(root = path_here, show_log = FALSE))
  expect_true(file.exists(file.path(path_there, "hipercow", "lib", "R6")))
})


test_that("Can run task in parallel", {
  path <- withr::local_tempdir()
  init_quietly(path)
  parallel <- hipercow_parallel("parallel")
  resources <- hipercow_resources(cores = 2)

  id <- withr::with_dir(
    path,
    suppressMessages(
      task_create_expr(
        parallel::clusterApply(NULL, 1:2, function(x) list(x, Sys.getpid())),
        parallel = parallel,
        resources = resources)))
  dat <- readRDS(path_to_task_file(path, id, "data"))
  expect_null(dat$variables)
  expect_equal(dat$parallel$method, "parallel")

  res <- evaluate_promise(
    withr::with_envvar(
      c(HIPERCOW_CORES = 2),
      withr::with_dir(path,  task_eval(id, root = path))))
  expect_length(res$messages, 4)
  expect_match(res$messages[[1]],
               "Creating a parallel cluster with 2 processes")
  expect_match(res$messages[[2]],
               "Cluster ready to use")
  expect_match(res$messages[[3]],
               "Stopping cluster")
  expect_match(res$messages[[4]],
               "Cluster stopped")

  expect_true(res$result)

  res <- task_result(id, root = path)
  expect_equal(res[[1]][[1]], 1)
  expect_equal(res[[2]][[1]], 2)
  expect_true(res[[1]][[2]] != res[[2]][[2]])
})


test_that("Can turn off workers by message", {
  skip_if_not_installed("callr")
  skip_if_no_redis()

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  suppressMessages({
    r <- hipercow_rrq_controller(root = path)
    cfg <- rrq::rrq_worker_config_read("hipercow", controller = r)
    cfg$poll_queue <- 1
    rrq::rrq_worker_config_save("hipercow", cfg, controller = r)
    launch_example_workers(path)
    info <- withr::with_dir(path, hipercow_rrq_workers_submit(1))
    id <- info$worker_id
  })

  msg <- capture_messages(hipercow_rrq_stop_workers_once_idle(path))
  expect_length(msg, 4)
  expect_match(msg[[1]], "Using existing rrq queue")
  expect_match(msg[[2]], "Sent message to 1 worker")
  expect_match(msg[[3]], "Workers will stop 1 second after their last task")
  expect_match(msg[[4]], "Current worker status: IDLE (1)", fixed = TRUE)

  Sys.sleep(2)
  expect_equal(unname(rrq::rrq_worker_status(id)), "EXITED")
  logs <- rrq::rrq_worker_log_tail(id, n = 4)
  expect_equal(logs$command,
               c("MESSAGE", "RESPONSE", "HEARTBEAT", "STOP"))
  expect_equal(logs$message,
               c("TIMEOUT_SET", "TIMEOUT_SET", "stopping", "OK (TIMEOUT)"))

  msg <- capture_messages(hipercow_rrq_stop_workers_once_idle(path))
  expect_length(msg, 2)
  expect_match(msg[[1]], "Using existing rrq queue")
  expect_match(msg[[2]], "No workers to send messages to")
})
