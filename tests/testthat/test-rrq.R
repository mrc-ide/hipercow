test_that("can create a rrq controller", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    r1 <- hipercow_rrq_controller(root = path),
    "Created new rrq queue")
  expect_message(
    r2 <- hipercow_rrq_controller(root = path),
    "Using existing rrq queue")

  expect_true("hipercow" %in% rrq::rrq_worker_config_list())
  cfg <- rrq::rrq_worker_config_read("hipercow")
  expect_false(cfg$verbose)
  expect_equal(cfg$heartbeat_period, 60)
  expect_equal(cfg$timeout_idle, 300)
})


test_that("using rrq requires a driver that supports it", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  expect_error(
    hipercow_rrq_controller(root = path_here),
    "No redis support for 'elsewhere'")
})


test_that("configuring a task with use_rrq sets a default controller", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  withr::defer(rrq::rrq_default_controller_clear())

  r <- suppressMessages(
    hipercow_rrq_controller(set_as_default = FALSE, root = path))

  expect_error(rrq::rrq_task_list(), "Default controller not set")

  parallel <- hipercow_parallel(use_rrq = TRUE)
  id <- withr::with_dir(
    path,
    suppressMessages(
      task_create_explicit(quote(rrq::rrq_task_list()), parallel = parallel)))

  res <- suppressMessages(task_eval(id, new.env(), root = path))
  expect_true(res)
  expect_equal(task_result(id, root = path), character())
})


test_that("setting up a task with use_rrq requires a driver", {
  path <- withr::local_tempdir()
  init_quietly(path)
  parallel <- hipercow_parallel(use_rrq = TRUE)
  expect_error(
    withr::with_dir(path,
                    task_create_explicit(quote(sqrt(2)), parallel = parallel)),
    "You have set 'use_rrq = TRUE' but no driver is configured")
})


test_that("setting up a task with use_rrq a controller configured", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  parallel <- hipercow_parallel(use_rrq = TRUE)
  expect_error(
    withr::with_dir(path,
                    task_create_explicit(quote(sqrt(2)), parallel = parallel)),
    "You have set 'use_rrq = TRUE' but have not yet configured rrq")
})


test_that("can't submit workers from below root", {
  skip_if_no_redis()

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  path_sub <- file.path(path, "sub")
  dir.create(path_sub)

  expect_error(
    withr::with_dir(
      path_sub,
      suppressMessages(hipercow_rrq_workers_submit(1, timeout = 0))),
    "Can't submit workers from below hipercow root")
})


test_that("can load rrq environment by preference", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  writeLines("f <- function() 1", file.path(path, "a.R"))
  writeLines("f <- function() 2", file.path(path, "b.R"))

  init_quietly(path, driver = "example")

  suppressMessages({
    hipercow_environment_create("default", sources = "b.R", root = path)
  })
  e <- new.env()
  withr::with_dir(path, hipercow_rrq_envir(e))
  expect_equal(e$f(), 2)

  suppressMessages({
    hipercow_environment_create("rrq", sources = "a.R", root = path)
  })
  e <- new.env()
  withr::with_dir(path, hipercow_rrq_envir(e))
  expect_equal(e$f(), 1)
})


## Tests below here launch workers.  They will be fragile and annoying
## to set up.
test_that("can spawn and use a worker", {
  ## This test does quite a bit to make sure that everything is ok,
  ## acting as an integration test.
  skip_if_not_installed("callr")
  skip_if_no_redis()

  path <- withr::local_tempdir()
  writeLines("f <- function() 1", file.path(path, "a.R"))
  writeLines("f <- function() 2", file.path(path, "b.R"))

  init_quietly(path, driver = "example")
  suppressMessages({
    hipercow_environment_create("rrq", sources = "a.R", root = path)
    hipercow_environment_create("default", sources = "b.R", root = path)
  })

  launch_example_workers(path)

  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    r <- hipercow_rrq_controller(root = path),
    "Created new rrq queue")
  suppressMessages(
    info <- withr::with_dir(path, hipercow_rrq_workers_submit(1)))

  expect_s3_class(info, "data.frame")
  expect_setequal(names(info),
                  c("queue_id", "worker_id", "task_id", "bundle_name"))
  expect_equal(info$queue_id, r$queue_id)
  expect_equal(rrq::rrq_worker_list(), info$worker_id)

  id <- rrq::rrq_task_create_expr(sqrt(2))
  expect_true(rrq::rrq_task_wait(id))
  expect_equal(rrq::rrq_task_result(id), sqrt(2))

  id <- rrq::rrq_task_create_expr(f())
  expect_true(rrq::rrq_task_wait(id))
  expect_equal(rrq::rrq_task_result(id), 1)

  rrq::rrq_worker_stop()
})


test_that("can submit workers with envvars", {
  skip_if_not_installed("callr")
  skip_if_no_redis()

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")

  launch_example_workers(path)

  e1 <- hipercow_envvars(X = "foo")
  e2 <- hipercow_envvars(Y = "bar", secret = TRUE)
  envvars <- c(e1, e2)

  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    hipercow_rrq_controller(root = path),
    "Created new rrq queue")
  suppressMessages(
    info <- withr::with_dir(path,
      hipercow_rrq_workers_submit(1, envvars = envvars)))

  id <- rrq::rrq_task_create_expr(Sys.getenv("X"))
  expect_true(rrq::rrq_task_wait(id))
  expect_equal(rrq::rrq_task_result(id), "foo")

  id <- rrq::rrq_task_create_expr(Sys.getenv("Y"))
  expect_true(rrq::rrq_task_wait(id))
  expect_equal(rrq::rrq_task_result(id), "bar")

  rrq::rrq_worker_stop()
})


test_that("can start rrq tasks from a hipercow task", {
  skip_if_not_installed("callr")
  skip_if_no_redis()

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  launch_example_workers(path, n = 2)

  expect_message(
    hipercow_rrq_controller(root = path),
    "Created new rrq queue")

  suppressMessages(withr::with_dir(path, {
    info <- hipercow_rrq_workers_submit(1)
    withr::defer(rrq::rrq_worker_stop())

    id <- task_create_expr({
      id <- rrq::rrq_task_create_expr(sqrt(2))
      rrq::rrq_task_wait(id)
      rrq::rrq_task_result(id)
    }, parallel = hipercow_parallel(use_rrq = TRUE))
  }))

  expect_true(task_wait(id, root = path))
  expect_equal(task_result(id, root = path), sqrt(2))
})


test_that("can nest rrq task starts", {
  skip_if_not_installed("callr")
  skip_if_no_redis()
  skip_on_covr() # Not sure why this one fails; it's during worker submit

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  launch_example_workers(path)

  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    hipercow_rrq_controller(root = path),
    "Created new rrq queue")

  parallel <- hipercow_parallel(use_rrq = TRUE)
  suppressMessages({
    info <- withr::with_dir(path,
      hipercow_rrq_workers_submit(1, parallel = parallel))
  })
  withr::defer(rrq::rrq_worker_stop())

  id <- rrq::rrq_task_create_expr({
    rrq::rrq_task_create_expr({
      rrq::rrq_task_create_expr(sqrt(2))
    })
  })

  expect_true(rrq::rrq_task_wait(id))
  id2 <- rrq::rrq_task_result(id)

  expect_true(rrq::rrq_task_wait(id2))
  id3 <- rrq::rrq_task_result(id2)

  expect_true(rrq::rrq_task_wait(id3))
  expect_equal(rrq::rrq_task_result(id3), sqrt(2))
})

test_that("can use rrq offload", {
  skip_if_not_installed("callr")
  skip_if_no_redis()

  withr::local_options("hipercow.rrq_offload_threshold_size" = 100)

  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  launch_example_workers(path, n = 1)

  suppressMessages({
    r <- hipercow_rrq_controller(root = path)
    info <- withr::with_dir(path, hipercow_rrq_workers_submit(1))
    withr::defer(rrq::rrq_worker_stop())
  })

  id <- rrq::rrq_task_create_expr(rep(1, 1000))
  rrq::rrq_task_wait(id)
  expect_equal(rrq::rrq_task_result(id), rep(1, 1000))

  # Make sure we did in fact use the offload
  hashes <- r$store$list()
  expect_length(hashes, 1)
  expect_equal(r$store$location(hashes), "offload")
  expect_equal(r$store$get(hashes), rep(1, 1000))
})


test_that("refresh worker environment when updating rrq", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  withr::defer(rrq::rrq_default_controller_clear())
  writeLines("a <- 1", file.path(path, "src.R"))
  msg <- capture_messages(
    hipercow_environment_create("rrq", sources = "src.R", root = path))
  expect_length(msg, 1)
  expect_match(msg[[1]], "Created environment 'rrq'")

  expect_message(
    r <- hipercow_rrq_controller(root = path),
    "Created new rrq queue")

  msg <- capture_messages(
    hipercow_environment_create("rrq", sources = "src.R", root = path))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Environment 'rrq' is unchanged")
  expect_match(msg[[2]], "Refreshing existing rrq worker environments")
  expect_match(msg[[3]], "Using existing rrq queue")
})


test_that("can detect failed workers", {
  ## Another integration test
  skip_if_not_installed("callr")
  skip_if_no_redis()
  skip_on_covr()

  path <- withr::local_tempdir()
  writeLines("f <- function() 1", file.path(path, "fns.R"))

  init_quietly(path, driver = "example")
  suppressMessages({
    hipercow_environment_create(sources = "fns.R", root = path)
  })

  writeLines("f <- function(", file.path(path, "fns.R"))

  launch_example_workers(path)

  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    r <- hipercow_rrq_controller(root = path),
    "Created new rrq queue")
  err <- expect_error(
    suppressMessages(
      info <- withr::with_dir(path, hipercow_rrq_workers_submit(1))),
    "Worker died")
})


test_that("check that we can migrate old queues", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  withr::defer(rrq::rrq_default_controller_clear())

  expect_message(
    hipercow_rrq_controller(root = path),
    "Created new rrq queue")
  r <- hipercow_root(path)

  path_queue_id <- file.path(r$path$rrq, setdiff(dir(r$path$rrq), "offload"))
  path_legacy <- file.path(r$path$rrq, "example")

  fs::file_move(path_queue_id, path_legacy)

  msg <- testthat::capture_messages(hipercow_rrq_controller(root = path))
  expect_match(msg[[1]],
               "Migrating rrq queue configuration from legacy version")
  expect_match(msg[[2]],
               "Using existing rrq queue")
})


test_that("can tolerate old queue with no remote setup", {
  skip_if_no_redis()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  withr::defer(rrq::rrq_default_controller_clear())

  r <- hipercow_root(path)
  path_legacy <- file.path(r$path$rrq, "example")
  fs::dir_create(r$path$rrq)

  writeLines(paste0("rrq:", ids::random_id(bytes = 8)), path_legacy)

  msg <- testthat::capture_messages(hipercow_rrq_controller(root = path))
  expect_match(msg[[1]], "Ignoring legacy rrq queue configuration")
  expect_match(msg[[2]], "Created new rrq queue")
})
