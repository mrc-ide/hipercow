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

  res <- withr::with_envvar(
    c("HIPERCOW_RRQ_QUEUE_ID" = r$queue_id),
    suppressMessages(task_eval(id, new.env(), root = path)))
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

  w <- launch_example_workers(path)

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
