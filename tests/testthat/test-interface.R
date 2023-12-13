test_that("can submit a task via a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()

  init_quietly(path_here)
  init_quietly(path_there)

  hermod_configure("elsewhere", path = path_there, root = path_here)

  id <- withr::with_dir(path_here, hermod_task_create_explicit(quote(getwd())))
  expect_equal(hermod_task_status(id, root = path_here), "created")

  withr::with_dir(path_here, hermod_task_submit(id))

  expect_equal(hermod_task_status(id, root = path_here), "submitted")
  expect_equal(
    readLines(file.path(path_here, "hermod", "tasks", id, "status-submitted")),
    "elsewhere")

  expect_true(file.exists(file.path(path_there, "hermod", "tasks", id)))
  expect_equal(dir(file.path(path_there, "hermod", "tasks", id)), "expr")
  expect_equal(readLines(file.path(path_there, "elsewhere.queue")), id)

  expect_true(withr::with_dir(path_there, hermod_task_eval(id)))

  expect_false(
    file.exists(file.path(path_here, "hermod", "tasks", id, STATUS_SUCCESS)))

  expect_equal(hermod_task_status(id, root = path_here), "success")
  expect_true(
    file.exists(file.path(path_here, "hermod", "tasks", id, STATUS_SUCCESS)))
  expect_true(id %in% names(hermod_root(path_here)$cache$task_status_terminal))
})


test_that("forbid additional arguments to submission, for now", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  hermod_configure("elsewhere", path = path_there, root = path_here)
  id <- withr::with_dir(path_here, hermod_task_create_explicit(quote(getwd())))
  expect_error(
    withr::with_dir(path_here, hermod_task_submit(id, cores = 2)),
    "Additional arguments to 'hermod_task_submit' not allowed")
})


test_that("fetch driver used for submission", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()

  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)

  hermod_configure("elsewhere", path = path_there, root = path_here)

  id <- withr::with_dir(path_here, hermod_task_create_explicit(quote(getwd())))

  expect_equal(hermod_task_driver(id, root = path_here), NA_character_)

  withr::with_dir(path_here, hermod_task_submit(id))
  expect_equal(hermod_task_driver(id, root = path_here), "elsewhere")
  expect_equal(root$cache$task_driver, set_names("elsewhere", id))
  ## Works from the cache, too
  expect_equal(hermod_task_driver(id, root = path_here), "elsewhere")
})


test_that("knowning driver stops refetching from disk", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)
  hermod_configure("elsewhere", path = path_there, root = path_here)
  id <- withr::with_dir(path_here, hermod_task_create_explicit(quote(getwd())))
  withr::with_dir(path_here, hermod_task_submit(id))

  mock_read_lines <- mockery::mock("foo", "bar")
  mockery::stub(hermod_task_driver, "readLines", mock_read_lines)
  expect_equal(hermod_task_driver(id, root = path_here), "foo")
  expect_equal(hermod_task_driver(id, root = path_here), "foo")
  mockery::expect_called(mock_read_lines, 1)
})


test_that("can retrieve a task result via a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)
  hermod_configure("elsewhere", path = path_there, root = path_here)
  id <- withr::with_dir(path_here, hermod_task_create_explicit(quote(getwd())))
  withr::with_dir(path_here, hermod_task_submit(id))
  expect_error(
    hermod_task_result(id, root = path_here),
    "Result for task '[[:xdigit:]]{32}' not available, status is 'submitted'")
  expect_true(withr::with_dir(path_there, hermod_task_eval(id)))
  expect_equal(
    hermod_task_result(id, root = path_here),
    normalize_path(path_there))
  expect_true(file.exists(
    file.path(path_here, "hermod", "tasks", id, "result")))
})


test_that("can call provision", {
  elsewhere_register()
  mock_provision <- mockery::mock()
  cache$drivers$elsewhere$provision <- mock_provision
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)
  hermod_configure("elsewhere", path = path_there, root = path_here)
  writeLines('install.packages("R6")', file.path(path_here, "provision.R"))

  path_root <- root$path$root
  config <- root$config$elsewhere

  hermod_provision(root = path_here, show_log = FALSE)
  mockery::expect_called(mock_provision, 1)
  environment <- new_environment("default", NULL, NULL)
  expect_equal(
    mockery::mock_args(mock_provision)[[1]],
    list(NULL, config, path_root, environment, show_log = FALSE))
})



test_that("helper elsewhere driver can use callr to run a task immediately", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)
  hermod_configure("elsewhere", path = path_there, immediate = TRUE,
                   root = path_here)
  id <- withr::with_dir(
    path_here,
    hermod_task_create_explicit(quote(Sys.getpid())))
  withr::with_dir(path_here, hermod_task_submit(id))
  status <- wait_until_status(id, "terminal", root = path_here)
  expect_equal(status, "success")
  pid <- hermod_task_result(id, root = path_here)
  expect_true(pid != Sys.getpid())
})


test_that("can cancel long-running tasks", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hermod_root(path_here)
  hermod_configure("elsewhere", path = path_there, immediate = TRUE,
                   root = path_here)
  id <- withr::with_dir(
    path_here,
    hermod_task_create_explicit(quote(Sys.sleep(120))))
  expect_equal(hermod_task_status(id, root = path_here), "created")
  withr::with_dir(path_here, hermod_task_submit(id))
  expect_equal(hermod_task_status(id, root = path_here), "running")

  path_pid <- file.path(path_there, "hermod", "tasks", id, "pid")
  pid <- as.integer(readLines(path_pid))
  expect_true(pid %in% ps::ps_pids())

  expect_true(hermod_task_cancel(id, root = path_here))
  ## It can take a little while for the process to drop off the table,
  ## allow up to 5s for this
  testthat::try_again(100, {
    Sys.sleep(0.05)
    expect_false(pid %in% ps::ps_pids())
  })
  expect_equal(hermod_task_status(id, root = path_here), "cancelled")

  expect_false(hermod_task_cancel(id, root = path_here))
  expect_equal(hermod_task_status(id, root = path_here), "cancelled")
})
