test_that("can submit a task via a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()

  init_quietly(path_here)
  init_quietly(path_there)

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  expect_message(
    id <- withr::with_dir(path_here, task_create_explicit(quote(getwd()))),
    "Submitted task '[[:xdigit:]]{32}' using 'elsewhere'")
  expect_equal(task_status(id, root = path_here), "submitted")

  expect_equal(
    readLines(
      file.path(path_here, "hipercow", "tasks", id, "status-submitted")),
    "elsewhere")

  expect_true(file.exists(file.path(path_there, "hipercow", "tasks", id)))
  expect_equal(dir(file.path(path_there, "hipercow", "tasks", id)), "expr")
  expect_equal(readLines(file.path(path_there, "elsewhere.queue")), id)

  expect_true(withr::with_dir(path_there, task_eval(id)))

  expect_false(
    file.exists(file.path(path_here, "hipercow", "tasks", id, STATUS_SUCCESS)))
  expect_false(
    file.exists(file.path(path_here, "hipercow", "tasks", id, INFO)))

  expect_equal(task_status(id, root = path_here), "success")
  expect_true(
    file.exists(file.path(path_here, "hipercow", "tasks", id, STATUS_SUCCESS)))
  expect_true(
    file.exists(file.path(path_here, "hipercow", "tasks", id, INFO)))
  expect_true(
    id %in% names(hipercow_root(path_here)$cache$task_status_terminal))
})


test_that("forbid additional arguments to submission, for now", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    task_create_explicit(quote(getwd()), submit = FALSE))
  expect_error(
    withr::with_dir(path_here, task_submit(id, cores = 2)),
    "Additional arguments to 'task_submit' not allowed")
})


test_that("fetch driver used for submission", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()

  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  id1 <- withr::with_dir(
    path_here,
    task_create_explicit(quote(getwd()), submit = FALSE))
  id2 <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(getwd()), submit = TRUE)))

  expect_equal(task_get_driver(id1, root = path_here), NA_character_)
  expect_equal(task_get_driver(id2, root = path_here), "elsewhere")
  expect_equal(root$cache$task_driver, set_names("elsewhere", id2))
  ## Works from the cache, too
  expect_equal(task_get_driver(id1, root = path_here), NA_character_)
  expect_equal(task_get_driver(id2, root = path_here), "elsewhere")
})


test_that("knowing driver stops refetching from disk", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(getwd()))))

  mock_read_lines <- mockery::mock("foo", "bar")
  mockery::stub(task_get_driver, "readLines", mock_read_lines)
  expect_equal(task_get_driver(id, root = path_here), "foo")
  expect_equal(task_get_driver(id, root = path_here), "foo")
  mockery::expect_called(mock_read_lines, 1)
})


test_that("can retrieve a task result via a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(getwd()))))
  expect_error(
    task_result(id, root = path_here),
    "Result for task '[[:xdigit:]]{32}' not available, status is 'submitted'")
  expect_true(withr::with_dir(path_there, task_eval(id)))
  expect_equal(
    task_result(id, root = path_here),
    normalize_path(path_there))
  expect_true(file.exists(
    file.path(path_here, "hipercow", "tasks", id, "result")))
})


test_that("can call provision", {
  elsewhere_register()
  mock_provision <- mockery::mock()
  cache$drivers$elsewhere$provision_run <- mock_provision
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  writeLines('install.packages("R6")', file.path(path_here, "provision.R"))

  path_root <- root$path$root
  config <- root$config$elsewhere

  hipercow_provision(root = path_here, show_log = FALSE)
  mockery::expect_called(mock_provision, 1)
  environment <- new_environment("default", NULL, NULL, NULL, NULL)
  expect_equal(
    mockery::mock_args(mock_provision)[[1]],
    list(list(method = NULL, environment = environment, show_log = FALSE),
         config, path_root))
})


test_that("can call provision_list", {
  elsewhere_register()
  mock_provision_list <- mockery::mock()
  cache$drivers$elsewhere$provision_list <- mock_provision_list
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  path_root <- root$path$root
  config <- root$config$elsewhere

  hipercow_provision_list(root = path_here)
  mockery::expect_called(mock_provision_list, 1)
  expect_equal(
    mockery::mock_args(mock_provision_list)[[1]],
    list(NULL, config, path_root))
})


test_that("can call provision_check", {
  elsewhere_register()
  mock_provision_list <- mockery::mock()
  cache$drivers$elsewhere$provision_list <- mock_provision_list
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  path_root <- root$path$root
  config <- root$config$elsewhere
  env <- new_environment("default", NULL, NULL, NULL, NULL)

  hipercow_provision_check(root = path_here)
  mockery::expect_called(mock_provision_list, 1)
  expect_equal(
    mockery::mock_args(mock_provision_list)[[1]],
    list(list(method = NULL, environment = env), config, path_root))

  hipercow_provision_check(method = "script", script = "foo.R",
                           root = path_here)
  mockery::expect_called(mock_provision_list, 2)
  expect_equal(
    mockery::mock_args(mock_provision_list)[[2]],
    list(list(method = "script", environment = env, script = "foo.R"),
         config, path_root))
})


test_that("can call provision_compare", {
  elsewhere_register()
  mock_provision_compare <- mockery::mock()
  cache$drivers$elsewhere$provision_compare <- mock_provision_compare
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  path_root <- root$path$root
  config <- root$config$elsewhere

  hipercow_provision_compare(root = path_here)
  mockery::expect_called(mock_provision_compare, 1)
  expect_equal(
    mockery::mock_args(mock_provision_compare)[[1]],
    list(config, path_root, 0, -1))
})


test_that("can cancel tasks", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(sqrt(2)))))

  expect_equal(task_status(id, root = path_here), "submitted")
  res <- evaluate_promise(task_cancel(id, root = path_here))
  expect_true(res$result)
  expect_match(res$messages, sprintf("Successfully cancelled '%s'", id))

  res <- evaluate_promise(task_cancel(id, root = path_here))
  expect_false(res$result)
  expect_match(res$messages, sprintf("Did not try to cancel '%s'", id))
  expect_error(
    withr::with_dir(path_here, task_eval(id)),
    "Can't start task '[[:xdigit:]]{32}', which has status 'cancelled'")

  path_info <- file.path(path_here, "hipercow", "tasks", id, "info")
  expect_true(file.exists(path_info))
  info <- readRDS(path_info)
  expect_equal(names(info),
               c("status", "times", "cpu", "memory"))
  expect_equal(info$status, "cancelled")
  expect_equal(names(info$times), c("created", "started", "finished"))
  expect_s3_class(info$times, "POSIXct")
  expect_equal(is.na(info$times),
               c(created = FALSE, started = TRUE, finished = FALSE))
  expect_null(info$cpu)
  expect_null(info$memory)
})


test_that("Can submit zero tasks silently", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  expect_silent(withr::with_dir(path_here, task_submit(character())))
})


test_that("can submit a bunch of tasks at once", {
  withr::local_options(cli.progress_show_after = 0)

  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  ids <- character(20)
  for (i in seq_along(ids)) {
    ids[[i]] <- withr::with_dir(
      path_here,
      task_create_explicit(quote(getwd()), submit = FALSE))
  }

  res <- evaluate_promise(task_submit(ids, root = path_here))
  expect_null(res$result)
  expect_match(res$messages[[1]], "Submitting tasks")
  expect_match(res$messages[[length(res$messages)]],
               "Submitted 20 tasks using 'elsewhere'")
})


test_that("can't submit task with no driver set up", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(sqrt(1)), submit = NULL))
  expect_equal(task_status(id, root = path), "created")
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(sqrt(1)), submit = FALSE))
  expect_equal(task_status(id, root = path), "created")
  err <- withr::with_dir(
    path,
    expect_error(
      task_create_explicit(quote(sqrt(1)), submit = TRUE),
      "Can't submit task because no driver configured"))
  expect_equal(
    err$body,
    c(i = "Run 'hipercow::hipercow_configure()' to configure a driver"))
})


test_that("tasks autosubmit by default", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(sqrt(1)), submit = NULL)))
  expect_equal(task_status(id, root = path_here), "submitted")
  id <- withr::with_dir(
    path_here,
    task_create_explicit(quote(sqrt(1)), submit = FALSE))
  expect_equal(task_status(id, root = path_here), "created")
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(sqrt(1)), submit = TRUE)))
  expect_equal(task_status(id, root = path_here), "submitted")
})


test_that("prevent autosubmission when more than one driver configured", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  root <- hipercow_root(path_here)
  root$config <- c(root$config, list(other = list()))
  expect_error(
    withr::with_dir(
      path_here,
      task_create_explicit(quote(sqrt(1)), submit = NULL)),
    "Can't cope with more than one driver configured yet")
  expect_error(
    withr::with_dir(
      path_here,
      task_create_explicit(quote(sqrt(1)), submit = TRUE)),
    "Can't cope with more than one driver configured yet")
  id <- withr::with_dir(
    path_here,
    task_create_explicit(quote(sqrt(1)), submit = FALSE))
  expect_message(
    task_submit(id, driver = "elsewhere", root = root),
    "Submitted task")
  expect_equal(task_status(id, root = root), "submitted")
})

test_that("can read logs", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  suppressMessages(
    id <- withr::with_dir(path_here, task_create_explicit(quote(sqrt(2)))))

  expect_null(task_log_value(id, root = path_here))
  expect_message(task_log_show(id, root = path_here),
                 "No logs for task '.+'")

  path_log <- file.path(path_there, "hipercow", "tasks", id, "elsewhere_log")

  file.create(path_log)
  expect_equal(task_log_value(id, root = path_here), character())
  expect_message(task_log_show(id, root = path_here),
                 "Empty logs for task '.+'")

  writeLines(c("a", "b"), path_log)
  expect_equal(task_log_value(id, root = path_here), c("a", "b"))
  expect_output(task_log_show(id, root = path_here),
                "a\nb")
})


test_that("can wait on a task, returning immediately", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(sqrt(1)))))
  expect_error(
    task_wait(id, root = path_here, timeout = 0, progress = FALSE),
    "Task '.+' did not complete in time")
  task_eval(id, root = path_there)
  expect_true(task_wait(id, root = path_here, timeout = 0, progress = FALSE))
})


test_that("can run a task without loading a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  id <- withr::with_dir(
    path_here,
    suppressMessages(task_create_explicit(quote(sqrt(2)))))

  cache$allow_load_drivers <- NULL
  withr::defer(cache$allow_load_drivers <- NULL)
  withr::local_envvar("HIPERCOW_NO_DRIVERS" = 1)

  expect_equal(task_status(id, root = path_here), "submitted")
  expect_error(
    task_result(id, root = path_here),
    "Result for task '.+' not available, status is 'submitted'")
  expect_false(cache$allow_load_drivers)

  expect_true(task_eval(id, root = path_here))
  expect_equal(task_status(id, root = path_here), "success")
  expect_equal(task_result(id, root = path_here), sqrt(2))
})


test_that("Can watch logs", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  suppressMessages(
    id <- withr::with_dir(path_here, task_create_explicit(quote(sqrt(2)))))

  mock_status <- mockery::mock("running", "running", "success")
  mock_log <- mockery::mock(
    "a", c("a", "b", "c"), c("a", "b", "c", "d", "e"))
  cache$drivers$elsewhere$log <- mock_log
  cache$drivers$elsewhere$status <- mock_status

  res <- evaluate_promise(
    task_log_watch(id, root = path_here, poll = 0))
  expect_true(res$result)
  expect_equal(res$messages, c("a\n", "b\nc\n", "d\ne\n"))
})


test_that("can get task info", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  suppressMessages(
    id <- withr::with_dir(path_here, task_create_expr(runif(1))))
  info <- task_info(id, root = path_here)
  expect_equal(info$status, "submitted")
})


test_that("can fix invalid info status via info", {
  elsewhere_register()
  mock_info <- mockery::mock(list(status = "running"),
                             list(status = "failure"))
  cache$drivers$elsewhere$info <- mock_info

  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  suppressMessages(
    id <- withr::with_dir(path_here, task_create_expr(runif(1))))
  res1 <- evaluate_promise(task_info(id, root = path_here))
  res2 <- evaluate_promise(task_info(id, root = path_here))

  ## No update here at the moment to the real status; we could update
  ## to "running" but odds are that the second call to task_status
  ## will do this.
  expect_length(res1$messages, 0)
  expect_equal(res1$result$status, "submitted")

  expect_length(res2$messages, 4)
  expect_match(res2$messages[[1]], "Fixing status of")
  expect_match(res2$messages[[2]], "If this is unexpected")
  expect_equal(res2$result$status, "failure")

  expect_true(file.exists(
    file.path(path_here, "hipercow", "tasks", id, "info")))
  expect_true(file.exists(
    file.path(path_here, "hipercow", "tasks", id, "result")))
  expect_true(file.exists(
    file.path(path_here, "hipercow", "tasks", id, "status-failure")))
  expect_equal(task_result(id, root = path_here),
               simpleError("task reported as lost"))
})
