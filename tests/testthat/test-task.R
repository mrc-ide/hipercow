test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(task_status(id, root = path), "created")

  expect_true(task_eval(id, root = path))
  expect_equal(task_status(id, root = path), "success")
  expect_equal(task_result(id, root = path), sqrt(2))

  info <- readRDS(path_to_task_file(path, id, "info"))
  expect_equal(names(info),
               c("status", "times", "cpu", "memory"))
  expect_equal(info$status, "success")
  expect_equal(names(info$times), c("created", "started", "finished"))
  expect_s3_class(info$times, "POSIXct")
  expect_s3_class(info$cpu, "proc_time")
  expect_equal(attributes(info$memory), attributes(gc(full = FALSE)))
})


test_that("can run a task that uses local variables", {
  env1 <- new.env()
  env2 <- new.env()
  env1$a <- 10
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(sqrt(a)), export = "a", envir = env1))
  expect_true(task_eval(id, envir = env2, root = path))
  expect_equal(task_result(id, root = path), sqrt(10))
  expect_equal(names(env2), "a")
  expect_equal(env2$a, 10)
})


test_that("tasks cannot be run twice", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_true(task_eval(id, root = path))
  expect_error(
    task_eval(id, root = path),
    "Can't start task '.+', which has status 'success'")
})


test_that("throw if result not available", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_error(
    task_result(id, root = path),
    "Result for task '.+' not available, status is 'created'")
})


test_that("return missing status for nonexisting tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- ids::random_id()
  expect_equal(task_status(id, root = path), NA_character_)
})


test_that("can run failing tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(readRDS("nofile.rds"))))
  expect_false(task_eval(id, root = path))
  result <- task_result(id, root = path)
  expect_s3_class(result, "error")
  expect_s3_class(result$trace, "rlang_trace")
  expect_length(result$warnings, 1)
  expect_s3_class(result$warnings[[1]], "warning")
})


test_that("tasks are saved with a directory", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(getwd()))
  expect_true(task_eval(id, root = path))
  expect_equal(normalize_path(task_result(id, root = path)),
               normalize_path(path))
})


test_that("that tasks run in subdirectories propagate that forward when run", {
  root <- withr::local_tempdir()
  init_quietly(root)
  path <- file.path(root, "a", "b")
  fs::dir_create(path)
  path <- normalize_path(path)
  id <- withr::with_dir(path, task_create_explicit(getwd()))
  expect_true(task_eval(id, root = root))
  expect_equal(normalize_path(task_result(id, root = root)), path)
})


test_that("refuse to create a task outside of the root", {
  root <- withr::local_tempdir()
  init_quietly(root)
  expect_error(
    task_create_explicit(getwd(), root = root),
    "Working directory is not a subdirectory of the hipercow root")
})


test_that("use cache to avoid looking up known terminal states", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  root <- hipercow_root(path)
  root$cache$task_status_terminal[[id]] <- "failure"
  expect_equal(task_status(id, root = path), "failure")
})


test_that("hipercow task status is vectorised", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, task_create_explicit(quote(sqrt(1))))
  id2 <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  task_eval(id2, root = path)

  expect_equal(task_status(character(), root = path), character(0))
  expect_equal(task_status(id1, root = path), "created")
  expect_equal(task_status(c(id1, id2), root = path),
               c("created", "success"))
  expect_equal(task_status(c(id1, id1), root = path),
               c("created", "created"))
})


test_that("protect against unknown task types", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  p <- path_to_task_file(path, id, "data")
  d <- readRDS(p)
  d$type <- "magic"
  saveRDS(d, p)
  expect_false(
    task_eval(id, root = path),
    "Tried to evaluate unknown type of task 'magic'")
  result <- task_result(id, root = path)
  expect_s3_class(result, "error")
  expect_s3_class(result$trace, "rlang_trace")
  expect_equal(result$message,
               "Tried to evaluate unknown type of task 'magic'")
})


test_that("can report about cancellation of individual ids", {
  expect_message(
    task_cancel_report("abc123", "created", TRUE, TRUE),
    "Successfully cancelled 'abc123'")
  expect_message(
    task_cancel_report("abc123", "created", FALSE, FALSE),
    "Did not try to cancel 'abc123' as it had status 'created'")
  expect_message(
    task_cancel_report("abc123", "created", FALSE, TRUE),
    "Did not manage to cancel 'abc123' which had status 'created'")
})


test_that("can report about cancellation of a group of ids", {
  n <- 10
  ids <- letters[seq_len(n)]
  status <- rep("running", length(ids))
  expect_silent(task_cancel_report(ids[0], status[0], logical(0), logical(0)))
  expect_message(
    task_cancel_report(ids, status, rep(TRUE, n), rep(TRUE, n)),
    "Successfully cancelled 10 tasks")
  expect_message(
    task_cancel_report(ids, status, rep(FALSE, n), rep(FALSE, n)),
    "Did not try to cancel any of 10 tasks as none were eligible")
  i <- rep(c(TRUE, FALSE), c(3, n - 3))
  expect_message(
    task_cancel_report(ids, status, i, i),
    "Successfully cancelled all 3 eligible tasks (of the 10 requested)",
    fixed = TRUE)
  i <- rep(c(TRUE, FALSE), c(1, n - 1))
  expect_message(
    task_cancel_report(ids, status, i, i),
    "Successfully cancelled the 1 eligible task (of the 10 requested)",
    fixed = TRUE)
  i <- rep(c(TRUE, FALSE), c(3, n - 3))
  j <- rep(c(TRUE, FALSE), c(4, n - 4))
  expect_message(
    task_cancel_report(ids, status, i, j),
    "Cancelled 3 of 4 eligible tasks (of the 10 requested)",
    fixed = TRUE)
  i <- rep(FALSE, n)
  j <- rep(c(TRUE, FALSE), c(4, n - 4))
  expect_message(
    task_cancel_report(ids, status, i, j),
    "Failed to cancel all 4 eligible tasks (of the 10 requested)",
    fixed = TRUE)
  i <- rep(FALSE, n)
  j <- rep(c(TRUE, FALSE), c(1, n - 1))
  expect_message(
    task_cancel_report(ids, status, i, j),
    "Failed to cancel the 1 eligible task (of the 10 requested)",
    fixed = TRUE)
})


test_that("cannot wait on a task that has not been submitted", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(identity(1))))
  err <- expect_error(task_wait(id, root = path),
                      "Cannot wait on task '.+', which has not been submitted")
  expect_equal(err$body,
               c(i = "You need to submit this task to wait on it"))
})


test_that("Can wait on a task", {
  mock_task_status <- mockery::mock(
    "submitted", "running", "running", "success")
  mockery::stub(task_wait, "task_status", mock_task_status)
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_true(task_wait(id, progress = FALSE, poll = 0, root = path))
  mockery::expect_called(mock_task_status, 4)
  expect_equal(
    mockery::mock_args(mock_task_status),
    rep(list(list(id, follow = FALSE, root = hipercow_root(path))), 4))
})


test_that("can map task status to logical for task_wait", {
  expect_equal(final_status_to_logical("submitted"), NA)
  expect_equal(final_status_to_logical("running"), NA)
  expect_equal(final_status_to_logical("success"), TRUE)
  expect_equal(final_status_to_logical("failure"), FALSE)
  expect_equal(final_status_to_logical("cancelled"), FALSE)
  expect_equal(final_status_to_logical("interrupt"), NA)
  expect_equal(final_status_to_logical("timeout"), NA)
  expect_error(final_status_to_logical("created"),
               "Unhandled status 'created'")
})


test_that("can map task status to logical for task_wait, running is final", {
  expect_equal(final_status_to_logical("submitted", TRUE), NA)
  expect_equal(final_status_to_logical("running", TRUE), TRUE)
  expect_equal(final_status_to_logical("success", TRUE), TRUE)
  expect_equal(final_status_to_logical("failure", TRUE), FALSE)
  expect_equal(final_status_to_logical("cancelled", TRUE), FALSE)
  expect_equal(final_status_to_logical("interrupt", TRUE), NA)
  expect_equal(final_status_to_logical("timeout", TRUE), NA)
  expect_error(final_status_to_logical("created", TRUE),
               "Unhandled status 'created'")
})


test_that("check that locals are not too big", {
  expect_no_error(check_locals_size(list()))
  expect_no_error(
    withr::with_options(list(hipercow.max_size_local = -Inf),
                        check_locals_size(list(a = 1))))
  expect_no_error(
    withr::with_options(list(hipercow.max_size_local = 10000),
                        check_locals_size(list(a = 1))))
  expect_error(
    withr::with_options(list(hipercow.max_size_local = 1),
                        check_locals_size(list(a = 1))),
    "Object too large to save with task: 'a'")
  expect_error(
    withr::with_options(list(hipercow.max_size_local = 1),
                        check_locals_size(list(a = 1, b = 2))),
    "Objects too large to save with task: 'a'.+ 'b'")
})


test_that("prevent large objects being saved by default", {
  withr::local_options(hipercow.max_size_local = NULL)

  path <- withr::local_tempdir()
  init_quietly(path)
  a <- runif(1e6)
  err <- expect_error(
    withr::with_dir(path, task_create_expr(mean(a))),
    "Object too large to save with task: 'a'")
  expect_match(err$body[[1]],
               "Objects saved with a hipercow task can only be 1 MB")
})


test_that("can be verbose running a task", {
  env1 <- new.env()
  env2 <- new.env()
  env1$a <- 10
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(sqrt(a)), export = "a", envir = env1))
  res <- evaluate_promise(
    task_eval(id, envir = env2, verbose = TRUE, root = path))
  expect_match(res$messages, "hipercow .+ running at", all = FALSE)
  expect_match(res$messages, "id: ", all = FALSE)
  expect_match(res$messages, "starting at: ", all = FALSE)
  expect_match(res$messages, "Task type: explicit", all = FALSE)
  expect_match(res$messages, "Expression: sqrt(a)", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "Locals: a", all = FALSE)
  expect_match(res$messages, "status: success", all = FALSE)
  expect_match(res$messages, "finishing at: ", all = FALSE)
})


test_that("can be verbose running a failing task", {
  env1 <- new.env()
  env2 <- new.env()
  env1$a <- 10
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(readRDS("nofile.rds"))))
  res <- evaluate_promise(
    task_eval(id, envir = env2, verbose = TRUE, root = path))
  expect_match(res$messages, "hipercow .+ running at", all = FALSE)
  expect_match(res$messages, "id: ", all = FALSE)
  expect_match(res$messages, "starting at: ", all = FALSE)
  expect_match(res$messages, "Task type: explicit", all = FALSE)
  expect_match(res$messages, 'Expression: readRDS("nofile.rds")',
               all = FALSE, fixed = TRUE)
  expect_match(res$messages, "Locals: (none)", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "status: failure", all = FALSE)
  expect_match(res$messages, "Error: ", all = FALSE)
  expect_match(res$messages, "1 warning found:", all = FALSE)
  expect_match(res$messages, "finishing at: ", all = FALSE)
})


test_that("cannot watch logs for a task that has not been submitted", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_explicit(quote(identity(1))))
  err <- expect_error(
    task_log_watch(id, root = path),
    "Cannot watch logs of task '.+',")
  expect_equal(err$body,
               c(i = "You need to submit this task to watch its logs"))
})


test_that("can get info from successful task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_true(task_eval(id, root = path))
  info <- task_info(id, root = path)
  expect_s3_class(info, "hipercow_task_info")
  expect_equal(info$id, id)
  expect_equal(info$status, "success")
  expect_null(info$driver)
  expect_s3_class(info$times, "POSIXct")
  expect_equal(names(info$times), c("created", "started", "finished"))
  expect_null(info$retry_chain)
  msg <- capture_messages(print(info))
  ## Specific tests for this below
  expect_match(msg, sprintf("task %s (success)", id), fixed = TRUE, all = FALSE)
  expect_match(msg, "Created at", fixed = TRUE, all = FALSE)
  expect_match(msg, "Started at", fixed = TRUE, all = FALSE)
  expect_match(msg, "Finished at", fixed = TRUE, all = FALSE)
})


test_that("can get info from created task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  info <- task_info(id, root = path)
  expect_s3_class(info, "hipercow_task_info")
  expect_equal(info$id, id)
  expect_equal(info$status, "created")
  expect_null(info$driver)
  expect_s3_class(info$times, "POSIXct")
  expect_equal(names(info$times), c("created", "started", "finished"))
  expect_equal(is.na(info$times),
               c(created = FALSE, started = TRUE, finished = TRUE))
  expect_null(info$retry_chain)
})


test_that("can get info from successful task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  expect_true(task_eval(id, root = path))
  info <- task_info(id, root = path)
  expect_s3_class(info, "hipercow_task_info")
  expect_equal(info$id, id)
  expect_equal(info$status, "success")
  expect_null(info$driver)
  expect_s3_class(info$times, "POSIXct")
  expect_equal(names(info$times), c("created", "started", "finished"))
  expect_null(info$retry_chain)
})


test_that("can get info from submitted/running tasks without driver", {
  cache$allow_load_drivers <- NULL
  withr::defer(cache$allow_load_drivers <- NULL)
  withr::local_envvar("HIPERCOW_NO_DRIVERS" = 1)

  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(sqrt(2))))
  writeLines("magic",
             path_to_task_file(path, id, "status-submitted"))

  res <- task_info(id, root = path)
  expect_equal(res$status, "submitted")
  expect_equal(res$driver, "magic")
  expect_s3_class(res$times, "POSIXct")
  expect_equal(is.na(res$times),
               c(created = FALSE, started = TRUE, finished = TRUE))

  file.create(path_to_task_file(path, id, "status-running"))
  res <- task_info(id, root = path)
  expect_equal(res$status, "running")
  expect_equal(res$driver, "magic")
  expect_s3_class(res$times, "POSIXct")
  expect_equal(is.na(res$times),
               c(created = FALSE, started = FALSE, finished = TRUE))
})


test_that("can print info objects with different times present", {
  t0 <- structure(1704735099, class = c("POSIXct", "POSIXt"))
  t1 <- t0 + 10
  t2 <- t1 + 360
  times1 <- c(created = t0, started = NA, finished = NA)
  times2 <- c(created = t0, started = t1, finished = NA)
  times3 <- c(created = t0, started = t1, finished = t2)
  times4 <- c(created = t0, started = NA, finished = t2)

  msg <- capture_messages(print_info_times(times1, "created"))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Created at 2024-01-08 17:31:39 \\(.+ago\\)")
  expect_match(msg[[2]], "Not started yet \\(waiting for .+\\)")
  expect_match(msg[[3]], "Not finished yet \\(waiting to start\\)")

  msg <- capture_messages(print_info_times(times2, "running"))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Created at 2024-01-08 17:31:39 \\(.+ago\\)")
  expect_match(msg[[2]],
               "Started at 2024-01-08 17:31:49 \\(.+ago; waited 10s\\)")
  expect_match(msg[[3]], "Not finished yet \\(running for .+\\)")

  msg <- capture_messages(print_info_times(times3, "success"))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Created at 2024-01-08 17:31:39 \\(.+ago\\)")
  expect_match(msg[[2]],
               "Started at 2024-01-08 17:31:49 \\(.+ago; waited 10s\\)")
  expect_match(msg[[3]],
               "Finished at 2024-01-08 17:37:49 \\(.+ago; ran for 6m\\)")

  msg <- capture_messages(print_info_times(times1, "failure"))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Created at 2024-01-08 17:31:39 \\(.+ago\\)")
  expect_match(msg[[2]], "Start time unknown!")
  expect_match(msg[[3]], "End time unknown!")

  msg <- capture_messages(print_info_times(times4, "failure"))
  expect_length(msg, 3)
  expect_match(msg[[1]], "Created at 2024-01-08 17:31:39 \\(.+ago\\)")
  expect_match(msg[[2]], "Start time unknown!")
  expect_match(msg[[3]],
               "Finished at 2024-01-08 17:37:49 \\(.+ago; ran for [?]{3}\\)")
})


test_that("can print information about the driver", {
  expect_no_message(print_info_driver(NULL))
  expect_message(print_info_driver("foo"),
                 "Submitted with 'foo'")
})


test_that("can print information about task data for expression-based tasks", {
  data <- list(type = "explicit",
               expr = quote(sqrt(1)),
               environment = "default",
               variables = NULL,
               envvars = NULL,
               path = ".")
  msg <- capture_messages(print_info_data(data))
  expect_length(msg, 4)
  expect_match(msg[[1]], "Task type: explicit")
  expect_match(msg[[2]], "Expression: sqrt(1)", fixed = TRUE)
  expect_match(msg[[3]], "Locals: (none)", fixed = TRUE)
  expect_match(msg[[4]], "Environment: default")

  data2 <- data
  data2$type <- "expression"
  data2$path <- "some/path"
  msg2 <- capture_messages(print_info_data(data2))
  expect_length(msg2, 5)
  expect_match(msg2[[1]], "Task type: expression")
  expect_match(msg2[[5]], "Relative path: some/path")
  expect_equal(msg2[2:4], msg[2:4])

  data3 <- data
  data3$variables <- list(locals = c(a = 1, b = 2, c = 3), globals = "d")
  msg3 <- capture_messages(print_info_data(data3))
  expect_length(msg3, 5)
  expect_match(msg3[[3]], "Locals: a, b, and c")
  expect_match(msg3[[4]], "Globals: d")
  expect_equal(msg3[c(1, 2, 5)], msg[c(1, 2, 4)])

  data4 <- data
  data4$envvars <- hipercow_envvars(X = "x", Y = "y", "Z" = "z")
  msg4 <- capture_messages(print_info_data(data4))
  expect_length(msg4, 8)
  expect_match(msg4[[5]], "Environment variables:")
  expect_match(msg4[[6]], "X = x")
  expect_match(msg4[[7]], "Y = y")
  expect_match(msg4[[8]], "Z = z")
  expect_equal(msg4[1:4], msg)
})


test_that("can print information about task data for script-based tasks", {
  data <- list(type = "script",
               script = "foo.R",
               environment = "default",
               chdir = FALSE,
               echo = TRUE)
  msg <- capture_messages(print_info_data(data))
  expect_length(msg, 4)
  expect_match(msg[[1]], "Task type: script")
  expect_match(msg[[2]], "Script: foo.R", fixed = TRUE)
  expect_match(msg[[3]], "Options: chdir = FALSE, echo = TRUE", fixed = TRUE)
  expect_match(msg[[4]], "Environment: default")
})


test_that("can print information about the chain in info", {
  t0 <- structure(1704735099, class = c("POSIXct", "POSIXt"))
  t1 <- t0 + 10
  t2 <- t1 + 360
  info <- structure(
    list(id = "aaa",
         status = "created",
         driver = NULL,
         data = list(type = "expression",
                     expr = quote(runif(1)),
                     environment = "default"),
         times = c(created = Sys.time(), started = NA, finished = NA),
         retry_chain = c("aaa", "bbb", "ccc")),
    class = "hipercow_task_info")
  msg <- capture_messages(print(info))
  cmp <- capture_messages(print_info_retry_chain(info$id, info$retry_chain))
  expect_match(msg, cmp, fixed = TRUE, all = FALSE)

  expect_message(
    print_info_retry_chain("aaa", info$retry_chain),
    "1st of a chain of a task retried 2 times, most recently 'ccc'")
  expect_message(
    print_info_retry_chain("aaa", info$retry_chain[-3]),
    "1st of a chain of a task retried 1 time, most recently 'bbb'")
  expect_message(
    print_info_retry_chain("ccc", info$retry_chain),
    "Last of a chain of a task retried 2 times")
  expect_message(
    print_info_retry_chain("bbb", info$retry_chain[-3]),
    "Last of a chain of a task retried 1 time")
})


test_that("can run a task with envvars", {
  path <- withr::local_tempdir()
  init_quietly(path)
  envvar <- hipercow_envvars(TEST_ENV = "hello!")
  id <- withr::with_dir(
    path, task_create_expr(Sys.getenv("TEST_ENV"), envvars = envvar))

  dat <- readRDS(path_to_task_file(path, id, "data"))
  expect_equal(dat$envvars, c(DEFAULT_ENVVARS, envvar))

  renviron <- readLines(path_to_task_file(path, id, "Renviron"))
  expect_equal(
    renviron,
    c(sprintf("%s=%s", DEFAULT_ENVVARS$name, DEFAULT_ENVVARS$value),
      "TEST_ENV=hello!"))

  expect_true(task_eval(id, root = path))
  expect_equal(task_result(id, root = path), "hello!")
  expect_equal(Sys.getenv("TEST_ENV"), "")
})


test_that("validate envvars", {
  path <- withr::local_tempdir()
  init_quietly(path)
  envvar <- hipercow_envvars(TEST_ENV = "hello!")
  expect_error(
    withr::with_dir(
      path, task_create_expr(Sys.getenv("TEST_ENV"), envvars = TRUE)),
    "Expected a 'hipercow_envvars' object for 'envvars'")
})


test_that("can validate nicely that we were given incorrect inputs", {
  b <- new_bundle("bundle", ids::random_id(3))

  err <- expect_error(
    check_task_id(b, "task_thing", FALSE, NULL),
    "Unexpected bundle where a vector of task identifiers expected")
  expect_equal(
    err$body,
    c(i = "Did you mean to pass element '$ids' of this bundle?",
      i = paste("Did you mean to use 'hipercow_bundle_thing()'",
                "instead of 'task_thing()'?")))

  err <- expect_error(
    check_task_id(b, "task_thing", TRUE, NULL),
    "Unexpected bundle where a single task identifier expected")
  expect_equal(
    err$body,
    c(x = paste("You have passed bundle 'bundle' to 'task_thing'",
                "that expects a single task; this can never work"),
      i = paste("Did you mean to use 'hipercow_bundle_thing()' instead",
                "of 'task_thing()'?")))

  err <- expect_error(
    check_task_id(b, "thing", FALSE, NULL),
    "Unexpected bundle where a vector of task identifiers expected")
  expect_equal(
    err$body,
    c(i = "Did you mean to pass element '$ids' of this bundle?"))

  err <- expect_error(
    check_task_id("foo", "thing", FALSE, NULL),
    "Invalid task identifier")
  expect_equal(
    err$body,
    c(x = "Was given: 'foo'",
      i = "Task identifiers are 32-character hexidecimal strings"))

  err <- expect_error(
    check_task_id(c("foo", b$ids, "bar"), "thing", FALSE, NULL),
    "Invalid task identifiers")
  expect_equal(
    err$body,
    c(x = "Was given: 'foo' and 'bar'",
      i = "Task identifiers are 32-character hexidecimal strings"))
})


test_that("can create task with parallel setup", {
  path <- withr::local_tempdir()
  init_quietly(path)
  parallel <- hipercow_parallel(method = "future")
  resources <- hipercow_resources(cores = 2)
  id <- withr::with_dir(path, suppressMessages(
    task_create_expr(sessionInfo(), resources = resources,
                     parallel = parallel)))
  dat <- readRDS(path_to_task_file(path, id, "data"))
  expect_equal(dat$parallel$method, "future")

  mock_parallel_setup <- mockery::mock()
  mockery::stub(task_eval, "hipercow_parallel_setup", mock_parallel_setup)
  res <- task_eval(id, root = path)
  mockery::expect_called(mock_parallel_setup, 1)
})


test_that("can create task with NULL parallel method", {
  path <- withr::local_tempdir()
  init_quietly(path)
  parallel <- hipercow_parallel(method = NULL)
  id <- withr::with_dir(
    path, task_create_expr(sessionInfo(), parallel = parallel))

  dat <- readRDS(path_to_task_file(path, id, "data"))
  expect_true(!is.null(dat$parallel))
  expect_true(is.null(dat$parallel$method))

  mock_parallel_setup <- mockery::mock()
  mockery::stub(task_eval, "hipercow_parallel_setup", mock_parallel_setup)
  res <- task_eval(id, root = path)
  mockery::expect_called(mock_parallel_setup, 1)
})
