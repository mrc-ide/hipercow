test_that("can submit a task", {
  mock_client <- list(submit = mockery::mock("1234"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_submit, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows

  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  windows_submit(id, resources = NULL, config, path_root)

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  batch_path <- windows_path_slashes(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hipercow/tasks",
    id,
    "run.bat"))

  mockery::expect_called(mock_client$submit, 1)
  expect_equal(
    mockery::mock_args(mock_client$submit)[[1]],
    list(batch_path, id, NULL))
  expect_true(
    file.exists(file.path(path_root, "hipercow", "tasks", id, "run.bat")))
  expect_true(
    file.exists(file.path(path_root, "hipercow", "tasks", id, "dide_id")))
  expect_equal(
    readLines(file.path(path_root, "hipercow", "tasks", id, "dide_id")),
    "1234")

  path_batch <- file.path(path_root, "hipercow", "tasks", id, "run.bat")
  code <- readLines(path_batch)
  expect_match(grep("R_LIBS_USER", code, value = TRUE),
               "I:/bootstrap/")
})


test_that("can get a task status", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  path_root <- root$path$root
  config <- root$config$windows
  expect_equal(windows_status(id, config, path_root), "submitted")

  file.create(file.path(path_root, "hipercow", "tasks", id, "status-running"))
  expect_equal(windows_status(id, config, path_root), "running")

  file.create(file.path(path_root, "hipercow", "tasks", id, "status-success"))
  expect_equal(windows_status(id, config, path_root), "success")
})


test_that("can get a task result", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sqrt(2)), submit = FALSE))
  hipercow::task_eval(id, root = path_root)
  expect_silent(windows_result(id, config, path_root))
  expect_equal(
    hipercow::task_result(id, root = path_root),
    sqrt(2))
})


test_that("can cancel a task", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sqrt(2)), submit = FALSE))
  writeLines("1234", file.path(root$path$tasks, id, "dide_id"))

  mock_client <- list(
    cancel = mockery::mock(c("1234" = "OK"), c("1234" = "WRONG_STATE")))
  mock_get_client <- mockery::mock(mock_client, cycle = TRUE)
  mockery::stub(windows_cancel, "get_web_client", mock_get_client)

  res1 <- windows_cancel(id, config, path_root)
  expect_true(res1$cancelled)
  expect_s3_class(res1$time_started, "POSIXct")
  expect_true(is.na(res1$time_started))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())
  mockery::expect_called(mock_client$cancel, 1)
  expect_equal(mockery::mock_args(mock_client$cancel)[[1]], list("1234"))

  res2 <- windows_cancel(id, config, path_root)
  expect_false(res2$cancelled)
  expect_equal(res2$time_started, res1$time_started)

  mockery::expect_called(mock_get_client, 2)
  expect_equal(mockery::mock_args(mock_get_client)[[2]], list())
  mockery::expect_called(mock_client$cancel, 2)
  expect_equal(mockery::mock_args(mock_client$cancel)[[2]], list("1234"))
})


test_that("can report on time started if known", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sqrt(2)), submit = FALSE))
  writeLines("1234", file.path(root$path$tasks, id, "dide_id"))

  mock_client <- list(
    cancel = mockery::mock(c("1234" = "OK"), c("1234" = "WRONG_STATE")))
  mock_get_client <- mockery::mock(mock_client, cycle = TRUE)
  mockery::stub(windows_cancel, "get_web_client", mock_get_client)

  file.create(file.path(path_root, "hipercow", "tasks", id, "status-running"))

  res1 <- windows_cancel(id, config, path_root)
  expect_true(res1$cancelled)
  expect_s3_class(res1$time_started, "POSIXct")
  expect_false(is.na(res1$time_started))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())
  mockery::expect_called(mock_client$cancel, 1)
  expect_equal(mockery::mock_args(mock_client$cancel)[[1]], list("1234"))

  res2 <- windows_cancel(id, config, path_root)
  expect_false(res2$cancelled)
  expect_s3_class(res2$time_started, "POSIXct")
  expect_true(is.na(res2$time_started))

  mockery::expect_called(mock_get_client, 2)
  expect_equal(mockery::mock_args(mock_get_client)[[2]], list())
  mockery::expect_called(mock_client$cancel, 2)
  expect_equal(mockery::mock_args(mock_client$cancel)[[2]], list("1234"))
})


test_that("can cancel a bunch of tasks, in reverse order", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  withr::with_dir(path_root, {
    id1 <- hipercow::task_create_explicit(quote(sqrt(1)), submit = FALSE)
    id2 <- hipercow::task_create_explicit(quote(sqrt(2)), submit = FALSE)
    id3 <- hipercow::task_create_explicit(quote(sqrt(3)), submit = FALSE)
  })
  ids <- c(id1, id2, id3)
  writeLines("1234", file.path(root$path$tasks, id1, "dide_id"))
  writeLines("1235", file.path(root$path$tasks, id2, "dide_id"))
  writeLines("1236", file.path(root$path$tasks, id3, "dide_id"))

  mock_client <- list(
    cancel = mockery::mock(c("1236" = "OK", "1235" = "OK",
                             "1234" = "WRONG_STATE")))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_cancel, "get_web_client", mock_get_client)

  expect_equal(windows_cancel(ids, config, path_root)$cancelled,
               c(TRUE, TRUE, FALSE))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())
  mockery::expect_called(mock_client$cancel, 1)
  expect_equal(mockery::mock_args(mock_client$cancel)[[1]],
               list(c("1236", "1235", "1234")))
})


test_that("can read a task log", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  path_log <- file.path(path_root, "hipercow", "tasks", id, "log")
  expect_null(windows_log(id, FALSE, config, path_root))
  file.create(path_log)
  expect_equal(windows_log(id, FALSE, config, path_root), character())
  writeLines(c("a", "b", "c"), path_log)
  expect_equal(windows_log(id, FALSE, config, path_root), c("a", "b", "c"))
})


test_that("can read dide log", {
  mock_client <- list(log = mockery::mock(c("some", "logs")))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_log, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))
  writeLines("1234", file.path(root$path$tasks, id, "dide_id"))

  expect_equal(windows_log(id, TRUE, config, path_root),
               c("some", "logs"))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  mockery::expect_called(mock_client$log, 1)
  expect_equal(mockery::mock_args(mock_client$log)[[1]], list("1234"))
})


test_that("can submit a task using the development bootstrap", {
  withr::local_options(hipercow.development = TRUE)
  mock_client <- list(submit = mockery::mock("1234"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_submit, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows

  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  windows_submit(id, config, resources = NULL, path_root)
  path_batch <- file.path(path_root, "hipercow", "tasks", id, "run.bat")
  code <- readLines(path_batch)
  expect_match(grep("R_LIBS_USER", code, value = TRUE),
               "I:/bootstrap-dev/")
})


test_that("can get task info", {
  mock_client <- list(status_job= mockery::mock("Failed"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_info, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hipercow::task_create_explicit(quote(sessionInfo()), submit = FALSE))
  writeLines("1234", file.path(root$path$tasks, id, "dide_id"))
  file.create(file.path(path_root, "hipercow", "tasks", id, "status-running"))

  res <- windows_info(id, config, path_root)
  expect_equal(names(res), c("status", "time_started"))
  expect_equal(res$status, "Failed")
  expect_s3_class(res$time_started, "POSIXct")

  mockery::expect_called(mock_get_client, 1)
  mockery::expect_called(mock_client$status_job, 1)
  expect_equal(mockery::mock_args(mock_client$status_job)[[1]], list("1234"))
})
