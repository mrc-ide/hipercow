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
    hermod::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  windows_submit(id, config, path_root)

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  batch_path <- windows_path_slashes(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hermod/tasks",
    id,
    "run.bat"))

  mockery::expect_called(mock_client$submit, 1)
  expect_equal(
    mockery::mock_args(mock_client$submit)[[1]],
    list(batch_path, id, "AllNodes"))
  expect_true(
    file.exists(file.path(path_root, "hermod", "tasks", id, "run.bat")))
  expect_true(
    file.exists(file.path(path_root, "hermod", "tasks", id, "dide_id")))
  expect_equal(
    readLines(file.path(path_root, "hermod", "tasks", id, "dide_id")),
    "1234")
})


test_that("can get a task status", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::task_create_explicit(quote(sessionInfo()), submit = FALSE))

  path_root <- root$path$root
  config <- root$config$windows
  expect_equal(windows_status(id, config, path_root), "submitted")

  file.create(file.path(path_root, "hermod", "tasks", id, "status-running"))
  expect_equal(windows_status(id, config, path_root), "running")

  file.create(file.path(path_root, "hermod", "tasks", id, "status-success"))
  expect_equal(windows_status(id, config, path_root), "success")
})


test_that("can get a task result", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::task_create_explicit(quote(sqrt(2)), submit = FALSE))
  hermod::task_eval(id, root = path_root)
  expect_silent(windows_result(id, config, path_root))
  expect_equal(
    hermod::task_result(id, root = path_root),
    sqrt(2))
})


test_that("can cancel a task", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::task_create_explicit(quote(sqrt(2)), submit = FALSE))
  writeLines("1234", file.path(root$path$tasks, id, "dide_id"))

  mock_client <- list(
    cancel = mockery::mock(c("1234" = "OK"), c("1234" = "WRONG_STATE")))
  mock_get_client <- mockery::mock(mock_client, cycle = TRUE)
  mockery::stub(windows_cancel, "get_web_client", mock_get_client)

  expect_true(windows_cancel(id, config, path_root))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())
  mockery::expect_called(mock_client$cancel, 1)
  expect_equal(mockery::mock_args(mock_client$cancel)[[1]], list("1234"))

  expect_false(windows_cancel(id, config, path_root))

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
    id1 <- hermod::task_create_explicit(quote(sqrt(1)), submit = FALSE)
    id2 <- hermod::task_create_explicit(quote(sqrt(2)), submit = FALSE)
    id3 <- hermod::task_create_explicit(quote(sqrt(3)), submit = FALSE)
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

  expect_equal(windows_cancel(ids, config, path_root),
               c(TRUE, TRUE, FALSE))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())
  mockery::expect_called(mock_client$cancel, 1)
  expect_equal(mockery::mock_args(mock_client$cancel)[[1]],
               list(c("1236", "1235", "1234")))
})
