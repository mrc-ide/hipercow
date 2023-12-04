test_that("can submit a task", {
  mock_client <- list(submit = mockery::mock("1234"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_submit, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows

  ## I don't see why this does not fail
  id <- withr::with_dir(
    path_root,
    hermod::hermod_task_create_explicit(quote(sessionInfo())))

  windows_submit(id, config, path_root)

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  batch_path <- windows_path(file.path(
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
  expect_equal(task_dide_id(id, path_root), "1234")
})


test_that("can get a task status", {
  mock_client <- list(status = mockery::mock("running"))
  mock_get_client <- mockery::mock(mock_client)
  mock_task_dide_id <- mockery::mock("1234")
  mockery::stub(windows_status, "get_web_client", mock_get_client)
  mockery::stub(windows_status, "task_dide_id", mock_task_dide_id)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::hermod_task_create_explicit(quote(sessionInfo())))

  path_root <- root$path$root
  config <- root$config$windows
  expect_equal(windows_status(id, config, path_root), "running")

  mockery::expect_called(mock_client$status, 1)
  expect_equal(
    mockery::mock_args(mock_client$status)[[1]],
    list("1234"))
  mockery::expect_called(mock_task_dide_id, 1)
  expect_equal(
    mockery::mock_args(mock_task_dide_id)[[1]],
    list(id, path_root))
})


test_that("can get dide id for submitted task", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  path_root <- root$path$root
  config <- root$config$windows
  id <- withr::with_dir(
    path_root,
    hermod::hermod_task_create_explicit(quote(sessionInfo())))
  writeLines("1234", file.path(path_root, "hermod", "tasks", id, "dide_id"))
  expect_equal(task_dide_id(id, path_root), "1234")
  expect_equal(cache$task_dide_id[[id]], "1234")
  expect_equal(task_dide_id(id, path_root), "1234")
})
