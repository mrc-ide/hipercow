test_that("can submit a task", {
  mock_client <- list(submit = mockery::mock("1234"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_submit, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows
  id <- hermod::hermod_task_create_explicit(quote(sessionInfo()),
                                            root = path_root)

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
})