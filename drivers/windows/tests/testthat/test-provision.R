test_that("can run provision script", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_job = mockery::mock("PENDING", "RUNNING", "RUNNING", "COMPLETE"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config$windows

  msg <- capture_messages(
    windows_provision("script", config, path_root, NULL, poll = 0))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  mockery::expect_called(mock_client$submit, 1)
  args <- mockery::mock_args(mock_client$submit)[[1]]
  expect_match(args[[2]], "^conan:[[:xdigit:]]{32}$")
  id <- args[[2]]
  batch_path <- windows_path(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hermod/provision",
    sub("^conan:", "", id),
    "provision.bat"))
  expect_equal(args, list(batch_path, id, "BuildQueue"))

  mockery::expect_called(mock_client$status_job, 4)
  expect_equal(mockery::mock_args(mock_client$status_job),
               rep(list(list("1234")), 4))
})


test_that("error on provision script failure", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_job = mockery::mock("PENDING", "RUNNING", "RUNNING", "ERROR"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision, "get_web_client", mock_get_client)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))
  path_root <- root$path$root
  config <- root$config$windows
  expect_error(
    suppressMessages(
      windows_provision("script", config, path_root, NULL, poll = 0)),
    "Installation failed")
})
