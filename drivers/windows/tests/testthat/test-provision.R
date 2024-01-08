test_that("can run provision script", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_job = mockery::mock("PENDING", "RUNNING", "RUNNING", "COMPLETE"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision_run, "get_web_client", mock_get_client)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config$windows
  args <- list(method = "script", environment = NULL, poll = 0)

  msg <- capture_messages(
    windows_provision_run(args, config, path_root))

  mockery::expect_called(mock_get_client, 1)
  expect_equal(mockery::mock_args(mock_get_client)[[1]], list())

  mockery::expect_called(mock_client$submit, 1)
  args <- mockery::mock_args(mock_client$submit)[[1]]
  expect_match(args[[2]], "^conan:[[:xdigit:]]{32}$")
  id <- args[[2]]
  batch_path <- windows_path_slashes(file.path(
    "//host.dide.ic.ac.uk/share/path/b/c/hipercow/provision",
    sub("^conan:", "", id),
    "provision.bat"))
  expect_equal(args, list(batch_path, id, "BuildQueue"))

  mockery::expect_called(mock_client$status_job, 4)
  expect_equal(mockery::mock_args(mock_client$status_job),
               rep(list(list("1234")), 4))

  expect_match(msg, "Installation script finished successfully", all = FALSE)
})


test_that("error on provision script failure", {
  mock_client <- list(
    submit = mockery::mock("1234"),
    status_job = mockery::mock("PENDING", "RUNNING", "RUNNING", "ERROR"))
  mock_get_client <- mockery::mock(mock_client)
  mockery::stub(windows_provision_run, "get_web_client", mock_get_client)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))
  path_root <- root$path$root
  config <- root$config$windows
  args <- list(method = "script", environment = NULL, poll = 0)
  expect_error(
    suppressMessages(
      windows_provision_run(args, config, path_root)),
    "Installation failed after")
})


test_that("can call provision_list using conan_list", {
  mock_conan_list <- mockery::mock()
  mockery::stub(windows_provision_list, "conan2::conan_list", mock_conan_list)

  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  file.create(file.path(root$path$root, "provision.R"))

  path_root <- root$path$root
  config <- root$config$windows
  path_lib <- file.path(path_root, config$path_lib)
  path_script <- file.path(path_root, "provision.R")

  res <- windows_provision_list(NULL, config, path_root)
  mockery::expect_called(mock_conan_list, 1)
  expect_equal(mockery::mock_args(mock_conan_list)[[1]],
               list(path_lib, NULL))

  res <- windows_provision_list(list(method = "script"), config, path_root)
  mockery::expect_called(mock_conan_list, 2)
  expect_equal(mockery::mock_args(mock_conan_list)[[2]],
               list(path_lib, rlang::hash_file(path_script)))
})


test_that("camn can provision_compare using conan_compare", {
  mock_conan_compare <- mockery::mock()
  mockery::stub(windows_provision_compare, "conan2::conan_compare",
                mock_conan_compare)
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")

  path_root <- root$path$root
  config <- root$config$windows
  path_lib <- file.path(path_root, config$path_lib)

  windows_provision_compare(config, path_root, 0, -1)
  mockery::expect_called(mock_conan_compare, 1)
  expect_equal(mockery::mock_args(mock_conan_compare)[[1]],
               list(path_lib, 0, -1))
})
