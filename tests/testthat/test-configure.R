test_that("Can create configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)
  path <- normalize_path(path)

  cfg <- list(a = 1, b = 2)
  mock_driver <- list(configure = mockery::mock(cfg))
  mock_driver_load <- mockery::mock(mock_driver)
  mockery::stub(hermod_configure, "hermod_driver_load", mock_driver_load)

  hermod_configure("foo", a = 1, b = 2, root = path)

  mockery::expect_called(mock_driver_load, 1)
  expect_equal(mockery::mock_args(mock_driver_load)[[1]], list("foo"))

  mockery::expect_called(mock_driver$configure, 1)
  expect_equal(
    mockery::mock_args(mock_driver$configure)[[1]],
    list(a = 1, b = 2))

  expect_equal(hermod_root(path)$config$foo, list(a = 1, b = 2))

  ## Force re-reading from disk
  rm(list = "roots", envir = cache)
  expect_equal(hermod_root(path)$config$foo, list(a = 1, b = 2))
})


test_that("can select an appropriate driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)

  root_here <- hermod_root(path_here)
  err <- expect_error(
    hermod_driver_select(NULL, root_here),
    "No hermod driver configured")
  expect_equal(err$body, c(i = "Please run 'hermod_configure()'"))

  err <- expect_error(
    hermod_driver_select("elsewhere", root_here),
    "Invalid value for 'driver': 'elsewhere'")
  expect_equal(
    err$body,
    c(i = "No driver configured; please run 'hermod_configure(\"elsewhere\")'"))

  hermod_configure("elsewhere", path = path_there, root = path_here)

  expect_equal(hermod_driver_select("elsewhere", root_here), "elsewhere")
  expect_equal(hermod_driver_select(NULL, root_here), "elsewhere")

  err <- expect_error(
    hermod_driver_select("other", root_here),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid option is: 'elsewhere'"))

  root_here$config$windows <- list()

  err <- expect_error(
    hermod_driver_select(NULL, root_here),
    "More than one hermod driver configured")
  expect_equal(err$body,
               c(i = "Please provide the argument 'driver'",
                 i = "Valid options are: 'elsewhere' and 'windows'"))
  err <- expect_error(
    hermod_driver_select("other", root_here),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid options are: 'elsewhere' and 'windows'"))


})


test_that("roots don't start with a configuration", {
  skip("rework")
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  err <- expect_error(
    hermod_driver_select("foo", path),
    "No hermod driver configured")
  expect_equal(err$body, c(i = "Please run 'hermod_configure(\"foo\", ...)'"))
})
