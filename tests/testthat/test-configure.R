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


test_that("can load a driver", {
  clear_drivers()
  mock_create <- mockery::mock(elsewhere_driver())
  mockery::stub(hermod_driver_load, "hermod_driver_create", mock_create)

  result <- hermod_driver_load("windows", NULL)
  expect_identical(result, elsewhere_driver())

  mockery::expect_called(mock_create, 1)
  expect_equal(mockery::mock_args(mock_create)[[1]], list("windows", NULL))
  expect_identical(cache$drivers$windows, result)

  expect_identical(hermod_driver_load("windows", NULL), result)
  mockery::expect_called(mock_create, 1) # not called again
})


test_that("good error if invalid driver loaded", {
  clear_drivers()
  mock_create <- mockery::mock(elsewhere_driver())
  mockery::stub(hermod_driver_load, "hermod_driver_create", mock_create)
  err <- expect_error(
    hermod_driver_load("other", NULL),
    "Invalid driver 'other'")
  expect_equal(err$body, c(i = "Valid choice is: 'windows'"))
  mockery::expect_called(mock_create, 0)
})


test_that("creating a package loads function and calls target function", {
  mock_ns <- list(hermod_driver_foo = mockery::mock(elsewhere_driver()))
  mock_ensure_package <- mockery::mock(mock_ns)
  mockery::stub(hermod_driver_create, "ensure_package", mock_ensure_package)
  result <- hermod_driver_create("foo")
  expect_equal(result, elsewhere_driver())

  mockery::expect_called(mock_ensure_package, 1)
  expect_equal(mockery::mock_args(mock_ensure_package)[[1]],
               list("hermod.foo", NULL))
  mockery::expect_called(mock_ns$hermod_driver_foo, 1)
  expect_equal(mockery::mock_args(mock_ns$hermod_driver_foo)[[1]],
               list())
})


test_that("roots don't start with a configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(getwd())))
  err <- expect_error(
    withr::with_dir(path, task_submit(id)),
    "No hermod driver configured")
  expect_equal(err$body, c(i = "Please run 'hermod_configure()'"))
})
