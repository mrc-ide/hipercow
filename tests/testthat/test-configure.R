test_that("Can create configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)
  path <- normalize_path(path)

  cfg <- list(a = 1, b = 2)
  mock_driver <- list(configure = mockery::mock(cfg))
  mock_driver_load <- mockery::mock(mock_driver)
  mockery::stub(hipercow_configure, "hipercow_driver_load", mock_driver_load)

  expect_message(
    hipercow_configure("foo", a = 1, b = 2, root = path),
    "Configured hipercow to use 'foo'")

  mockery::expect_called(mock_driver_load, 1)
  expect_equal(mockery::mock_args(mock_driver_load)[[1]], list("foo"))

  mockery::expect_called(mock_driver$configure, 1)
  expect_equal(
    mockery::mock_args(mock_driver$configure)[[1]],
    list(a = 1, b = 2))

  expect_equal(hipercow_root(path)$config$foo, list(a = 1, b = 2))

  ## Force re-reading from disk
  rm(list = "roots", envir = cache)
  expect_equal(hipercow_root(path)$config$foo, list(a = 1, b = 2))
})


## TODO: this block of tests should be broken up.
test_that("can select an appropriate driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)

  root_here <- hipercow_root(path_here)
  expect_null(hipercow_driver_select(NULL, FALSE, root_here))
  expect_null(hipercow_driver_select(FALSE, FALSE, root_here))
  err <- expect_error(
    hipercow_driver_select(TRUE, FALSE, root_here),
    "No hipercow driver configured")
  expect_equal(
    err$body,
    c(i = "Please run 'hipercow_configure()' to configure a driver"))
  expect_error(
    hipercow_driver_select(FALSE, TRUE, root_here),
    "Invalid choice 'driver = FALSE'; a driver is required here")
  err <- expect_error(
    hipercow_driver_select("elsewhere", FALSE, root_here),
    "Invalid value for 'driver': 'elsewhere'")
  expect_equal(
    err$body,
    c(i = paste("The 'elsewhere' driver is not configured; please run",
                "'hipercow_configure(\"elsewhere\", ...)'")))

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  expect_equal(
    hipercow_driver_select("elsewhere", FALSE, root_here),
    "elsewhere")
  expect_equal(hipercow_driver_select(NULL, FALSE, root_here), "elsewhere")
  expect_equal(hipercow_driver_select(TRUE, FALSE, root_here), "elsewhere")
  expect_null(hipercow_driver_select(FALSE, FALSE, root_here))

  expect_equal(
    hipercow_driver_select("elsewhere", TRUE, root_here),
    "elsewhere")
  expect_equal(hipercow_driver_select(NULL, TRUE, root_here), "elsewhere")
  expect_equal(hipercow_driver_select(TRUE, TRUE, root_here), "elsewhere")
  expect_error(hipercow_driver_select(FALSE, TRUE, root_here),
               "Invalid choice 'driver = FALSE'")

  err <- expect_error(
    hipercow_driver_select("other", FALSE, root_here),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid option is: 'elsewhere'"))

  root_here$config$windows <- list()

  body <- c(i = "Please provide the argument 'driver'",
            i = "Valid options are: 'elsewhere' and 'windows'",
            i = paste("If you have configured a driver you no longer want,",
                      "you can remove it using 'hipercow_unconfigure()',",
                      "after which the default behaviour will improve"))
  err <- expect_error(
    hipercow_driver_select(NULL, FALSE, root_here),
    "'driver' not specified but multiple drivers are configured")
  expect_equal(err$body, body)
  err <- expect_error(
    hipercow_driver_select(TRUE, FALSE, root_here),
    "'driver' not specified but multiple drivers are configured")
  expect_equal(err$body, body)
  err <- expect_error(
    hipercow_driver_select("other", FALSE, root_here),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid options are: 'elsewhere' and 'windows'"))

  expect_equal(hipercow_driver_select("windows", FALSE, root_here),
               "windows")
  expect_equal(hipercow_driver_select("elsewhere", FALSE, root_here),
               "elsewhere")
})


test_that("can load a driver", {
  clear_drivers()
  mock_create <- mockery::mock(elsewhere_driver())
  mockery::stub(hipercow_driver_load, "hipercow_driver_create", mock_create)

  result <- hipercow_driver_load("windows", NULL)
  expect_identical(result, elsewhere_driver())

  mockery::expect_called(mock_create, 1)
  expect_equal(mockery::mock_args(mock_create)[[1]], list("windows", NULL))
  expect_identical(cache$drivers$windows, result)

  expect_identical(hipercow_driver_load("windows", NULL), result)
  mockery::expect_called(mock_create, 1) # not called again
})


test_that("good error if invalid driver loaded", {
  err <- expect_error(
    hipercow_driver_create("other", NULL),
    "Invalid driver 'other'")
  expect_equal(err$body, c(i = "Valid choice is: 'windows'"))
})


test_that("creating a package loads function and calls target function", {
  mock_ns <- list(hipercow_driver_windows = mockery::mock(elsewhere_driver()))
  mock_ensure_package <- mockery::mock(mock_ns)
  mockery::stub(hipercow_driver_create, "ensure_package", mock_ensure_package)
  result <- hipercow_driver_create("windows")
  expect_equal(result, elsewhere_driver())

  mockery::expect_called(mock_ensure_package, 1)
  expect_equal(mockery::mock_args(mock_ensure_package)[[1]],
               list("hipercow.windows", NULL))
  mockery::expect_called(mock_ns$hipercow_driver_windows, 1)
  expect_equal(mockery::mock_args(mock_ns$hipercow_driver_windows)[[1]],
               list())
})


test_that("roots don't start with a configuration", {
  mount <- withr::local_tempfile()
  path <- file.path(mount, "b", "c")
  root <- init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(getwd())))
  err <- expect_error(
    withr::with_dir(path, task_submit(id)),
    "No hipercow driver configured")
  expect_equal(
    err$body,
    c(i = "Please run 'hipercow_configure()' to configure a driver"))
})


test_that("informative messages on configuration", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  path_elsewhere <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  init_quietly(path_elsewhere)

  root_here <- hipercow_root(path_here)
  expect_message(
    hipercow_configure("elsewhere", path = path_there, root = path_here),
    "Configured hipercow to use 'elsewhere'")
  expect_message(
    hipercow_configure("elsewhere", path = path_there, root = path_here),
    "Configuration for 'elsewhere' unchanged")
  expect_message(
    hipercow_configure("elsewhere", path = path_elsewhere, root = path_here),
    "Updated configuration for 'elsewhere'")
})


test_that("prevent loading of drivers", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  root_here <- hipercow_root(path_here)

  cache$allow_load_drivers <- NULL
  withr::defer(cache$allow_load_drivers <- NULL)
  withr::local_envvar("HIPERCOW_NO_DRIVERS" = 1)

  expect_error(
    hipercow_driver_prepare("elsewhere", root_here),
    "Trying to load a driver from code that should not do so")
  expect_error(
    hipercow_driver_load("elsewhere"),
    "Trying to load a driver from code that should not do so")
})


test_that("can remove a driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  expect_type(hipercow_root(path_here)$config$elsewhere, "list")
  expect_true(file.exists(
    file.path(path_here, "hipercow", "config", "elsewhere.rds")))

  expect_message(
    hipercow_unconfigure("elsewhere", root = path_here),
    "Removed configuration for 'elsewhere'")

  expect_null(hipercow_root(path_here)$config$elsewhere)
  expect_false(file.exists(
    file.path(path_here, "hipercow", "config", "elsewhere.rds")))

  expect_message(
    hipercow_unconfigure("elsewhere", root = path_here),
    "Did not remove configuration for 'elsewhere' as it was not enabled")
})
