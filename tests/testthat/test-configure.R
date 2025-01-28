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

  clear_cached_roots()

  expect_equal(hipercow_root(path)$config$foo, list(a = 1, b = 2))
})


test_that("can select an appropriate driver when none set", {
  elsewhere_register()
  path <- withr::local_tempfile()
  init_quietly(path)

  root <- hipercow_root(path)
  expect_null(hipercow_driver_select(NULL, FALSE, root))
  expect_null(hipercow_driver_select(FALSE, FALSE, root))
  err <- expect_error(
    hipercow_driver_select(TRUE, FALSE, root),
    "No hipercow driver configured")
  expect_equal(
    err$body,
    c(i = "Please run 'hipercow_configure()' to configure a driver"))
  expect_error(
    hipercow_driver_select(FALSE, TRUE, root),
    "Invalid choice 'driver = FALSE'; a driver is required here")
  err <- expect_error(
    hipercow_driver_select("elsewhere", FALSE, root),
    "Invalid value for 'driver': 'elsewhere'")
  expect_equal(
    err$body,
    c(i = paste("The 'elsewhere' driver is not configured; please run",
                "'hipercow_configure(\"elsewhere\", ...)'")))
})


test_that("can select an appropriate driver when one set", {
  path <- withr::local_tempfile()
  init_quietly(path, driver = "example")
  root <- hipercow_root(path)

  expect_equal(
    hipercow_driver_select("example", FALSE, root),
    "example")
  expect_equal(hipercow_driver_select(NULL, FALSE, root), "example")
  expect_equal(hipercow_driver_select(TRUE, FALSE, root), "example")
  expect_null(hipercow_driver_select(FALSE, FALSE, root))

  expect_equal(
    hipercow_driver_select("example", TRUE, root),
    "example")
  expect_equal(hipercow_driver_select(NULL, TRUE, root), "example")
  expect_equal(hipercow_driver_select(TRUE, TRUE, root), "example")
  expect_error(hipercow_driver_select(FALSE, TRUE, root),
               "Invalid choice 'driver = FALSE'")

  err <- expect_error(
    hipercow_driver_select("other", FALSE, root),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid option is: 'example'"))
})


test_that("can select an appropriate driver when several set", {
  path <- withr::local_tempfile()
  init_quietly(path, driver = "example")
  root <- hipercow_root(path)
  root$config$windows <- list()

  body <- c(i = "Please provide the argument 'driver'",
            i = "Valid options are: 'example' and 'windows'",
            i = paste("If you have configured a driver you no longer want,",
                      "you can remove it using 'hipercow_unconfigure()',",
                      "after which the default behaviour will improve"))
  err <- expect_error(
    hipercow_driver_select(NULL, FALSE, root),
    "'driver' not specified but multiple drivers are configured")
  expect_equal(err$body, body)
  err <- expect_error(
    hipercow_driver_select(TRUE, FALSE, root),
    "'driver' not specified but multiple drivers are configured")
  expect_equal(err$body, body)
  err <- expect_error(
    hipercow_driver_select("other", FALSE, root),
    "Invalid value for 'driver': 'other'")
  expect_equal(err$body, c(i = "Valid options are: 'example' and 'windows'"))

  expect_equal(hipercow_driver_select("dide-windows", FALSE, root),
               "dide-windows")
  expect_equal(hipercow_driver_select("example", FALSE, root),
               "example")
})


test_that("can load a driver", {
  clear_cached_drivers()
  mock_create <- mockery::mock(elsewhere_driver())
  mockery::stub(hipercow_driver_load, "hipercow_driver_create", mock_create)

  result <- hipercow_driver_load("dide-windows", NULL)
  expect_identical(result, elsewhere_driver())

  mockery::expect_called(mock_create, 1)
  expect_equal(mockery::mock_args(mock_create)[[1]], list("dide-windows", NULL))
  expect_identical(cache$drivers$windows, result)

  expect_identical(hipercow_driver_load("dide-windows", NULL), result)
  mockery::expect_called(mock_create, 1) # not called again
})


test_that("nice error if user hits hipercow driver loading bug", {
  path <- withr::local_tempfile()
  init_quietly(path)
  root <- hipercow_root(path)
  expect_error(
    hipercow_driver_prepare(NULL, root, NULL),
    "Trying to load a driver after deciding not to (a hipercow bug)",
    fixed = TRUE)
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
  result <- hipercow_driver_create("dide-windows")
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

  config <- file.path(path_here, "hipercow", "config", hostname(),
                      "elsewhere.rds")

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  expect_type(hipercow_root(path_here)$config$elsewhere, "list")
  expect_true(file.exists(config))

  expect_message(
    hipercow_unconfigure("elsewhere", root = path_here),
    "Removed configuration for 'elsewhere'")

  expect_null(hipercow_root(path_here)$config$elsewhere)
  expect_false(file.exists(config))

  expect_message(
    hipercow_unconfigure("elsewhere", root = path_here),
    "Did not remove configuration for 'elsewhere' as it was not enabled")
})


test_that("supports legacy configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)

  legacy_path <- file.path(path, "hipercow", "config", "example.rds")
  fs::dir_create(fs::path_dir(legacy_path))
  saveRDS(list(a = 1), legacy_path)

  clear_cached_roots()

  expect_warning({
    res <- hipercow_root(root = path)
  }, "Using legacy configuration for the 'example' driver")

  expect_equal(res$config, list(example = list(a = 1)))
})


test_that("modern configuration path takes precedence over the legacy", {
  # This situation shouldn't arise (we always delete the legacy path when
  # creating the new one), but we might as well ensure we handle it properly.

  path <- withr::local_tempfile()
  init_quietly(path)

  legacy_path <- file.path(path, "hipercow", "config", "example.rds")
  modern_path <-  file.path(path, "hipercow", "config", hostname(),
                            "example.rds")
  fs::dir_create(fs::path_dir(modern_path))

  saveRDS(list(a = 1), legacy_path)
  saveRDS(list(a = 2), modern_path)

  clear_cached_roots()

  res <- expect_no_warning(hipercow_root(path))
  expect_equal(res$config, list(example = list(a = 2)))
})


test_that("configure overwrites legacy configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)

  legacy_path <- file.path(path, "hipercow", "config", "example.rds")
  modern_path <-  file.path(path, "hipercow", "config", hostname(),
                            "example.rds")
  fs::dir_create(fs::path_dir(legacy_path))

  saveRDS(list(a = 1), legacy_path)

  clear_cached_roots()

  expect_warning({
    expect_message(
      hipercow_configure("example", root = path),
      "Migrated from legacy configuration for 'example'")
  }, "Using legacy configuration for the 'example' driver")

  expect_true(file.exists(modern_path))
  expect_false(file.exists(legacy_path))
})


test_that("can remove legacy configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)

  legacy_path <- file.path(path, "hipercow", "config", "example.rds")
  fs::dir_create(fs::path_dir(legacy_path))

  saveRDS(list(a = 1), legacy_path)

  clear_cached_roots()

  expect_warning({
    expect_message(
      hipercow_unconfigure("example", root = path),
      "Removed configuration for 'example'")
  }, "Using legacy configuration for the 'example' driver")

  expect_false(file.exists(legacy_path))
})


test_that("Configuration is scoped per-hostname", {
  path <- withr::local_tempfile()
  init_quietly(path)

  local_mocked_bindings(
    hipercow_driver_load = function(...) list(configure = list))

  with_mocked_bindings({
    clear_cached_roots()
    expect_message(
      hipercow_configure("example", a = 1, root = path),
      "Configured hipercow to use 'example'")
  }, hostname = function() "alice")

  with_mocked_bindings({
    clear_cached_roots()
    expect_message(
      hipercow_configure("example", a = 2, root = path),
      "Configured hipercow to use 'example'")
  }, hostname = function() "bob")

  with_mocked_bindings({
    clear_cached_roots()
    expect_equal(hipercow_root(path)$config$example, list(a = 1))
  }, hostname = function() "alice")

  with_mocked_bindings({
    clear_cached_roots()
    expect_equal(hipercow_root(path)$config$example, list(a = 2))
  }, hostname = function() "bob")
})
