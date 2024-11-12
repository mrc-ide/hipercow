test_that("can report on configuration", {
  path <- withr::local_tempfile()
  init_quietly(path)
  path <- normalize_path(path)
  root <- hipercow_root(path)
  res <- evaluate_promise(
    withr::with_dir(path, hipercow_configuration()))

  cfg <- res$result
  expect_type(cfg, "list")
  expect_equal(names(cfg),
               c("platform", "packages", "paths", "environments", "drivers"))
  expect_equal(cfg$platform, configuration_platform())
  expect_equal(cfg$packages, configuration_packages())
  expect_equal(cfg$paths, withr::with_dir(path, configuration_paths(root)))
  expect_equal(cfg$drivers, configuration_drivers(root))
  expect_silent(
    withr::with_dir(path, hipercow_configuration(show = FALSE)))

  expect_null(cfg$drivers)

  cmp <- withVisible(
    withr::with_dir(path, hipercow_configuration(show = FALSE)))
  expect_equal(cmp$value, cfg)
  expect_false(cmp$visible)
})


test_that("can report on configuration with a driver configured", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  res <- evaluate_promise(
    withr::with_dir(path_here, hipercow_configuration()))
  cfg <- res$result
  expect_type(cfg, "list")
  expect_equal(names(cfg),
               c("platform", "packages", "paths", "environments", "drivers"))
  expect_equal(cfg$platform, configuration_platform())
  expect_equal(cfg$packages, configuration_packages())
  expect_equal(cfg$paths, withr::with_dir(path_here, configuration_paths(root)))
  expect_equal(cfg$drivers, configuration_drivers(root))
  expect_silent(
    withr::with_dir(path_here, hipercow_configuration(show = FALSE)))

  expect_equal(cfg$drivers, list(elsewhere = root$config$elsewhere))

  cmp <- withVisible(
    withr::with_dir(path_here, hipercow_configuration(show = FALSE)))
  expect_equal(cmp$value, cfg)
  expect_false(cmp$visible)
})


test_that("can report on configuration with an unconfigurable driver", {
  path <- withr::local_tempfile()
  init_quietly(path, driver = "example")
  res <- evaluate_promise(
    withr::with_dir(path, hipercow_configuration()))
  msg <- capture_messages(configuration_render_drivers(res$result$drivers))
  expect_match(msg, "Drivers", all = FALSE)
  expect_match(msg, "1 driver configured ('example')",
               fixed = TRUE, all = FALSE)
  expect_match(msg, "(unconfigurable)", fixed = TRUE, all = FALSE)
  expect_true(all(msg %in% res$messages))
})


test_that("can report about package version problems", {
  mock_version <- mockery::mock(1, 2, 3, 4, 5)
  mockery::stub(configuration_packages, "package_version_if_installed",
                mock_version)
  res <- configuration_packages()
  expect_equal(res$hipercow, 1)
  expect_equal(res$others,
               list(hipercow.windows = 2, conan2 = 3, logwatch = 4, rrq = 5))
  expect_equal(
    res$notes,
    c("!" = "hipercow and hipercow.windows have different versions"))
})


test_that("can report about missing packages", {
  mock_version <- mockery::mock(1, 2, NULL, NULL, NULL)
  mockery::stub(configuration_packages, "package_version_if_installed",
                mock_version)
  res <- configuration_packages()
  expect_equal(res$hipercow, 1)
  expect_equal(res$others, list(hipercow.windows = 2))
  expect_equal(
    res$notes,
    c("x" = "conan2 is not installed",
      "!" = "hipercow and hipercow.windows have different versions"))
})


test_that("can report about everything being missing", {
  mock_version <- mockery::mock(1, NULL, NULL, NULL, NULL)
  mockery::stub(configuration_packages, "package_version_if_installed",
                mock_version)
  res <- configuration_packages()
  expect_equal(res$hipercow, 1)
  expect_equal(res$others, set_names(list(), character()))
  expect_equal(
    res$notes,
    c("x" = "hipercow.windows is not installed",
      "x" = "conan2 is not installed"))
})


test_that("can add windows username to configuration", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  mock_username <- mockery::mock("alice")
  mockery::stub(configuration_drivers, "windows_username", mock_username)
  res <- configuration_drivers(root)
  mockery::expect_called(mock_username, 0)
  expect_equal(res, root$config)

  root$config <- c(root$config, list(windows = list(a = 1, b = 2)))
  res <- configuration_drivers(root)
  mockery::expect_called(mock_username, 1)
  cmp <- root$config
  cmp$windows$username <- "alice"
  expect_equal(res, cmp)
})


test_that("don't error if windows username lookup fails", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))

  mock_username <- mockery::mock(stop("error looking up windows username"))
  mockery::stub(configuration_drivers, "windows_username", mock_username)

  root$config <- c(root$config, list(windows = list(a = 1, b = 2)))
  warn <- expect_warning(
    res <- configuration_drivers(root),
    "Failed to read windows username")
  mockery::expect_called(mock_username, 1)
  cmp <- root$config
  cmp$windows$username <- "(???)"
  expect_equal(res, cmp)

  expect_equal(conditionMessage(warn$parent),
               "error looking up windows username")
})
