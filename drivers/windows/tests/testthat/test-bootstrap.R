test_that("can run bootstrap", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  mock_hipercow_provision <- mockery::mock()
  mockery::stub(bootstrap_update, "hipercow::hipercow_provision",
                mock_hipercow_provision)

  bootstrap_update(root = root)
  mockery::expect_called(mock_hipercow_provision, 1)
  expect_true(file.exists(
    file.path(root$path$root, "hipercow", "bootstrap-windows.R")))
  expect_equal(
    mockery::mock_args(mock_hipercow_provision)[[1]],
    list("script", script = "hipercow/bootstrap-windows.R", root = root))
  s <- readLines(file.path(root$path$root, "hipercow", "bootstrap-windows.R"))
  expect_match(s[[1]], "I:/bootstrap/")
})


test_that("can run development bootstrap", {
  mount <- withr::local_tempfile()
  root <- example_root(mount, "b/c")
  mock_hipercow_provision <- mockery::mock()
  mockery::stub(bootstrap_update, "hipercow::hipercow_provision",
                mock_hipercow_provision)

  bootstrap_update(development = "mrc-4827", root = root)
  mockery::expect_called(mock_hipercow_provision, 1)
  expect_true(file.exists(
    file.path(root$path$root, "hipercow", "bootstrap-windows.R")))
  expect_equal(
    mockery::mock_args(mock_hipercow_provision)[[1]],
    list("script", script = "hipercow/bootstrap-windows.R", root = root))
  s <- readLines(file.path(root$path$root, "hipercow", "bootstrap-windows.R"))
  expect_match(s[[1]], "I:/bootstrap-dev/")
  expect_match(
    s,
    'remotes::install_github("mrc-ide/hipercow", ref = "mrc-4827",',
    fixed = TRUE, all = FALSE)
})


test_that("respond to option to select dev bootstrap - windows", {
  config <- list(r_version = numeric_version("4.3.2"),
                 platform = "windows")
  withr::with_options(
    list(hipercow.development = NULL),
    expect_equal(path_bootstrap(config), "I:/bootstrap/4.3.2"))
  withr::with_options(
    list(hipercow.development = FALSE),
    expect_equal(path_bootstrap(config), "I:/bootstrap/4.3.2"))
  withr::with_options(
    list(hipercow.development = TRUE),
    expect_equal(path_bootstrap(config), "I:/bootstrap-dev/4.3.2"))
})

test_that("respond to option to select dev bootstrap - linux", {
  config <- list(r_version = numeric_version("4.3.2"),
                 platform = "linux")
  withr::with_options(
    list(hipercow.development = NULL),
    expect_equal(path_bootstrap(config),
                 "/wpia-hn/Hipercow/bootstrap-linux/4.3.2"))
  withr::with_options(
    list(hipercow.development = FALSE),
    expect_equal(path_bootstrap(config),
                 "/wpia-hn/Hipercow/bootstrap-linux/4.3.2"))
  withr::with_options(
    list(hipercow.development = TRUE),
    expect_equal(path_bootstrap(config),
                 "/wpia-hn/Hipercow/bootstrap-dev-linux/4.3.2"))
})


test_that("can find recent versions", {
  v <- c("4.0.5", "4.1.3", "4.2.3", "4.3.0")
  expect_equal(recent_versions(numeric_version(v)), numeric_version(v[3:4]))
})


test_that("bootstrap iterates through correct versions", {
  mock_update <- mockery::mock()
  mock_init <- mockery::mock()
  mock_versions <- mockery::mock(
    numeric_version(c("4.0.5", "4.1.3", "4.2.3", "4.3.0")), cycle = TRUE)
  mockery::stub(bootstrap_update_all, "bootstrap_update", mock_update)
  mockery::stub(bootstrap_update_all, "hipercow::hipercow_init", mock_init)
  mockery::stub(bootstrap_update_all, "r_versions", mock_versions)

  suppressMessages(bootstrap_update_all())

  mockery::expect_called(mock_versions, 2)
  mockery::expect_called(mock_init, 4)
  expect_equal(
    mockery::mock_args(mock_init)[[1]],
    list(".", driver = "dide-windows", r_version = numeric_version("4.2.3")))
  expect_equal(
    mockery::mock_args(mock_init)[[2]],
    list(".", driver = "dide-windows", r_version = numeric_version("4.3.0")))
  mockery::expect_called(mock_update, 4)
  expect_equal(
    mockery::mock_args(mock_update),
    list(list(development = NULL, root = NULL, platform = "windows"),
         list(development = NULL, root = NULL, platform = "windows"),
         list(development = NULL, root = NULL, platform = "linux"),
         list(development = NULL, root = NULL, platform = "linux")
    ))
})
