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

  bootstrap_update(development = TRUE, root = root)
  mockery::expect_called(mock_hipercow_provision, 1)
  expect_true(file.exists(
    file.path(root$path$root, "hipercow", "bootstrap-windows.R")))
  expect_equal(
    mockery::mock_args(mock_hipercow_provision)[[1]],
    list("script", script = "hipercow/bootstrap-windows.R", root = root))
  s <- readLines(file.path(root$path$root, "hipercow", "bootstrap-windows.R"))
  expect_match(s[[1]], "I:/bootstrap-dev/")
})


test_that("respond to option to select dev bootstrap", {
  config <- list(r_version = numeric_version("4.3.2"))
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
