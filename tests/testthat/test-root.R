test_that("can initialise a hipercow root", {
  path <- withr::local_tempfile()
  res1 <- testthat::evaluate_promise(hipercow_init(path))
  expect_match(res1$messages[[1]], "Initialised hipercow at '.+'")
  expect_match(res1$messages[[2]], "Next, call 'hipercow_configure()",
               fixed = TRUE)

  res2 <- testthat::evaluate_promise(hipercow_init(path))
  expect_match(res2$messages[[1]], "hipercow already initialised at '.+'")
  expect_equal(res2$messages[[2]], res1$messages[[2]])

  expect_s3_class(res1$result, "hipercow_root")
  expect_s3_class(res2$result, "hipercow_root")
  path_norm <- normalizePath(path, "/")
  expect_equal(
    res1$result$path,
    list(root = path_norm,
         tasks = file.path(path_norm, "hipercow", "tasks"),
         environments = file.path(path_norm, "hipercow", "environments"),
         config = file.path(path_norm, "hipercow", "config")))
  expect_identical(res1$result$path, res2$result$path)
  expect_identical(hipercow_root(res1$result), res1$result)
})


test_that("Can locate a hipercow root from a subdirectory", {
  path1 <- withr::local_tempfile()
  path2 <- file.path(path1, "a", "b", "c")
  dir.create(path2, FALSE, TRUE)
  r <- init_quietly(path1)
  expect_equal(hipercow_root(path2)$path, r$path)
})


test_that("Error if root not found", {
  path <- withr::local_tempdir()
  expect_error(hipercow_root(path))
})


test_that("can create a root and configure in one step", {
  elsewhere_register()
  path <- withr::local_tempdir()
  path_here <- file.path(path, "here")
  path_there <- file.path(path, "there")
  suppressMessages(hipercow_init(path_there))

  msg <- capture_messages(
    hipercow_init(path_here, "elsewhere", path = path_there))
  expect_length(msg, 2)
  expect_match(msg[[1]], "Initialised hipercow")
  expect_match(msg[[2]], "Configured hipercow to use 'elsewhere'")

  expect_equal(names(hipercow_root(path_here)$config), "elsewhere")
})


test_that("Failure to configure a root does not destroy it", {
  elsewhere_register()
  path <- withr::local_tempdir()
  path_here <- file.path(path, "here")
  path_there <- file.path(path, "there")

  err <- expect_error(
    suppressMessages(hipercow_init(path_here, "elsewhere", path = path_there)),
    "Configuration failed")
  expect_true(file.exists(file.path(path_here, "hipercow.json")))
  expect_null(hipercow_root(path_here)$config)
})
