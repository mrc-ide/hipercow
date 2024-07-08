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
         bundles = file.path(path_norm, "hipercow", "bundles"),
         retry = file.path(path_norm, "hipercow", "retry"),
         rrq = file.path(path_norm, "hipercow", "rrq"),
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


test_that("avoid finding the hipercow package by default", {
  path <- withr::local_tempdir()
  path_pkg <- file.path(path, "hipercow")
  fs::dir_create(path_pkg)
  writeLines("Package: hipercow", file.path(path_pkg, "DESCRIPTION"))
  path_tests <- file.path(path_pkg, "tests/testthat")
  fs::dir_create(path_tests)
  expect_error(hipercow_root_find(path_tests),
               "Found unlikely hipercow root")
})


test_that("Error if root not found", {
  path <- withr::local_tempdir()
  expect_error(hipercow_root(path),
               "Couldn't find hipercow root.")
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
  expect_true(fs::dir_exists(file.path(path_here, "hipercow")))
  expect_null(hipercow_root(path_here)$config)
})


test_that("sensible error if unexpected file found", {
  path <- withr::local_tempfile()
  fs::dir_create(path)
  file.create(file.path(path, "hipercow"))
  expect_error(
    hipercow_init(path),
    "Unexpected file 'hipercow' (rather than directory) found at",
    fixed = TRUE)
})


test_that("Prevent loading old root", {
  path_new <- withr::local_tempdir()
  init_quietly(path_new)
  d <- data.frame(x = 1:257)
  b <- withr::with_dir(
    path_new,
    suppressMessages(task_create_bulk_expr(sqrt(x), d)))

  path_old <- withr::local_tempdir()
  fs::dir_create(file.path(path_old, "hipercow", "tasks", b$ids))
  fs::file_copy(path_to_task_file(path_new, b$ids, "data"),
                file.path(path_old, "hipercow", "tasks", b$ids, "data"))

  expect_error(
    hipercow_root(path_old),
    "Your hipercow root is incompatible with this version of hipercow")
  expect_error(
    task_eval(b$ids[[13]], root = path_old),
    "Your hipercow root is incompatible with this version of hipercow")

  migrate_0_3_0(path_old)
  expect_true(task_eval(b$ids[[13]], root = path_old))
  expect_equal(task_result(b$ids[[13]], root = path_old), sqrt(13))
})


test_that("Report working directory if helpful", {
  path <- withr::local_tempdir()
  withr::with_dir(path,
    res <- testthat::evaluate_promise(hipercow_init("."))
  )
  msg <- substring(res$messages[[1]], 3)
  expect_match(msg, "Initialised hipercow at '.' (.+)\n")

  path <- withr::local_tempdir()
  withr::with_dir(path,
    res <- testthat::evaluate_promise(hipercow_init(path))
  )
  msg <- substring(res$messages[[1]], 3)
  expect_match(msg, "Initialised hipercow at '.+'\n")

})


test_that("Extraneous args are warned about", {
  path <- withr::local_tempdir()
  withr::with_dir(path,
    res <- testthat::evaluate_promise(
      hipercow_init(potato = TRUE, turnip = 42))
  )
  expect_length(res$messages, 3)
  expect_match(res$messages[1], "`driver` was not specified, but")
})
