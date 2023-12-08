test_that("can initialise a hermod root", {
  path <- withr::local_tempfile()
  res1 <- testthat::evaluate_promise(hermod_init(path))
  expect_match(res1$messages[[1]], "Initialised hermod at '.+'")
  expect_match(res1$messages[[2]], "Next, call 'hermod_configure()",
               fixed = TRUE)

  res2 <- testthat::evaluate_promise(hermod_init(path))
  expect_match(res2$messages[[1]], "hermod already initialised at '.+'")
  expect_equal(res2$messages[[2]], res1$messages[[2]])

  expect_s3_class(res1$result, "hermod_root")
  expect_s3_class(res2$result, "hermod_root")
  path_norm <- normalizePath(path, "/")
  expect_equal(
    res1$result$path,
    list(root = path_norm,
         tasks = file.path(path_norm, "hermod", "tasks"),
         environments = file.path(path_norm, "hermod", "environments"),
         config = file.path(path_norm, "hermod", "config")))
  expect_identical(res1$result$path, res2$result$path)
  expect_identical(hermod_root(res1$result), res1$result)
})


test_that("Can locate a hermod root from a subdirectory", {
  path1 <- withr::local_tempfile()
  path2 <- file.path(path1, "a", "b", "c")
  dir.create(path2, FALSE, TRUE)
  r <- init_quietly(path1)
  expect_equal(hermod_root(path2)$path, r$path)
})


test_that("Error if root not found", {
  path <- withr::local_tempdir()
  expect_error(hermod_root(path))
})
