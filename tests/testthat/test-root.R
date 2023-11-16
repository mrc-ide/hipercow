test_that("can initialise a hermod root", {
  path <- withr::local_tempfile()
  expect_message(r1 <- hermod_init(path), "Initialised hermod at '.+'")
  expect_message(r2 <- hermod_init(path), "hermod already initialised at '.+'")
  expect_s3_class(r1, "hermod_root")
  expect_s3_class(r2, "hermod_root")
  path_norm <- normalizePath(path, "/")
  expect_equal(r1$path,
               list(root = path_norm,
                    tasks = file.path(path_norm, "hermod", "tasks")))
  expect_identical(r1$path, r2$path)
  expect_identical(hermod_root(r1), r1)
})


test_that("Can locate a hermod root from a subdirectory", {
  path1 <- withr::local_tempfile()
  path2 <- file.path(path1, "a", "b", "c")
  dir.create(path2, FALSE, TRUE)
  suppressMessages(r <- hermod_init(path1))
  expect_equal(hermod_root(path2)$path, r$path)
})


test_that("Error if root not found", {
  path <- withr::local_tempdir()
  expect_error(hermod_root(path))
})
