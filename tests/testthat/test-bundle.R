test_that("can create a simple task bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids <- ids::random_id(3)
  b <- hipercow_bundle_create(ids, validate = FALSE, root = path)

  expect_s3_class(b, "hipercow_bundle")
  expect_setequal(names(b), c("name", "ids"))
  expect_equal(b$ids, ids)
  expect_type(b$name, "character")
  expect_true(file.exists(file.path(path, "hipercow", "bundles", b$name)))
})


test_that("can create a simple task bundle with fixed name", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids <- ids::random_id(3)
  expect_equal(
    hipercow_bundle_create(ids, name = "foo", validate = FALSE, root = path),
    new_bundle("foo", ids))
})


test_that("bundles must have at least one id", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(hipercow_bundle_create(NULL, root = path),
               "Can't make a bundle with no tasks")
  expect_error(hipercow_bundle_create(character(), root = path),
               "Can't make a bundle with no tasks")
})


test_that("can control overwriting with 'overwrite' arg", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids1 <- ids::random_id(1)
  ids2 <- ids::random_id(2)
  name <- "mybundle"
  b1 <- hipercow_bundle_create(ids1, name = name, validate = FALSE, root = path)
  expect_equal(hipercow_bundle_load(name, root = path), b1)

  b2 <- hipercow_bundle_create(ids2, name = name, validate = FALSE, root = path)
  expect_equal(hipercow_bundle_load(name, root = path), b2)

  ## No reference tricks:
  expect_equal(b1$ids, ids1)
  expect_equal(b2$ids, ids2)

  expect_error(
    hipercow_bundle_create(ids1, name = name, validate = FALSE,
                           overwrite = FALSE, root = path),
    "Bundle 'mybundle' exists and overwrite is FALSE")
})


test_that("can't load nonexistant bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    hipercow_bundle_load("mybundle", root = path),
    "No such bundle 'mybundle'")
})


test_that("can list bundles", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids1 <- ids::random_id(1)
  ids2 <- ids::random_id(2)

  res0 <- hipercow_bundle_list(path)
  expect_equal(nrow(res0), 0)
  expect_equal(res0$name, character())
  expect_equal(res0$time, Sys.time()[0])

  b1 <- hipercow_bundle_create(ids1, validate = FALSE, root = path)
  res1 <- hipercow_bundle_list(path)
  expect_equal(nrow(res1), 1)
  expect_equal(res1$name, b1$name)
  expect_s3_class(res1$time, class(Sys.time()), exact = TRUE)

  ## Need to sleep here for a bit so that windows definitely puts the
  ## second bundle second!
  Sys.sleep(1)

  b2 <- hipercow_bundle_create(ids2, validate = FALSE, root = path)
  res2 <- hipercow_bundle_list(path)
  expect_equal(nrow(res2), 2)
  expect_equal(res2$name, c(b2$name, b1$name))
  expect_s3_class(res2$time, class(Sys.time()), exact = TRUE)
})


test_that("can delete bundles", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b1 <- hipercow_bundle_create(ids::random_id(1), validate = FALSE, root = path)
  b2 <- hipercow_bundle_create(ids::random_id(2), validate = FALSE, root = path)
  expect_equal(nrow(hipercow_bundle_list(root = path)), 2)

  hipercow_bundle_delete(b1$name, root = path)
  expect_equal(hipercow_bundle_list(root = path)$name, b2$name)

  hipercow_bundle_delete("something_else", root = path)
  expect_equal(hipercow_bundle_list(root = path)$name, b2$name)
  hipercow_bundle_delete(b2$name, root = path)
  expect_equal(hipercow_bundle_list(root = path)$name, character())
})


test_that("bundles must be composed of sensible names", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    hipercow_bundle_create("a", validate = FALSE, root = path),
    "All entries in 'ids' must be valid ids (32 character hex strings)",
    fixed = TRUE)
})


test_that("can validate that tasks really exist", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, task_create_expr(runif(1)))
  expect_equal(hipercow_bundle_create(id1, name = "b", root = path),
               new_bundle("b", id1))
  id2 <- ids::random_id()
  expect_error(
    hipercow_bundle_create(id2, name = "b", root = path),
    "Can't include tasks in bundle that don't exist, validate is 'TRUE'")
  expect_equal(
    hipercow_bundle_create(id2, validate = FALSE, name = "b", root = path),
    new_bundle("b", id2))
})


test_that("can print a bundle", {
  expect_message(
    print(new_bundle("a", "x")),
    "hipercow_bundle 'a' with 1 task")
  expect_message(
    print(new_bundle("a", c("x", "y", "z"))),
    "hipercow_bundle 'a' with 3 tasks")
})
