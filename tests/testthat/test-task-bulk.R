test_that("bulk creation of a task", {
  path <- withr::local_tempdir()
  init_quietly(path)

  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  x <- 1
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_expr(list(x, a, b), d)))
  expect_s3_class(b, "hipercow_bundle")

  id <- b$ids

  expect_length(id, 3)
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$", all = TRUE)
  expect_equal(task_status(id, root = path), rep("created", 3))

  d <- lapply(file.path(path, "hipercow", "tasks", id, "data"), readRDS)
  expect_equal(d[[1]]$variables$locals, list(x = 1, a = 1, b = "x"))
  expect_equal(d[[2]]$variables$locals, list(x = 1, a = 2, b = "y"))
  expect_equal(d[[3]]$variables$locals, list(x = 1, a = 3, b = "z"))

  expect_equal(d[[1]]$expr, quote(list(x, a, b)))
  expect_equal(d[[1]]$environment, "default")
  expect_equal(d[[1]]$path, ".")
  expect_equal(d[[1]]$type, "expression")

  v <- c("type", "path", "environment", "data")
  expect_equal(d[[2]][v], d[[1]][v])
  expect_equal(d[[3]][v], d[[1]][v])
})


test_that("use splicing to disambiguate expressions", {
  path <- withr::local_tempdir()
  init_quietly(path)

  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  a <- 1
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_expr(list(!!a, a, b), d)))
  id <- b$ids

  d <- lapply(file.path(path, "hipercow", "tasks", id, "data"), readRDS)
  expect_equal(d[[1]]$variables$locals, list(a = 1, b = "x"))
  expect_equal(d[[2]]$variables$locals, list(a = 2, b = "y"))
  expect_equal(d[[3]]$variables$locals, list(a = 3, b = "z"))
  expect_equal(d[[1]]$expr, quote(list(1, a, b)))
  expect_equal(d[[2]]$expr, quote(list(1, a, b)))
  expect_equal(d[[3]]$expr, quote(list(1, a, b)))
})


test_that("require that data is a data.frame", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    withr::with_dir(path, task_create_bulk_expr(list(x, a, b), NULL)),
    "Expected 'data' to be a data.frame")
})
