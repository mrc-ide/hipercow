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

  d <- lapply(path_to_task_file(path, id, "data"), readRDS)
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

  d <- lapply(path_to_task_file(path, id, "data"), readRDS)
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


test_that("can create a bulk lapply-like bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)

  x <- 1:10
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, x)))
  expect_length(b$ids, 10)

  d <- task_info(b$ids[[2]], root = path)$data
  expect_equal(d$fn$name, "sqrt")
  expect_equal(d$args, list(2))
  expect_true(task_eval(b$ids[[2]], root = path))
  expect_equal(task_result(b$ids[[2]], root = path), sqrt(2))
})


test_that("can pass additional arguments in via lapply interface", {
  path <- withr::local_tempdir()
  init_quietly(path)

  x <- 1:10
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(log, x, args = list(base = 2))))
  expect_length(b$ids, 10)

  d <- task_info(b$ids[[5]], root = path)$data
  expect_equal(d$fn$name, "log")
  expect_equal(d$args, list(5, base = 2))
  expect_true(task_eval(b$ids[[5]], root = path))
  expect_equal(task_result(b$ids[[5]], root = path), log(5, 2))
})


test_that("can iterate over a data.frame", {
  path <- withr::local_tempdir()
  init_quietly(path)
  x <- data.frame(a = 1:10, b = runif(10))
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(function(a, b) a / b, x)))

  expect_length(b$ids, 10)

  d <- task_info(b$ids[[2]], root = path)$data
  expect_null(d$fn$name)
  expect_type(d$fn$value, "closure")
  expect_equal(d$args, list(a = 2, b = x$b[[2]]))
  expect_true(task_eval(b$ids[[2]], root = path))
  expect_equal(task_result(b$ids[[2]], root = path), 2 / x$b[[2]])
})


test_that("prevent creation of zero-length bundles", {
  path <- withr::local_tempdir()
  init_quietly(path)
  withr::with_dir(
    path,
    expect_error(
      task_create_bulk_call(identity, numeric(0)),
      "'data' must have at least one element"))
  withr::with_dir(
    path,
    expect_error(
      task_create_bulk_call(identity, data.frame(x = numeric(0))),
      "'data' must have at least one row"))
  withr::with_dir(
    path,
    expect_error(
      task_create_bulk_expr(identity(x), data.frame(x = numeric(0))),
      "'data' must have at least one row"))
})
