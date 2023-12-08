test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, hermod_task_create_expression(sqrt(2)))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(hermod_task_status(id, root = path), "created")

  d <- readRDS(file.path(path, "hermod", "tasks", id, "expr"))
  expect_equal(d$type, "expression")
  expect_equal(d$expr, quote(sqrt(2)))
  expect_null(d$locals)
  expect_equal(d$path, ".")
  expect_equal(d$environment, "default")

  expect_true(hermod_task_eval(id, root = path))
  expect_equal(hermod_task_status(id, root = path), "success")
  expect_equal(hermod_task_result(id, root = path), sqrt(2))
})


test_that("must use a function call", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    withr::with_dir(path, hermod_task_create_expression(1)),
    "Expected 'expr' to be a function call")
})


test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  a <- 2
  id <- withr::with_dir(path, hermod_task_create_expression(sqrt(a)))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(hermod_task_status(id, root = path), "created")

  d <- readRDS(file.path(path, "hermod", "tasks", id, "expr"))
  expect_equal(d$type, "expression")
  expect_equal(d$expr, quote(sqrt(a)))
  expect_equal(d$locals, list(a = 2))
  expect_equal(d$path, ".")
  expect_equal(d$environment, "default")

  expect_true(hermod_task_eval(id, root = path))
  expect_equal(hermod_task_status(id, root = path), "success")
  expect_equal(hermod_task_result(id, root = path), sqrt(2))
})


test_that("can use escape hatch", {
  path <- withr::local_tempdir()
  init_quietly(path)

  expr1 <- quote(sqrt(2))
  id1 <- withr::with_dir(path, hermod_task_create_expression(expr1))
  d1 <- readRDS(file.path(path, "hermod", "tasks", id1, "expr"))
  expect_equal(d1$type, "expression")
  expect_equal(d1$expr, quote(sqrt(2)))
  expect_null(d1$locals)

  ## Also works with expressions that reference variables, for simple
  ## cases at least.
  a <- 2
  expr2 <- quote(sqrt(a))
  id2 <- withr::with_dir(path, hermod_task_create_expression(expr2))

  d2 <- readRDS(file.path(path, "hermod", "tasks", id2, "expr"))
  expect_equal(d2$type, "expression")
  expect_equal(d2$expr, quote(sqrt(a)))
  expect_equal(d2$locals, list(a = 2))
})


test_that("pulling from a symbol must still be a call", {
  path <- withr::local_tempdir()
  init_quietly(path)

  myexpr <- quote(sqrt)
  err <- expect_error(
    withr::with_dir(path, hermod_task_create_expression(myexpr)),
    "Expected 'expr' to be a function call")
  expect_match(err$body[[1]], "You passed a symbol 'myexpr'")
})


test_that("pulling from a symbol must still be a call", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    withr::with_dir(path, hermod_task_create_expression(someexpr)),
    "Could not find expression 'someexpr'")
})


test_that("error on double quote", {
  path <- withr::local_tempdir()
  init_quietly(path)
  err <- expect_error(
    withr::with_dir(path, hermod_task_create_expression(quote(f(x)))),
    "You have an extra layer of quote() around 'expr'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "You passed 'quote(f(x))' but probably meant to pass 'f(x)'"))

  expr <- quote(quote(g(y)))
  err <- expect_error(
    withr::with_dir(path, hermod_task_create_expression(expr)),
    "You have an extra layer of quote() around 'expr'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "You passed 'quote(g(y))' but probably meant to pass 'g(y)'"))
})
