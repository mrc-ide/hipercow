test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_expr(sqrt(2)))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(task_status(id, root = path), "created")

  d <- readRDS(file.path(path, "hipercow", "tasks", id, "data"))
  expect_equal(d$type, "expression")
  expect_equal(d$expr, quote(sqrt(2)))
  expect_null(d$variables)
  expect_equal(d$path, ".")
  expect_equal(d$environment, "default")

  expect_true(task_eval(id, root = path))
  expect_equal(task_status(id, root = path), "success")
  expect_equal(task_result(id, root = path), sqrt(2))
})


test_that("must use a function call", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    withr::with_dir(path, task_create_expr(1)),
    "Expected 'expr' to be a function call")
})


test_that("Can create and run a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  a <- 2
  id <- withr::with_dir(path, task_create_expr(sqrt(a)))
  expect_type(id, "character")
  expect_match(id, "^[[:xdigit:]]{32}$")
  expect_equal(task_status(id, root = path), "created")

  d <- readRDS(file.path(path, "hipercow", "tasks", id, "data"))
  expect_equal(d$type, "expression")
  expect_equal(d$expr, quote(sqrt(a)))
  expect_equal(d$variables, list(locals = list(a = 2), globals = NULL))
  expect_equal(d$path, ".")
  expect_equal(d$environment, "default")

  expect_true(task_eval(id, root = path))
  expect_equal(task_status(id, root = path), "success")
  expect_equal(task_result(id, root = path), sqrt(2))
})


test_that("can use escape hatch", {
  path <- withr::local_tempdir()
  init_quietly(path)

  expr1 <- quote(sqrt(2))
  id1 <- withr::with_dir(path, task_create_expr(expr1))
  d1 <- readRDS(file.path(path, "hipercow", "tasks", id1, "data"))
  expect_equal(d1$type, "expression")
  expect_equal(d1$expr, quote(sqrt(2)))
  expect_null(d1$variables)

  ## Also works with expressions that reference variables, for simple
  ## cases at least.
  a <- 2
  expr2 <- quote(sqrt(a))
  id2 <- withr::with_dir(path, task_create_expr(expr2))

  d2 <- readRDS(file.path(path, "hipercow", "tasks", id2, "data"))
  expect_equal(d2$type, "expression")
  expect_equal(d2$expr, quote(sqrt(a)))
  expect_equal(d2$variables, list(locals = list(a = 2), globals = NULL))
})


test_that("pulling from a symbol must still be a call", {
  path <- withr::local_tempdir()
  init_quietly(path)

  myexpr <- quote(sqrt)
  err <- expect_error(
    withr::with_dir(path, task_create_expr(myexpr)),
    "Expected 'expr' to be a function call")
  expect_match(err$body[[1]], "You passed a symbol 'myexpr'")
})


test_that("symbols that might contain expressions must exist", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    withr::with_dir(path, task_create_expr(someexpr)),
    "Could not find expression 'someexpr'")
})


test_that("error on double quote", {
  path <- withr::local_tempdir()
  init_quietly(path)
  err <- expect_error(
    withr::with_dir(path, task_create_expr(quote(f(x)))),
    "You have an extra layer of quote() around 'expr'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "You passed 'quote(f(x))' but probably meant to pass 'f(x)'"))

  expr <- quote(quote(g(y)))
  err <- expect_error(
    withr::with_dir(path, task_create_expr(expr)),
    "You have an extra layer of quote() around 'expr'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "You passed 'quote(g(y))' but probably meant to pass 'g(y)'"))
})


test_that("can run a task that uses variable from globals", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 1", file.path(path, "src.R"))
  suppressMessages(
    hipercow_environment_create("foo", sources = "src.R", globals = "a",
                                root = path))
  withr::local_options(hipercow.validate_globals = FALSE)
  id1 <- withr::with_dir(path,
                         task_create_expr(sqrt(a), environment = "foo"))
  d <- readRDS(file.path(path, "hipercow", "tasks", id1, "data"))
  expect_equal(d$variables,
               list(locals = set_names(list(), character()), globals = NULL))

  b <- 2
  id2 <- withr::with_dir(path,
                         task_create_expr(atan2(a, b), environment = "foo"))
  d <- readRDS(file.path(path, "hipercow", "tasks", id2, "data"))
  expect_equal(d$variables,
               list(locals = list(b = 2), globals = NULL))
})


test_that("can save information for global validation", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 1", file.path(path, "src.R"))
  suppressMessages(
    hipercow_environment_create("foo", sources = "src.R", globals = "a",
                                root = path))

  a <- 1
  b <- 2
  withr::local_options(hipercow.validate_globals = TRUE)
  id1 <- withr::with_dir(path,
                         task_create_expr(sqrt(a), environment = "foo"))
  d <- readRDS(file.path(path, "hipercow", "tasks", id1, "data"))
  expect_equal(d$variables,
               list(locals = set_names(list(), character()),
                    globals = c(a = rlang::hash(a))))

  id2 <- withr::with_dir(path,
                         task_create_expr(atan2(a, b), environment = "foo"))
  d <- readRDS(file.path(path, "hipercow", "tasks", id2, "data"))
  expect_equal(d$variables,
               list(locals = list(b = 2), globals = c(a = rlang::hash(a))))
})


test_that("can validate globals on load", {
  path <- withr::local_tempfile()
  root <- init_quietly(path)
  writeLines("a <- 2", file.path(path, "src.R"))
  suppressMessages(
    hipercow_environment_create("foo", sources = "src.R", globals = "a",
                                root = path))

  a <- 2
  withr::local_options(hipercow.validate_globals = TRUE)
  id1 <- withr::with_dir(path,
                         task_create_expr(sqrt(a), environment = "foo"))
  id2 <- withr::with_dir(path,
                         task_create_expr(sqrt(a), environment = "foo"))

  env <- new.env(parent = topenv())
  expect_true(task_eval(id1, env, root = path))
  expect_equal(task_result(id1, root = path), sqrt(2))
  writeLines("a <- 3", file.path(path, "src.R"))
  expect_false(task_eval(id2, env, root = path))
  err <- task_result(id2, root = path)
  expect_match(err$message, "Unexpected value for global variable: 'a'")
})


test_that("can allow more complex expressions through", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path, task_create_expr({
      x <- 3
      sqrt(x)
    }))
  env <- new.env(parent = topenv())
  expect_true(task_eval(id, env, root = path))
  expect_equal(env$x, 3)
  expect_equal(task_result(id, root = path), sqrt(3))
})
