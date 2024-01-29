test_that("can resolve functions when call is namespaced", {
  expect_mapequal(check_function(rlang::quo(ids::random_id)),
                  list(name = "random_id",
                       namespace = "ids",
                       value = NULL))
})


test_that("can resolve function when function is in a package", {
  myfn <- ids::random_id
  expect_mapequal(check_function(rlang::quo(myfn), NULL),
                  list(namespace = NULL, # the value
                       name = "myfn",
                       value = myfn))
})


test_that("can resolve a function when passed by value", {
  myfn <- function(x) x + 1
  expect_mapequal(check_function(rlang::quo(myfn), NULL),
                  list(value = myfn,
                       name = "myfn",
                       namespace = NULL))
})


test_that("can resolve an anonymous function", {
  res <- check_function(rlang::quo(function(x) x + 1), NULL)
  expect_mapequal(res,
                  list(value = function(x) x + 1,
                       name = NULL,
                       namespace = NULL))
})


test_that("can run a task using a namespaced function", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_call(base::sqrt, list(2)))
  expect_true(task_eval(id, root = path, verbose = FALSE))
  expect_equal(task_result(id, root = path), sqrt(2))
})


test_that("can run a task using a function by name", {
  path <- withr::local_tempdir()
  init_quietly(path)
  myfn <- sqrt
  id <- withr::with_dir(path, task_create_call(myfn, list(2)))
  env <- new.env(parent = .GlobalEnv)
  expect_true(task_eval(id, envir = env, root = path, verbose = FALSE))
  expect_equal(env$myfn, sqrt)
  expect_equal(task_result(id, root = path), sqrt(2))
})


test_that("can run a task using a function by name", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_call(function(x) x + 1, list(2)))
  env <- new.env(parent = .GlobalEnv)
  expect_true(task_eval(id, envir = env, root = path, verbose = FALSE))
  expect_equal(task_result(id, root = path), 3)
})
