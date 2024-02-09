test_that("can resolve functions when call is namespaced", {
  expect_mapequal(check_function(rlang::quo(ids::random_id)),
                  list(name = "random_id",
                       namespace = "ids",
                       value = NULL))
})


test_that("can resolve functions when call is non-exported", {
  expect_mapequal(check_function(rlang::quo(ids:::toupper_initial)),
                  list(name = NULL,
                       namespace = NULL,
                       value = quote(ids:::toupper_initial)))
})


test_that("can resolve function when function is in a package", {
  myfn <- ids::random_id
  expect_mapequal(check_function(rlang::quo(myfn)),
                  list(namespace = NULL, # the value
                       name = "myfn",
                       value = myfn))
})


test_that("symbols must resolve to functions immediately in the environment", {
  foo <- TRUE
  expect_error(check_function(rlang::quo(foo)),
               "The symbol 'foo' is not a function")
  myfn <- sqrt
  local({
    myfn <- 1:10
    expect_error(check_function(rlang::quo(myfn)),
                 "The symbol 'myfn' is not a function")
  })
})


test_that("can resolve a function when passed by value", {
  myfn <- function(x) x + 1
  expect_mapequal(check_function(rlang::quo(myfn)),
                  list(value = myfn,
                       name = "myfn",
                       namespace = NULL))
})


test_that("can resolve an anonymous function", {
  res <- check_function(rlang::quo(function(x) x + 1))
  expect_mapequal(res,
                  list(value = function(x) x + 1,
                       name = NULL,
                       namespace = NULL))
})


test_that("objects passed by value must be a function", {
  expect_error(check_function(rlang::quo(list(1, 2, 3))),
               "The value passed is not a function")
})


test_that("check arguments for call", {
  expect_equal(check_args(NULL), list())
  expect_equal(check_args(list()), list())
  expect_equal(check_args(list(1, 2)), list(1, 2))
  expect_equal(check_args(list(1, a = 2)), list(1, a = 2))
  expect_error(check_args(1),
               "Expeced a list for 'args'")
})


test_that("can run a task using a namespaced function", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_call(base::sqrt, list(2)))
  expect_true(task_eval(id, root = path, verbose = FALSE))
  expect_equal(task_result(id, root = path), sqrt(2))

  msg <- capture_messages(print(task_info(id, root = path)))
  expect_match(msg, "Call: base::sqrt", all = FALSE, fixed = TRUE)
  expect_match(msg, "Args: 2", all = FALSE, fixed = TRUE)
})


test_that("can run a task using a non-exported function", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path,
                        task_create_call(ids:::toupper_initial, list("foo")))
  expect_true(task_eval(id, root = path, verbose = FALSE))
  expect_equal(task_result(id, root = path), "Foo")

  msg <- capture_messages(print(task_info(id, root = path)))
  expect_match(msg, "Call: ids:::toupper_initial", all = FALSE, fixed = TRUE)
  expect_match(msg, 'Args: "foo"', all = FALSE, fixed = TRUE)
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
