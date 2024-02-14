test_that("can create a simple task bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids <- ids::random_id(3)
  expect_message(
    b <- hipercow_bundle_create(ids, validate = FALSE, root = path),
    "Created bundle '.+' with 3 tasks")

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
  expect_message(
    b <- hipercow_bundle_create(ids, name = "foo", validate = FALSE,
                                root = path),
    "Created bundle 'foo' with 3 tasks")
  expect_equal(b, new_bundle("foo", ids))
})


test_that("bundles must have at least one id", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(hipercow_bundle_create(NULL, root = path),
               "Can't make a bundle with no tasks")
  expect_error(hipercow_bundle_create(character(), root = path),
               "Can't make a bundle with no tasks")
})


test_that("bundle names must be filename-safe", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids <- ids::random_id(3)
  expect_error(hipercow_bundle_create(ids, name = "foo/bar", root = path),
               "Invalid bundle name 'foo/bar'")
})


test_that("can control overwriting with 'overwrite' arg", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids1 <- ids::random_id(1)
  ids2 <- ids::random_id(2)
  name <- "mybundle"
  b1 <- suppressMessages(
    hipercow_bundle_create(ids1, name = name, validate = FALSE, root = path))
  expect_equal(hipercow_bundle_load(name, root = path), b1)

  b2 <- suppressMessages(
    hipercow_bundle_create(ids2, name = name, validate = FALSE, root = path))
  expect_equal(hipercow_bundle_load(name, root = path), b2)

  ## No reference tricks:
  expect_equal(b1$ids, ids1)
  expect_equal(b2$ids, ids2)

  expect_error(
    hipercow_bundle_create(ids1, name = name, validate = FALSE,
                           overwrite = FALSE, root = path),
    "Bundle 'mybundle' exists and overwrite is FALSE")
})


test_that("can load bundles nicely with check_bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  ids <- ids::random_id(3)
  expect_message(
    b <- hipercow_bundle_create(ids, validate = FALSE, root = path),
    "Created bundle '.+' with 3 tasks")
  expect_identical(check_bundle(b, root), b)
  expect_identical(check_bundle(b$name, root), b)
  expect_error(check_bundle(TRUE, root),
               "Invalid value for 'bundle'")
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

  b1 <- suppressMessages(
    hipercow_bundle_create(ids1, validate = FALSE, root = path))
  res1 <- hipercow_bundle_list(path)
  expect_equal(nrow(res1), 1)
  expect_equal(res1$name, b1$name)
  expect_s3_class(res1$time, class(Sys.time()), exact = TRUE)

  ## Need to sleep here for a bit so that windows definitely puts the
  ## second bundle second!
  Sys.sleep(1)

  b2 <- suppressMessages(
    hipercow_bundle_create(ids2, validate = FALSE, root = path))
  res2 <- hipercow_bundle_list(path)
  expect_equal(nrow(res2), 2)
  expect_equal(res2$name, c(b2$name, b1$name))
  expect_s3_class(res2$time, class(Sys.time()), exact = TRUE)
})


test_that("can delete bundles", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b1 <- suppressMessages(
    hipercow_bundle_create(ids::random_id(1), validate = FALSE, root = path))
  b2 <- suppressMessages(
    hipercow_bundle_create(ids::random_id(2), validate = FALSE, root = path))
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
    "Invalid task identifier",
    fixed = TRUE)
})


test_that("can validate that tasks really exist", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, task_create_expr(runif(1)))
  expect_equal(
    suppressMessages(hipercow_bundle_create(id1, name = "b", root = path)),
    new_bundle("b", id1))
  id2 <- ids::random_id()
  expect_error(
    hipercow_bundle_create(id2, name = "b", root = path),
    "Can't include tasks in bundle that don't exist, validate is 'TRUE'")
  expect_equal(
    suppressMessages(
      hipercow_bundle_create(id2, validate = FALSE, name = "b", root = path)),
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


test_that("can get bundle results", {
  path <- withr::local_tempdir()
  init_quietly(path)
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  x <- 1
  b <- withr::with_dir(
    path,
    suppressMessages(
      task_create_bulk_expr(list(x, a, b), d, bundle_name = "b")))
  err <- expect_error(
    hipercow_bundle_result(b, root = path),
    "Can't fetch results for bundle 'b' due to error")
  expect_match(conditionMessage(err$parent),
               "'created'")
  env <- new.env()
  for (i in b$ids) {
    task_eval(i, root = path, envir = env)
  }
  expect_equal(hipercow_bundle_result(b, root = path),
               list(list(1, 1, "x"),
                    list(1, 2, "y"),
                    list(1, 3, "z")))
})


test_that("can get bundle status", {
  path <- withr::local_tempdir()
  init_quietly(path)
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  x <- 1
  b <- withr::with_dir(
    path,
    suppressMessages(
      task_create_bulk_expr(list(x, a, b), d, bundle_name = "b")))
  expect_equal(hipercow_bundle_status(b, root = path),
               rep("created", 3))
  expect_equal(hipercow_bundle_status(b, reduce = TRUE, root = path),
               "created")
  env <- new.env()
  task_eval(b$ids[[2]], root = path, envir = env)
  expect_equal(hipercow_bundle_status(b, root = path),
               c("created", "success", "created"))
  expect_equal(hipercow_bundle_status(b, reduce = TRUE, root = path),
               "created")
  task_eval(b$ids[[1]], root = path, envir = env)
  task_eval(b$ids[[3]], root = path, envir = env)
  expect_equal(hipercow_bundle_status(b, root = path),
               c("success", "success", "success"))
  expect_equal(hipercow_bundle_status(b, reduce = TRUE, root = path),
               "success")
})


test_that("can reduce statuses", {
  expect_equal(
    status_reduce(c("created", "submitted", "running"), "status"),
    "created")
  expect_equal(
    status_reduce(c("submitted", "submitted", "running"), "status"),
    "running")
  expect_equal(
    status_reduce(c("running", "running", "running"), "status"),
    "running")
  expect_equal(
    status_reduce(c("running", "success", "success"), "status"),
    "running")
  expect_equal(
    status_reduce(c("success", "success", "success"), "status"),
    "success")
  expect_equal(
    status_reduce(c("success", "failure", "success"), "status"),
    "failure")
  expect_equal(
    status_reduce(c("cancelled", "failure", "success"), "status"),
    "failure")
  expect_equal(
    status_reduce(c("cancelled", "cancelled", "success"), "status"),
    "cancelled")
})


test_that("can reduce status for wait with early and late failure", {
  expect_equal(
    status_reduce(c("created", "submitted", "running", "success"),
                  "wait-fail-early"),
    "created")
  expect_equal(
    status_reduce(c("running", "submitted", "running", "success"),
                  "wait-fail-early"),
    "running")
  expect_equal(
    status_reduce(c("running", "submitted", "running", "failure"),
                  "wait-fail-early"),
    "failure")
  expect_equal(
    status_reduce(c("running", "submitted", "running", "failure"),
                  "wait-fail-late"),
    "running")
  expect_equal(
    status_reduce(c("success", "submitted", "success", "failure"),
                  "wait-fail-late"),
    "submitted")
})


test_that("cancelling a bundle just forwards ids to task_cancel", {
  path <- withr::local_tempdir()
  init_quietly(path)
  ids <- ids::random_id(3)
  bundle <- suppressMessages(
    hipercow_bundle_create(ids, validate = FALSE, root = path))
  mock_cancel <- mockery::mock()
  mockery::stub(hipercow_bundle_cancel, "task_cancel", mock_cancel)
  hipercow_bundle_cancel(bundle, root = path)
  mockery::expect_called(mock_cancel, 1)
  expect_equal(mockery::mock_args(mock_cancel)[[1]],
               list(ids, follow = TRUE, root = hipercow_root(path)))
})


test_that("can cancel submitted tasks", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  x <- 1
  d <- data.frame(a = 1:3, b = c("x", "y", "z"))
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(list(x, a, b), d, bundle_name = "b")))
  expect_equal(hipercow_bundle_status(b, reduce = TRUE, root = path_here),
               "submitted")
  expect_message(
    res <- hipercow_bundle_cancel(b, root = path_here),
    "Successfully cancelled 3 tasks")
  expect_equal(res, rep(TRUE, 3))
  expect_equal(hipercow_bundle_status(b, root = path_here),
               rep("cancelled", 3))
  expect_message(
    res <- hipercow_bundle_cancel(b, root = path_here),
    "Did not try to cancel any of 3 tasks")
  expect_equal(res, rep(FALSE, 3))
})


test_that("can read bundle logs", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(x = 1:3)
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))
  path_log <- path_to_task_file(path_there, b$ids, "elsewhere_log")

  expect_equal(hipercow_bundle_log_value(b, root = path_here),
               vector("list", 3))

  file.create(path_log[[2]])
  expect_equal(
    hipercow_bundle_log_value(b, root = path_here),
    list(NULL, character(), NULL))

  writeLines(c("a", "b"), path_log[[3]])
  expect_equal(
    hipercow_bundle_log_value(b, root = path_here),
    list(NULL, character(), c("a", "b")))
})


test_that("can retry tasks in a bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  d <- data.frame(x = 1:3)
  b <- withr::with_dir(
    path,
    suppressMessages(
      task_create_bulk_expr(runif(x), d, bundle_name = "b")))
  env <- new.env()
  for (i in b$ids) {
    task_eval(i, root = path, envir = env)
  }

  expect_equal(hipercow_bundle_status(b, root = path),
               rep("success", 3))
  res <- evaluate_promise(hipercow_bundle_retry(b, root = path))
  expect_equal(res$result, rep(TRUE, 3))
  expect_match(res$messages, "Retrying 3 / 3 tasks")
  expect_equal(hipercow_bundle_status(b, root = path), rep("created", 3))
})


test_that("can retry some tasks in a bundle", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(p = c("a", "b", "c"))
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(readLines(p), d, bundle_name = "b")))
  writeLines("a", file.path(path_there, "a"))
  writeLines("b", file.path(path_there, "b"))

  env <- new.env()
  for (i in b$ids) {
    task_eval(i, root = path_there, envir = env)
  }

  expect_equal(hipercow_bundle_status(b, root = path_here),
               c("success", "success", "failure"))

  res <- evaluate_promise(hipercow_bundle_retry(b, if_status_in = "failure",
                                                root = path_here))
  expect_equal(res$result, c(FALSE, FALSE, TRUE))
  expect_match(res$messages[[1]], "Retrying 1 / 3 tasks")
  expect_match(res$messages[[2]], "Submitted task")
  expect_equal(hipercow_bundle_status(b, root = path_here),
               c("success", "success", "submitted"))

  expect_error(
    hipercow_bundle_retry(b, if_status_in = "failure", root = path_here),
    "No tasks eligible for retry: 1 submitted and 2 success")
})


test_that("validate that status are reasonable", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- new_bundle("b", ids::random_id(3))
  expect_error(
    hipercow_bundle_retry(b, if_status_in = "foo", root = path),
    "Invalid value for 'if_status_in': 'foo'")
  expect_error(
    hipercow_bundle_retry(b, if_status_in = c("success", "foo", "created"),
                          root = path),
    "Invalid values for 'if_status_in': 'foo' and 'created'")
})


test_that("can wait on a bundle", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(x = 1:2)
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))

  mock_status <- mockery::mock(
    c("submitted", "submitted"),
    c("running", "submitted"),
    c("running", "running"),
    c("success", "running"),
    c("success", "success"))
  mockery::stub(hipercow_bundle_wait, "task_status", mock_status)

  expect_true(hipercow_bundle_wait(b, poll = 0, root = path_here))
  mockery::expect_called(mock_status, 5)
  root <- hipercow_root(path_here)
  expect_equal(
    mockery::mock_args(mock_status),
    rep(list(list(b$ids, follow = FALSE, root = root)), 5))
})


test_that("can't wait un unsubmitted tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  d <- data.frame(x = 1:3)
  b <- withr::with_dir(
    path,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))
  expect_error(
    hipercow_bundle_wait(b, root = path),
    "Cannot wait on bundle 'b', which has unsubmitted tasks")
})


test_that("early exit on wait by default", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(x = 1:2)
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))
  mock_status <- mockery::mock(
    c("submitted", "submitted"),
    c("running", "submitted"),
    c("running", "failure"),
    c("success", "failure"))
  mockery::stub(hipercow_bundle_wait, "task_status", mock_status)
  expect_false(hipercow_bundle_wait(b, poll = 0, root = path_here))
  mockery::expect_called(mock_status, 3)
})


test_that("can defer early exit if requested", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(x = 1:2)
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))
  mock_status <- mockery::mock(
    c("submitted", "submitted"),
    c("running", "submitted"),
    c("running", "failure"),
    c("success", "failure"))
  mockery::stub(hipercow_bundle_wait, "task_status", mock_status)
  expect_false(
    hipercow_bundle_wait(b, poll = 0, fail_early = FALSE, root = path_here))
  mockery::expect_called(mock_status, 4)
})


test_that("can defer early exit if requested", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  d <- data.frame(x = 1:2)
  b <- withr::with_dir(
    path_here,
    suppressMessages(
      task_create_bulk_expr(sqrt(x), d, bundle_name = "b")))
  expect_error(
    hipercow_bundle_wait(b, timeout = 0, root = path_here),
    "Bundle 'b' did not complete in time")
})


test_that("correctly destructure list column", {
  path <- withr::local_tempdir()
  init_quietly(path)

  my_named_list <- list(
    list(val = 2, other = 3),
    list(val = 3, other = 4),
    list(val = 4, other = 5),
    list(val = 5, other = 6),
    list(val = 6, other = 7)
  )
  d <- data.frame(a = 1:5, b = runif(5), c = I(my_named_list))

  f <- function(a, b, c) {
    a + b + c$val + c$other
  }
  bundle <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_expr(f(a, b, c), d)))

  env <- new.env()
  for (i in bundle$ids) {
    task_eval(i, root = path, envir = env)
  }

  expect_equal(hipercow_bundle_result(bundle, root = path),
               as.list(d$a + d$b + 2:6 + 3:7))
})
