test_that("Can retry a simple task", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, task_create_explicit(quote(runif(1))))
  expect_true(task_eval(id1, root = path))
  r1 <- task_result(id1, root = path)

  id2 <- task_retry(id1, root = path)

  root <- hipercow_root(path)

  expect_equal(task_status(id1, root = path), "created")
  expect_equal(task_status(id1, follow = FALSE, root = path), "success")

  expect_true(task_eval(id2, root = path))
  r2 <- task_result(id2, root = path)

  expect_equal(task_status(id1, root = path), "success")
  expect_equal(task_status(id2, root = path), "success")
  expect_false(r2 == r1)
  expect_equal(task_result(id1, root = path), r2)
  expect_equal(task_result(id1, follow = FALSE, root = path), r1)
})


test_that("can't retry nonterminal tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path, task_create_explicit(quote(runif(1))))
  err <- expect_error(
    task_retry(id, root = path),
    "1 task does not have terminal status")
  expect_match(err$body, c("x" = sprintf("%s: created", id)))
})


test_that("can't retry nonterminal tasks in complex case", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(
    path,
    task_create_bulk_expr(sqrt(a), data.frame(a = 1:20)))
  for (i in id[c(2, 4, 6, 8)]) {
    task_eval(i, root = path)
  }
  err <- expect_error(
    task_retry(id, root = path),
    "16 tasks do not have terminal status")
  expect_length(err$body, 6)
  expect_equal(err$body[1:5],
               set_names(sprintf("%s: created", id[c(1, 3, 5, 7, 9)]), "x"))
  expect_equal(err$body[6],
               c(i = "...and 11 other tasks"))

  err <- expect_error(
    task_retry(id[7:13], root = path),
    "6 tasks do not have terminal status")
  expect_length(err$body, 6)
  expect_equal(err$body[1:5],
               set_names(sprintf("%s: created", id[c(7, 9:12)]), "x"))
  expect_equal(err$body[6],
               c(i = "...and 1 other task"))
})


test_that("can retry a retried task", {
  set.seed(1)
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)

  id1 <- withr::with_dir(path, task_create_explicit(quote(runif(1))))
  expect_true(task_eval(id1, root = path))
  r1 <- task_result(id1, root = path)
  expect_equal(nrow(root$retry_map), 0)
  expect_null(retry_chain(id1, root = root))

  ## First level retry:
  id2 <- task_retry(id1, root = path)
  expect_true(task_eval(id2, root = path))
  r2 <- task_result(id2, root = path)
  expect_equal(root$retry_map,
               data.frame(id = id2, parent = id1, base = id1))
  expect_equal(retry_chain(id2, root = root), c(id1, id2))

  ## Second level retry, from the leaf
  id3 <- task_retry(id2, root = path)
  expect_true(task_eval(id3, root = path))
  r3 <- task_result(id3, root = path)
  expect_equal(root$retry_map,
               data.frame(id = c(id2, id3),
                          parent = c(id1, id2),
                          base = id1))
  expect_equal(retry_chain(id2, root = root), c(id1, id2, id3))

  ## Third level retry, from the root
  id4 <- task_retry(id1, root = path)
  expect_true(task_eval(id4, root = path))
  r4 <- task_result(id4, root = path)
  expect_equal(root$retry_map,
               data.frame(id = c(id2, id3, id4),
                          parent = c(id1, id2, id3),
                          base = id1))

  expect_equal(task_result(id1, root = path), r4)
  expect_equal(task_result(id2, root = path), r4)
  expect_equal(task_result(id3, root = path), r4)
  expect_equal(task_result(id4, root = path), r4)

  expect_equal(retry_chain(id1, root = root), c(id1, id2, id3, id4))
  expect_equal(retry_chain(id2, root = root), c(id1, id2, id3, id4))
  expect_equal(retry_chain(id3, root = root), c(id1, id2, id3, id4))
  expect_equal(retry_chain(id4, root = root), c(id1, id2, id3, id4))

  expect_equal(task_info(id1, root = path)$chain, c(id1, id2, id3, id4))
  expect_equal(task_info(id4, root = path)$chain, c(id1, id2, id3, id4))
})


test_that("verbosely running retried task prints about original location", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id1 <- withr::with_dir(path, task_create_expr(runif(1)))
  task_eval(id1, root = path)
  id2 <- task_retry(id1, root = path)
  res <- evaluate_promise(
    task_eval(id2, verbose = TRUE, root = path))
  expect_match(res$messages, sprintf("pointing at %s (expression)", id1),
               fixed = TRUE, all = FALSE)
})


test_that("can choose to follow logs or not", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, action = "immediate",
                       root = path_here))

  suppressMessages(
    id1 <- withr::with_dir(path_here, task_create_expr(runif(1))))
  writeLines("a",
             file.path(path_there, "hipercow", "tasks", id1, "elsewhere_log"))
  expect_equal(task_log_value(id1, root = path_here), "a")

  expect_message(
    id2 <- task_retry(id1, root = path_here),
    "Submitted task")
  writeLines("b",
             file.path(path_there, "hipercow", "tasks", id2, "elsewhere_log"))

  expect_equal(task_log_value(id1, root = path_here), "b")
  expect_equal(task_log_value(id2, root = path_here), "b")
  expect_equal(task_log_value(id1, follow = FALSE, root = path_here), "a")
  expect_equal(task_log_value(id2, follow = FALSE, root = path_here), "b")
})
