test_that("can select tasks to purge by id", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  ids <- ids::random_id(5)

  root <- hipercow_root(path)
  expect_equal(purge_select_by_id(b$ids, root, NULL), b$ids)
  expect_equal(purge_select_by_id(b$ids[1:2], root, NULL), b$ids[1:2])
  expect_equal(purge_select_by_id(character(), root, NULL), character())
  expect_equal(purge_select_by_id(NULL, root, NULL), NULL)
  expect_error(purge_select_by_id(ids, root, NULL),
               "5 / 5 tasks do not exist")
  expect_error(purge_select_by_id(c(b$ids, ids), root, NULL),
               "5 / 10 tasks do not exist")
  expect_error(purge_select_by_id(c(b$ids, ids[1]), root, NULL),
               "1 / 6 tasks do not exist")
})


test_that("can select tasks to purge by bundle", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b1 <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5, bundle_name = "a_1")))
  b2 <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:2, bundle_name = "a_2")))
  b3 <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:3, bundle_name = "b_1")))

  root <- hipercow_root(path)
  expect_equal(purge_select_by_bundle(NULL, "a_1", root, NULL), b1$ids)
  expect_setequal(purge_select_by_bundle(NULL, c("a_1", "a_2"), root, NULL),
                  c(b1$ids, b2$ids))
  expect_setequal(purge_select_by_bundle(NULL, c("a*"), root, NULL),
                  c(b1$ids, b2$ids))
  expect_setequal(purge_select_by_bundle(NULL, c("*_1"), root, NULL),
                  c(b1$ids, b3$ids))
  expect_setequal(purge_select_by_bundle(NULL, c("a*", "b_1"), root, NULL),
                  c(b1$ids, b2$ids, b3$ids))

  ids1 <- c(b1$ids[2:3], b2$ids[2], b3$ids[2])
  ids2 <- ids::random_id(4)

  expect_equal(purge_select_by_bundle(ids1, "a_1", root, NULL), ids1[1:2])
  expect_setequal(purge_select_by_bundle(ids1, c("a_1", "a_2"), root, NULL),
                  ids1[1:3])
  expect_setequal(purge_select_by_bundle(ids1, c("a*"), root, NULL),
                  ids1[1:3])
  expect_setequal(purge_select_by_bundle(ids1, c("*_1"), root, NULL),
                  ids1[c(1, 2, 4)])
  expect_setequal(purge_select_by_bundle(ids1, c("a*", "b_1"), root, NULL),
                  ids1)
  expect_setequal(purge_select_by_bundle(ids1, "*", root, NULL),
                  ids1)

  expect_equal(purge_select_by_bundle(ids2, "*", root, NULL),
               character())
  expect_equal(purge_select_by_bundle(character(), "*", root, NULL),
               character())

  expect_equal(purge_select_by_bundle(NULL, "xzy", root, NULL),
               character())
})


test_that("can select by status", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b1 <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  b2 <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(stop, c("a", "b", "c"))))
  for (id in b1$ids) {
    task_eval(id, root = path)
  }

  root <- hipercow_root(path)
  expect_setequal(
    purge_select_by_status(NULL, c("created", "success"), root),
    c(b1$ids, b2$ids))
  expect_setequal(
    purge_select_by_status(NULL, c("success", "failure"), root),
    b1$ids)
  expect_setequal(
    purge_select_by_status(NULL, "failure", root),
    character())
  expect_error(
    purge_select_by_status(NULL, "submitted", root),
    "Cannot purge tasks with status: 'submitted'")
  expect_error(
    purge_select_by_status(NULL, c("submitted", "running"), root),
    "Cannot purge tasks with statuses: 'submitted' and 'running'")

  for (id in b2$ids) {
    task_eval(id, root = path)
  }

  root <- hipercow_root(path)
  expect_setequal(
    purge_select_by_status(NULL, "failure", root),
    b2$ids)
  expect_setequal(
    purge_select_by_status(b1$ids, c("failure", "success"), root),
    b1$ids)
  expect_setequal(
    purge_select_by_status(character(), c("failure", "success"), root),
    character())

  expect_equal(purge_select_by_status(b1$ids, NULL, root), b1$ids)
})


test_that("can parse finished_before", {
  now <- structure(1708070841.93384, class = c("POSIXct", "POSIXt"))
  expect_identical(finished_before_to_time(now), now)
  expect_identical(finished_before_to_time(as.Date(now)), as.Date(now))

  expect_equal(
    finished_before_to_time(as.difftime(1, units = "hours"), now = now),
    now - 60^2)
  expect_equal(
    finished_before_to_time(as.difftime(-1, units = "hours"), now = now),
    now - 60^2)
  expect_equal(
    finished_before_to_time(as.difftime(3, units = "days"), now = now),
    now - 3 * 24 * 60^2)

  expect_equal(
    finished_before_to_time("1h", now = now),
    now - 60^2)
  expect_equal(
    finished_before_to_time("2 day", now = now),
    now - 60^2 * 24 * 2)
  expect_equal(
    finished_before_to_time("3   weeks", now = now),
    now - 60^2 * 24 * 7 * 3)

  expect_error(
    finished_before_to_time("1 month", now = now),
    "Invalid string input for 'finished_before': 1 month")
  expect_error(
    finished_before_to_time(1, now = now),
    "Unsupported input for 'finished_before'")
})


test_that("can select by time", {
  ## This one requires even more faffing about with the setup, as we
  ## need to adjust the times so that we can have a sensible spread of
  ## times here.
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  times <- Sys.time() - c(20,               # recent
                          60 * 60 * 2,      # 2h
                          60 * 60 * 12,     # 12h
                          60 * 60 * 24 * 2, # 2d
                          60 * 60 * 24 * 9) # 9d
  for (i in seq_along(b$ids)) {
    id <- b$ids[[i]]
    task_eval(id, root = path)
    p <- file.path(path_task(root$path$tasks, id), INFO)
    d <- readRDS(p)
    d$times[["finished"]] <- times[[i]]
    saveRDS(d, p)
  }

  expect_setequal(purge_select_by_time(NULL, "1d", root, NULL),
                  b$ids[4:5])
  expect_setequal(purge_select_by_time(NULL, times[2], root, NULL),
                  b$ids[3:5])
  expect_setequal(purge_select_by_time(NULL, "1 hour", root, NULL),
                  b$ids[2:5])

  expect_setequal(purge_select_by_time(b$ids[1:3], "1 hour", root, NULL),
                  b$ids[2:3])
  expect_setequal(purge_select_by_time(character(), "1 hour", root, NULL),
                  character())
})


test_that("can purge", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(hipercow_purge(in_bundle = "*", root = path))
  expect_equal(res$result, b$ids)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Purging 5 tasks")
  expect_match(res$messages[[2]], "Deleting 1 task bundle")
})


test_that("can report when no bundles were deleted", {
  path <- withr::local_tempdir()
  init_quietly(path)
  id <- withr::with_dir(path,
                        suppressMessages(task_create_call(sqrt, list(1))))
  res <- evaluate_promise(hipercow_purge(with_status = "created", root = path))
  expect_equal(res$result, id)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Purging 1 task")
  expect_match(res$messages[[2]], "No task bundles need deleting")
})


test_that("can tell the user when purge achieves nothing", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(
    hipercow_purge(with_status = "cancelled", root = path))
  expect_equal(res$result, character())
  expect_length(res$messages, 1)
  expect_match(res$messages[[1]], "Nothing to purge")
})


test_that("disallow purging if running or submitted tasks selected", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  expect_error(
    hipercow_purge(in_bundle = b$name, root = path),
    "Can't purge; some tasks are submitted or running")
})


test_that("can purge with retries", {
  path <- withr::local_tempdir()
  init_quietly(path)
  root <- hipercow_root(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  for (id in b$ids) {
    task_eval(id, root = path)
  }
  suppressMessages(hipercow_bundle_retry(b, root = path))
  ids <- setdiff(unlist(lapply(b$ids, retry_chain, root)), b$ids)

  res <- evaluate_promise(
    hipercow_purge(task_ids = b$ids, root = path))
  expect_setequal(res$result, c(b$ids, ids))
  expect_length(res$messages, 3)
  expect_match(res$messages[[1]], "Also purging 5 tasks from retry chains")
})


test_that("disallow purging if running or submitted retried tasks selected", {
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  root <- hipercow_root(path)
  b <- withr::with_dir(
    path,
    suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  for (id in b$ids) {
    task_eval(id, root = path)
  }
  suppressMessages(hipercow_bundle_retry(b, root = path))

  expect_error(
    hipercow_purge(task_ids = b$ids, root = path),
    "Can't purge; some tasks have submitted or running retries")
})


test_that("must use at least one filter type", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_error(
    hipercow_purge(root = path),
    "No filter selected")
})

test_that("can do a dry run purge", {
  path <- withr::local_tempdir()
  init_quietly(path)
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  res <- evaluate_promise(hipercow_purge(in_bundle = "*", root = path,
                                         dry_run = TRUE))
  expect_equal(res$result, b$ids)
  expect_length(res$messages, 4)
  expect_match(res$messages[[1]], "Purging 5 tasks")
  expect_match(res$messages[[2]], "Dry_run")
  expect_match(res$messages[[3]], "Deleting 1 task bundle")
  expect_match(res$messages[[4]], "Dry_run")
  
  file1 <-
  expect_true(file.exists(substring(res$message[[4]], 32)))
  
  
})
