test_that("can list all tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  expect_equal(task_list(root = path), character())
  b <- withr::with_dir(path,
                       suppressMessages(task_create_bulk_call(sqrt, 1:5)))
  expect_setequal(task_list(root = path), b$ids)
})


test_that("can list tasks by status", {
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

  expect_equal(task_list(in_bundle = "a_1", root = path), b1$ids)
  expect_setequal(task_list(in_bundle = c("a_1", "a_2"), root = path),
                  c(b1$ids, b2$ids))
  expect_setequal(task_list(in_bundle = c("a*"), root = path),
                  c(b1$ids, b2$ids))
  expect_setequal(task_list(in_bundle = c("*_1"), root = path),
                  c(b1$ids, b3$ids))
  expect_setequal(task_list(in_bundle = c("a*", "b_1"), root = path),
                  c(b1$ids, b2$ids, b3$ids))
})


test_that("can select tasks by bundle", {
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
  expect_equal(task_select_by_bundle(NULL, "a_1", root, NULL), b1$ids)
  expect_setequal(task_select_by_bundle(NULL, c("a_1", "a_2"), root, NULL),
                  c(b1$ids, b2$ids))
  expect_setequal(task_select_by_bundle(NULL, c("a*"), root, NULL),
                  c(b1$ids, b2$ids))
  expect_setequal(task_select_by_bundle(NULL, c("*_1"), root, NULL),
                  c(b1$ids, b3$ids))
  expect_setequal(task_select_by_bundle(NULL, c("a*", "b_1"), root, NULL),
                  c(b1$ids, b2$ids, b3$ids))

  ids1 <- c(b1$ids[2:3], b2$ids[2], b3$ids[2])
  ids2 <- ids::random_id(4)

  expect_equal(task_select_by_bundle(ids1, "a_1", root, NULL), ids1[1:2])
  expect_setequal(task_select_by_bundle(ids1, c("a_1", "a_2"), root, NULL),
                  ids1[1:3])
  expect_setequal(task_select_by_bundle(ids1, c("a*"), root, NULL),
                  ids1[1:3])
  expect_setequal(task_select_by_bundle(ids1, c("*_1"), root, NULL),
                  ids1[c(1, 2, 4)])
  expect_setequal(task_select_by_bundle(ids1, c("a*", "b_1"), root, NULL),
                  ids1)
  expect_setequal(task_select_by_bundle(ids1, "*", root, NULL),
                  ids1)

  expect_equal(task_select_by_bundle(ids2, "*", root, NULL),
               character())
  expect_equal(task_select_by_bundle(character(), "*", root, NULL),
               character())

  expect_equal(task_select_by_bundle(NULL, "xzy", root, NULL),
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
    task_select_by_status(NULL, c("created", "success"), root),
    c(b1$ids, b2$ids))
  expect_setequal(
    task_select_by_status(NULL, c("success", "failure"), root),
    b1$ids)
  expect_setequal(
    task_select_by_status(NULL, "failure", root),
    character())
  for (id in b2$ids) {
    task_eval(id, root = path)
  }

  root <- hipercow_root(path)
  expect_setequal(
    task_select_by_status(NULL, "failure", root),
    b2$ids)
  expect_setequal(
    task_select_by_status(b1$ids, c("failure", "success"), root),
    b1$ids)
  expect_setequal(
    task_select_by_status(character(), c("failure", "success"), root),
    character())

  expect_equal(task_select_by_status(b1$ids, NULL, root), b1$ids)
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

  expect_setequal(task_select_by_time(NULL, "1d", root, NULL),
                  b$ids[4:5])
  expect_setequal(task_select_by_time(NULL, times[2], root, NULL),
                  b$ids[3:5])
  expect_setequal(task_select_by_time(NULL, "1 hour", root, NULL),
                  b$ids[2:5])

  expect_setequal(task_select_by_time(b$ids[1:3], "1 hour", root, NULL),
                  b$ids[2:3])
  expect_setequal(task_select_by_time(character(), "1 hour", root, NULL),
                  character())
})
