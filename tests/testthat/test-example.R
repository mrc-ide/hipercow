test_that("can run simple example", {
  owd <- getwd()
  expect_message(
    cleanup <- hipercow_example_helper(),
    "This example uses a special helper")
  cwd <- getwd()
  expect_false(cwd == owd)
  expect_equal(dir(), "hipercow")
  expect_true(is.function(cleanup))

  id <- suppressMessages(task_create_expr(sqrt(2)))
  expect_true(task_wait(id, timeout = 5))
  expect_equal(task_result(id), sqrt(2))

  expect_message(
    cleanup(),
    "Cleaning up example")
  expect_equal(getwd(), owd)
  expect_false(file.exists(cwd))
})


test_that("can configure an example driver", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "hipercow"))
  withr::with_dir(path, expect_equal(example_configure(), list()))
  expect_true(file.exists(file.path(path, "hipercow/example.queue")))
  expect_equal(readLines(file.path(path, "hipercow/example.queue")),
               character())
})


test_that("submitting a task updates the queue", {
  path <- withr::local_tempdir()
  dir.create(file.path(path, "hipercow"))
  path_q <- file.path(path, "hipercow/example.queue")
  file.create(path_q)
  id <- ids::random_id(2)
  example_submit(id[[1]], NULL, list(), path)
  expect_equal(readLines(path_q), id[[1]])
  example_submit(id[[2]], NULL, list(), path)
  expect_equal(readLines(path_q), id[1:2])
})


test_that("can get task status", {
  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  id <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(1))))
  expect_equal(task_status(id, root = path), "submitted")
  expect_equal(example_status(id, list(), path), "submitted")
  file.create(file.path(path, "hipercow", "tasks", id, "status-running"))
  expect_equal(task_status(id, root = path), "running")
  expect_equal(example_status(id, list(), path), "running")
})


test_that("can fetch started time", {
  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  id <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(1))))
  p_running <- file.path(path, "hipercow", "tasks", id, "status-running")
  expect_equal(example_info(id, list(), path),
               list(status = "submitted",
                    time_started = file.info(p_running)$ctime))
  file.create(p_running)
  expect_equal(example_info(id, list(), path),
               list(status = "running",
                    time_started = file.info(p_running)$ctime))
})


test_that("can fetch log", {
  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  id <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(1))))

  expect_null(example_log(id, FALSE, list(), path))
  expect_null(example_log(id, TRUE, list(), path))

  writeLines(letters, file.path(path, "hipercow", "tasks", id, "log"))
  writeLines(LETTERS, file.path(path, "hipercow", "tasks", id, "log.outer"))
  expect_equal(example_log(id, FALSE, list(), path), letters)
  expect_equal(example_log(id, TRUE, list(), path), LETTERS)
})


test_that("can fetch log, really", {
  cleanup <- suppressMessages(hipercow_example_helper(with_logging = TRUE))
  withr::defer(suppressMessages(cleanup()))
  id <- suppressMessages(task_create_expr(message("hello")))
  task_wait(id)
  expect_match(task_log_value(id), "hello", all = FALSE)
  expect_match(task_log_value(id, outer = TRUE), "Running task", all = FALSE)
})


test_that("can cancel tasks", {
  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  id1 <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(1))))
  id2 <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(2))))
  id3 <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(3))))
  res <- example_cancel(id2, list(), path)
  expect_true(res$cancelled)
  expect_true(is.na(res$time_started))
})


test_that("example cluster info is meagre", {
  info <- example_cluster_info()
  expect_equal(info$max_ram, 1)
  expect_equal(info$max_cores, 1)
})


test_that("can run provision script", {
  mock_conan_run <- mockery::mock()
  mockery::stub(example_provision_run, "conan2::conan_run", mock_conan_run)

  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  writeLines("R6", file.path(path, "pkgdepends.txt"))

  suppressMessages(example_provision_run(list(method = NULL), list(), path))
  cmp <- suppressMessages(conan2::conan_configure(
    method = NULL,
    path = path,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = .libPaths()[[1]]))
  mockery::expect_called(mock_conan_run, 1)
  expect_equal(mockery::mock_args(mock_conan_run)[[1]],
               list(cmp, show_log = TRUE))
})


test_that("can list installations", {
  mock_conan_list <- mockery::mock()
  mockery::stub(example_provision_list, "conan2::conan_list", mock_conan_list)

  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  writeLines("R6", file.path(path, "pkgdepends.txt"))

  path_lib <- file.path(path, "hipercow", "lib")

  suppressMessages(example_provision_list(NULL, list(), path))
  mockery::expect_called(mock_conan_list, 1)
  expect_equal(mockery::mock_args(mock_conan_list)[[1]],
               list(path_lib, NULL))

  cmp <- suppressMessages(conan2::conan_configure(
    method = NULL,
    path = path,
    path_lib = file.path("hipercow", "lib"),
    path_bootstrap = .libPaths()[[1]]))

  suppressMessages(example_provision_list(list(method = NULL), list(), path))
  mockery::expect_called(mock_conan_list, 2)
  expect_equal(mockery::mock_args(mock_conan_list)[[2]],
               list(path_lib, cmp$hash))
})


test_that("camn can provision_compare using conan_compare", {
  mock_conan_compare <- mockery::mock()
  mockery::stub(example_provision_compare, "conan2::conan_compare",
                mock_conan_compare)
  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))

  path_lib <- file.path(path, "hipercow", "lib")

  example_provision_compare(0, -1, config, path)
  mockery::expect_called(mock_conan_compare, 1)
  expect_equal(mockery::mock_args(mock_conan_compare)[[1]],
               list(path_lib, 0, -1))
})


test_that("can read example keypair", {
  path <- withr::local_tempdir()
  res <- example_keypair(list(), path)
  expect_true(file.exists(file.path(path, "hipercow", "example", "key")))
  expect_equal(openssl::write_ssh(as.list(openssl::read_key(res$key))$pubkey),
               res$pub)
  expect_equal(example_keypair(list(), path), res)
})


test_that("set cache dir for example", {
  withr::local_envvar(R_USER_CACHE_DIR = NA)
  cleanup <- suppressMessages(hipercow_example_helper(runner = FALSE))
  expect_false(is.na(Sys.getenv("R_USER_CACHE_DIR", NA)))
  suppressMessages(cleanup())
  expect_true(is.na(Sys.getenv("R_USER_CACHE_DIR", NA)))
})


test_that("can run example task", {
  mock_sleep <- mockery::mock()
  mockery::stub(example_step, "Sys.sleep", mock_sleep)

  path <- withr::local_tempdir()
  init_quietly(path)
  suppressMessages(hipercow_configure("example", root = path))
  id1 <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(1))))
  id2 <- suppressMessages(withr::with_dir(path, task_create_expr(sqrt(2))))

  example_step(path, FALSE, 0.1)
  expect_equal(task_status(id1, root = path), "success")
  expect_null(task_log_value(id1, root = path))
  expect_equal(task_status(id2, root = path), "submitted")
  mockery::expect_called(mock_sleep, 0)

  example_step(path, TRUE, 0.1)
  expect_equal(task_status(id2, root = path), "success")
  expect_type(task_log_value(id2, root = path), "character")
  mockery::expect_called(mock_sleep, 0)

  example_step(path, FALSE, 0.1)
  mockery::expect_called(mock_sleep, 1)
  expect_equal(mockery::mock_args(mock_sleep)[[1]], list(0.1))
})
