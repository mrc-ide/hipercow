test_that("Validate resource args", {
  expect_equal(validate_cores(Inf), Inf)
  expect_equal(validate_cores(1), 1L)

  expect_error(validate_cores(-Inf),
               "Invalid value for 'cores': -Inf")
  expect_error(validate_cores(-1),
               "Invalid value for 'cores': -1")
  expect_error(validate_cores("potato"),
               "Invalid value for 'cores': potato")
  expect_error(validate_cores(NULL),
               "'cores' must be a scalar")
  expect_error(validate_cores(NA),
               "Invalid value for 'cores': NA")
  expect_error(validate_cores(mtcars),
               "'cores' must be a scalar")
  expect_error(validate_cores(c(1, 2, 3)),
               "'cores' must be a scalar")
})


test_that("validate max_runtime", {
  expect_null(validate_max_runtime(NULL))
  expect_equal(validate_max_runtime(35), 35)
  expect_equal(validate_max_runtime("2h35m"), 155)

  expect_error(
    validate_max_runtime("0"),
    "Invalid value for 'max_runtime': 0")
  expect_error(
    validate_max_runtime("Kazahstan"),
    "Invalid value for 'max_runtime': Kazahstan")
  expect_error(
    validate_max_runtime(NA),
    "Invalid value for 'max_runtime': NA")
  expect_error(
    validate_max_runtime(c(1, 2, 3)),
    "'max_runtime' must be a scalar")
})


test_that("validate hold_until", {
  expect_null(validate_hold_until(NULL))
  expect_equal(validate_hold_until("tonight"), "tonight")
  expect_equal(validate_hold_until("midnight"), "midnight")

  expect_error(
    validate_hold_until(0),
    "Invalid value for 'hold_until': 0")
  expect_equal(validate_hold_until(60), 60)
  expect_equal(validate_hold_until("2h30m"), 150)
  expect_equal(validate_hold_until("1d1h1m"), 1501)
  tomorrow <- Sys.Date() + 1
  expect_equal(validate_hold_until(tomorrow), as.POSIXlt(tomorrow))
  today <- Sys.Date()
  expect_error(validate_hold_until(today),
               "Invalid value for 'hold_until'")

  soon <- Sys.time() + 120
  expect_equal(validate_hold_until(soon), as.POSIXlt(soon))
  err <- expect_error(validate_hold_until(Sys.time() - 1),
                      "Invalid value for 'hold_until'")
  expect_match(err$body[[1]], "is in the past")
})


test_that("validate memory", {
  expect_null(validate_memory(NULL, "mem"))
  expect_error(
    validate_memory(c("a", "b"), "mem"),
    "'mem' must be a scalar")
  expect_error(
    validate_memory("10M", "mem"),
    "Invalid string representation of memory for 'mem': 10M")
  expect_equal(validate_memory(1, "mem"), 1)
  expect_equal(validate_memory("1", "mem"), 1)
  expect_equal(validate_memory("9G", "mem"), 9)
  expect_equal(validate_memory("2T", "mem"), 2000)
  expect_error(validate_memory("1G2T", "mem"),
               "Invalid string representation of memory for 'mem': 1G2T")
  expect_error(validate_memory("1GG", "mem"),
               "Invalid string representation of memory for 'mem': 1GG")

  err <- expect_error(validate_memory(-1, "mem"),
                      "Invalid value for 'mem': -1")
  expect_equal(err$body,
               c(i = "Amount of memory must be a positive integer"))

  err <- expect_error(validate_memory(TRUE, "mem"),
                      "Invalid value for 'mem': TRUE")
  expect_equal(
    err$body,
    c(i = "Expected an integer or a string representing a size"))


  err <- expect_error(validate_memory(0, "mem"),
                      "Invalid value for 'mem': 0")
  expect_equal(
    err$body,
    c(i = "We need some memory to run your tasks!"))
})


test_that("validate nodes", {
  expect_null(validate_nodes(NULL))
  expect_equal(validate_nodes(c("A", "B")), c("A", "B"))
  expect_equal(validate_nodes(c("A", "A")), "A")
  expect_equal(validate_nodes("A"), "A")
  expect_error(validate_nodes(NA),
               "'nodes' must be a character")
})


test_that("validate priority", {
  expect_null(validate_priority(NULL))
  expect_equal(validate_priority("low"), "low")
  expect_equal(validate_priority("normal"), "normal")
  expect_error(validate_priority(3000),
               "'priority' must be a character")
})


test_that("prevent high priorities", {
  mock_browse_url <- mockery::mock()
  mockery::stub(validate_priority, "utils::browseURL", mock_browse_url)
  err <- expect_error(
    validate_priority("high"),
    "Could not understand priority 'high'")
  expect_equal(err$body, c(i = "Priority can only be 'low' or 'normal'"))
  mockery::expect_called(mock_browse_url, 1)
  expect_equal(mockery::mock_args(mock_browse_url)[[1]],
               list("https://www.youtube.com/watch?v=dQw4w9WgXcQ"))
})


test_that("validate queue", {
  expect_null(validate_queue(NULL), NULL)
  expect_equal(validate_queue("Q"), "Q")
  expect_error(validate_queue(NA),
               "'queue' must be a character")
  expect_error(validate_queue(c("a", "b")),
               "'queue' must be a scalar")
})


test_that("Can get a hipercow_resources", {
  res <- hipercow_resources()
  expect_s3_class(res, "hipercow_resources")
  expect_equal(res$cores, 1)
  expect_equal(res$exclusive, FALSE)
})


test_that("Can validate resources against driver", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)

  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  root <- hipercow_root(path_here)

  cluster_info <- hipercow_cluster_info(driver = "elsewhere", root = root)
  expect_equal(cluster_info$resources$max_ram, 16)
  expect_equal(cluster_info$resources$max_cores, 8)
  expect_true("kevin" %in% cluster_info$resources$nodes)
  expect_true("Tesco" %in% cluster_info$resources$queues)

  res <- hipercow_resources(cores = 1,
                            memory_per_node = 5,
                            memory_per_process = 5,
                            requested_nodes = "Kevin")

  res2 <- hipercow_resources_validate(res, driver = "elsewhere",
                                      root = root)

  expect_equal(res2$queue, "Aldi")
})


test_that("Validate cores against cluster", {
  expect_silent(validate_cluster_cores(Inf, 1))
  expect_silent(validate_cluster_cores(5, 8))
  expect_error(validate_cluster_cores(10, 8))
})


test_that("Validate memory against cluster", {
  expect_silent(validate_cluster_memory(NULL, 10))
  expect_silent(validate_cluster_memory(5, 8))
  expect_error(validate_cluster_memory(10, 8))
})


test_that("Validate queue against cluster", {
  expect_silent(validate_cluster_queue(NULL, c("Q1", "Q2")))
  expect_silent(validate_cluster_queue("Q1", c("Q1", "Q2")))
  expect_error(validate_cluster_queue("Q3", c("Q1", "Q2")))
})


test_that("Validate requested nodes against cluster", {
  nodes <- c("N1", "N2", "N3")
  expect_silent(validate_cluster_requested_nodes(NULL, nodes))
  expect_silent(validate_cluster_requested_nodes("N1", nodes))
  expect_silent(validate_cluster_requested_nodes(c("N1", "N2"), nodes))
  expect_error(validate_cluster_requested_nodes("N4", nodes))
})


test_that("don't revalidate things that are flagged as valid", {
  elsewhere_register()
  path <- withr::local_tempdir()
  init_quietly(path, driver = "example")
  root <- hipercow_root(path)

  r <- hipercow_resources()
  r$cores <- 999
  attr(r, "validated") <- "foo"
  expect_identical(resources_validate(r, NULL, "foo", root), r)
  expect_error(resources_validate(r, NULL, "example", root),
               "999 is too many cores for this cluster")
})


test_that("can print default resource control", {
  x <- hipercow_resources()
  res <- evaluate_promise(print(x))
  expect_match(res$messages, "hipercow resource control (hipercow_resources)",
               all = FALSE, fixed = TRUE)
  expect_match(res$messages, "cores: 1", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "exclusive: FALSE", all = FALSE, fixed = TRUE)
  expect_match(res$messages, "Unset: 'max_runtime',",
               all = FALSE, fixed = TRUE)
})
