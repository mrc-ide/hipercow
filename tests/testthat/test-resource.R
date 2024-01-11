resource_test <- function(f, v, comp = v) {
  expect_identical(f(v), list(original = v, computed = comp))
}

test_that("Validate resource args", {
  resource_test(validate_cores, Inf)
  resource_test(validate_cores, 1, 1L)
  expect_error(validate_cores(-Inf))
  expect_error(validate_cores(-1))
  expect_error(validate_cores("potato"))
  expect_error(validate_cores(NULL))
  expect_error(validate_cores(NA))
  expect_error(validate_cores(mtcars))
  expect_error(validate_cores(c(1, 2, 3)))
})


test_that("validate max_runtime", {
  resource_test(validate_max_runtime, NULL)
  resource_test(validate_max_runtime, 35)
  expect_error(validate_max_runtime("0"))
  expect_error(validate_max_runtime("Kazahstan"))
  expect_error(validate_max_runtime(NA))
  expect_error(validate_max_runtime(c(1, 2, 3)))
  expect_error(validate_max_runtime("0d0d"))
})


test_that("validate hold_until", {
  resource_test(validate_hold_until, NULL)
  resource_test(validate_hold_until, "tonight")
  resource_test(validate_hold_until, "midnight")
  expect_error(validate_hold_until(0))
  resource_test(validate_hold_until, 60)
  resource_test(validate_hold_until, "2h30", 150)
  resource_test(validate_hold_until, "1d1h1m", 1501)
  resource_test(validate_hold_until, Sys.Date() + 1,
                                     as.POSIXlt(Sys.Date() + 1))
  expect_error(validate_hold_until(Sys.Date()))
  now <- Sys.time()
  resource_test(validate_hold_until, now + 120, now + 120)
  expect_error(validate_hold_until(now - 1))
})

test_that("validate memory", {
  resource_test(validate_memory, NULL)
  expect_error(validate_memory(c("a", "b")))
  expect_error(validate_memory("10M"))
  resource_test(validate_memory, 1)
  resource_test(validate_memory, "1", 1)
  resource_test(validate_memory, "9G", 9)
  resource_test(validate_memory, "2T", 2000)
  expect_error(validate_memory("1G2T"))
  expect_error(validate_memory("1GG"))
})


test_that("validate nodes", {
  resource_test(validate_nodes, NULL)
  expect_error(validate_nodes(NA))
  resource_test(validate_nodes, c("A", "B"))
  resource_test(validate_nodes, "A")
  resource_test(validate_nodes, c("A", "A"), "A")
})


test_that("validate priority", {
  resource_test(validate_priority, NULL)
  resource_test(validate_priority, "low")
  resource_test(validate_priority, "normal")
  expect_error(validate_priority("high"))
  expect_error(validate_priority(3000))
})


test_that("validate queue", {
  resource_test(validate_queue, NULL)
  expect_error(validate_queue(NA))
  expect_error(validate_queue(c("a", "b")))
  resource_test(validate_queue, "Q")
})


test_that("Can get a hipercow_resource", {
  res <- hipercow_resources()
  expect_s3_class(res, "hipercow_resource")
  expect_equal(res$cores$computed, 1)
  expect_equal(res$exclusive$computed, FALSE)
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
  expect_equal(cluster_info$max_ram, 16)
  expect_equal(cluster_info$max_cores, 8)
  expect_true("kevin" %in% cluster_info$nodes)
  expect_true("Tesco" %in% cluster_info$queues)


  res <- hipercow_resources(cores = 1, memory_per_node = 5,
                            memory_per_process = 5,
                            queue = "Aldi",
                            requested_nodes = "Kevin")

  expect_true(hipercow_resources_validate(res, driver = "elsewhere",
                                          root = root))

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
