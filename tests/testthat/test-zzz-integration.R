test_that("can provision a library", {
  elsewhere_register()
  path_here <- withr::local_tempdir()
  path_there <- withr::local_tempdir()
  init_quietly(path_here)
  init_quietly(path_there)
  root <- hipercow_root(path_here)
  suppressMessages(
    hipercow_configure("elsewhere", path = path_there, root = path_here))
  writeLines('install.packages("R6")', file.path(path_here, "provision.R"))
  suppressMessages(hipercow_provision(root = path_here, show_log = FALSE))
  expect_true(file.exists(file.path(path_there, "hipercow", "lib", "R6")))
})


test_that("Can run task in parallel", {
  path <- withr::local_tempdir()
  init_quietly(path)
  parallel <- hipercow_parallel("parallel")
  resources <- hipercow_resources(cores = 2)

  id <- withr::with_dir(
    path,
    suppressMessages(
      task_create_expr(
        parallel::clusterApply(NULL, 1:2, function(x) list(x, Sys.getpid())),
        parallel = parallel,
        resources = resources)))
  dat <- readRDS(path_to_task_file(path, id, "data"))
  expect_null(dat$variables)
  expect_equal(dat$parallel$method, "parallel")

  res <- evaluate_promise(
    withr::with_envvar(
      c(HIPERCOW_CORES = 2),
      withr::with_dir(path,  task_eval(id, root = path))))
  expect_length(res$messages, 4)
  expect_match(res$messages[[1]],
               "Creating a parallel cluster with 2 processes")
  expect_match(res$messages[[2]],
               "Cluster ready to use")
  expect_match(res$messages[[3]],
               "Stopping cluster")
  expect_match(res$messages[[4]],
               "Cluster stopped")

  expect_true(res$result)

  res <- task_result(id, root = path)
  expect_equal(res[[1]][[1]], 1)
  expect_equal(res[[2]][[1]], 2)
  expect_true(res[[1]][[2]] != res[[2]][[2]])
})
