test_that("Cluster info returns something for wpia-hn", {
  # NB - this is accessing the API - we should try and mock maybe
  config <- list(cluster = "wpia-hn", platform = "windows")
  res <- dide_cluster_info(config, "")
  expect_setequal(names(res), c("resources", "r_versions", "redis_url"))
  expect_setequal(
    names(res$resources),
    c("name", "node_os",
      "max_cores", "max_ram", "queues", "default_queue", "nodes",
      "build_queue", "redis_url", "testing_queue"))
  expect_s3_class(res$r_versions, "numeric_version")
  expect_equal(res$r_versions, r_versions("windows"))
  expect_equal(res$redis_url, "wpia-hn.hpc.dide.ic.ac.uk")
})


test_that("Cluster info for unknown headnode", {
  config <- list(cluster = "gru", platform = "windows")
  expect_error(dide_cluster_info(config, ""),
               "Cluster 'gru' not supported by dide driver")
})

test_that("Cluster info for unknown platform", {
  config <- list(cluster = "wpia-hn", platform = "potato")
  expect_error(dide_cluster_info(config,
               "'config$platform' must be one of 'windows', 'linux'"))
})
