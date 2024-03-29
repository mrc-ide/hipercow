test_that("Cluster info returns something for wpia-hn", {
  config <- list(cluster = "wpia-hn")
  res <- windows_cluster_info(config, "")
  expect_setequal(names(res), c("resources", "r_versions", "redis_url"))
  expect_setequal(
    names(res$resources),
    c("max_cores", "max_ram", "queues", "default_queue", "nodes"))
  expect_s3_class(res$r_versions, "numeric_version")
  expect_equal(res$r_versions, r_versions())
  expect_equal(res$redis_url, "wpia-hn.hpc.dide.ic.ac.uk")
})


test_that("Cluster info for unknown headnode", {
  config <- list(cluster = "gru")
  expect_error(windows_cluster_info(config, ""),
               "Cluster 'gru' not supported by windows driver")
})
