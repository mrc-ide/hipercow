test_that("Cluster info returns something for wpia-hn", {
  config <- list(cluster = "wpia-hn")
  res <- windows_cluster_info(config, "")
  expect_s3_class(res, "windows_cluster_info")
  expect_setequal(names(res), c("max_cores", "max_ram", "queues", "nodes"))
})


test_that("Cluster info for unknown headnode", {
  config <- list(cluster = "gru")
  expect_error(windows_cluster_info(config, ""))
})

