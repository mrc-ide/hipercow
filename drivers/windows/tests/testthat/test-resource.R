test_that("Cluster info returns something", {
  
  # Trivial as it returns something constant.
  
  res <- windows_cluster_info()
  expect_s3_class(res, "windows_cluster_info")
  expect_equal(length(res), 4)
  expect_setequal(names(res), c("max_cores", "max_ram", "queues", "nodes"))
})
