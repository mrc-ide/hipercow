test_that("Can transform cluster names", {
  expect_equal(cluster_name(NULL), "wpia-hn")
  expect_equal(cluster_name("wpia-hn"), "wpia-hn")
  expect_equal(cluster_name("sk"), "wpia-hn")
  expect_equal(cluster_name("new"), "wpia-hn")
  expect_equal(cluster_name("windows"), "wpia-hn")
})


test_that("if r_versions cache is empty, call client", {
  prev <- cache$r_versions
  rm(list = "r_versions", envir = cache)
  on.exit(cache$r_versions <- prev)

  versions <- list(windows = numeric_version(c("4.2.3", "4.3.1")),
                   linux = numeric_version(c("4.5.6", "0.1.2")))
  fetch <- mockery::mock(versions)
  mockery::stub(r_versions, "r_versions_fetch", fetch)
  expect_equal(r_versions("windows"), versions$windows)
  expect_equal(cache$r_versions, versions)
  mockery::expect_called(fetch, 1)
  expect_equal(r_versions("windows"), versions$windows)
  mockery::expect_called(fetch, 1)
})


test_that("fetch r versions", {
  testthat::skip_if_offline()
  ## Sometimes flakey
  testthat::try_again(5, dat <- r_versions_fetch())
  expect_s3_class(dat$windows, "numeric_version")
  expect_s3_class(dat$linux, "numeric_version")
  expect_true(numeric_version("4.4.2") %in% dat$windows)
  expect_true(numeric_version("4.4.2") %in% dat$linux)
  expect_true(length(dat$windows) > 3)
  expect_true(length(dat$linux) > 3)
})
