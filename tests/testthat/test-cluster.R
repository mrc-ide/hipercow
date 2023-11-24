test_that("Can transform cluster names", {
  expect_equal(cluster_name(NULL), "wpia-hn")
  expect_equal(cluster_name("wpia-hn"), "wpia-hn")
  expect_equal(cluster_name("sk"), "wpia-hn")
  expect_equal(cluster_name("new"), "wpia-hn")
  expect_equal(cluster_name("windows"), "wpia-hn")
})


test_that("can list valid templates", {
  expect_type(valid_templates("wpia-hn"), "character")
  expect_true("AllNodes" %in% valid_templates("wpia-hn"))
  expect_error(valid_templates("imperial"),
               "Invalid cluster 'imperial'")
})


test_that("can detect valid cores", {
  expect_equal(valid_cores("wpia-hn"), 32)
  expect_error(valid_cores("imperial"), "Invalid cluster 'imperial'")
})
