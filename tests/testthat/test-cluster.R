test_that("Can transform cluster names", {
  expect_equal(cluster_name(NULL), "fi--dideclusthn")
  expect_equal(cluster_name("fi--dideclusthn"), "fi--dideclusthn")
  expect_equal(cluster_name("small"), "fi--dideclusthn")
  expect_equal(cluster_name("little"), "fi--dideclusthn")
  expect_equal(cluster_name("dide"), "fi--dideclusthn")
  expect_equal(cluster_name("ide"), "fi--dideclusthn")
  expect_equal(cluster_name("dideclusthn"), "fi--dideclusthn")

  expect_equal(cluster_name("fi--didemrchnb"), "fi--didemrchnb")
  expect_equal(cluster_name("big"), "fi--didemrchnb")
  expect_equal(cluster_name("mrc"), "fi--didemrchnb")
})


test_that("can list valid templates", {
  expect_type(valid_templates("fi--dideclusthn"), "character")
  expect_true("GeneralNodes" %in% valid_templates("fi--dideclusthn"))
  expect_type(valid_templates("fi--didemrchnb"), "character")
  expect_true("GeneralNodes" %in% valid_templates("fi--didemrchnb"))
  expect_type(valid_templates("wpia-hn"), "character")
  expect_true("AllNodes" %in% valid_templates("wpia-hn"))
  expect_error(valid_templates("imperial"),
               "Invalid cluster 'imperial'")
})


test_that("can detect valid cores", {
  expect_equal(valid_cores("fi--dideclusthn"), 24)
  expect_equal(valid_cores("fi--didemrchnb"), 64)
  expect_equal(valid_cores("wpia-hn"), 32)
  expect_error(valid_cores("imperial"), "Invalid cluster 'imperial'")
})
