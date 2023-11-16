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
