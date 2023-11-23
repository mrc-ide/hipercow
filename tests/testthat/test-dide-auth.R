test_that("Require a sensible name", {
  expect_error(dide_username(""), "Invalid empty username")
  expect_equal(dide_username("bob"), "bob")
  expect_equal(dide_username("DIDE\\bob"), "bob")
})
