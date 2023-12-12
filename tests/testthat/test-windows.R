test_that("windows_path calls hermod.windows", {
  p <- getwd()
  expect_identical(
    windows_path("home", p, "//fi--san03/homes/bob", "Q:"),
    hermod.windows:::windows_path("home", p, "//fi--san03/homes/bob", "Q:"))
})
