test_that("Can format the overall load", {
  x <- list(cluster = "hipercow",
            detail = NULL,
            summary = data.frame(
              name = c("wpia-hn", "wpia-hn2"),
              free = c(203, 850),
              used = c(13, 930),
              total = c(216, 1780),
              percent_used = c(6, 52),
              stringsAsFactors = FALSE),
            overall = list(name = "hipercow",
                           free = 1053,
                           used = 943,
                           total = 1996,
                           percent_used = 47))
  class(x) <- "dide_clusterload"
  expected <- c("    name free used total % used",
                "-------- ---- ---- ----- ------",
                " wpia-hn  203   13   216     6%",
                "wpia-hn2  850  930  1780    52%",
                "-------- ---- ---- ----- ------",
                "hipercow 1053  943  1996    47%")
  str <- withr::with_options(list(crayon.enabled = FALSE),
                             format(x))
  expect_equal(str, expected)
  str_col <- withr::with_options(list(crayon.enabled = TRUE),
                                 format(x))
  ## TODO: some testthat change means that ansi colours are not forced
  ## on anymore, and this fails. Test passes locally though.
  ## > expect_true(any(crayon::has_style(str_col)))
  expect_equal(crayon::strip_style(str_col), str)

  str2 <- withr::with_options(list(crayon.enabled = FALSE),
                              format(x, nodes = FALSE))
  expect_equal(str2, expected[c(1:2, 6)])

  withr::with_options(
    list(crayon.enabled = FALSE),
    expect_equal(capture.output(print(x)), str))
  withr::with_options(
    list(crayon.enabled = TRUE),
    expect_equal(capture.output(print(x)), str_col))
})
