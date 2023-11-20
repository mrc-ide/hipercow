test_that("batch data creates entries for share drives", {
  config <- example_config(r_version = numeric_version("4.0.5"))
  root <- init_quietly(config$workdir)
  dat <- template_data(root, config)
  expect_match(dat$network_shares_create,
               "net use Q:", fixed = TRUE)
  expect_match(dat$network_shares_create,
               "net use T:", fixed = TRUE)
})
