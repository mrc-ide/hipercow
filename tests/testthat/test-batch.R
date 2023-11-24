test_that("batch data creates entries for share drives", {
  skip("rework")
  config <- example_config(r_version = numeric_version("4.0.5"))
  root <- init_quietly(config$workdir)
  dat <- template_data(root, config)
  expect_match(dat$network_shares_create,
               "net use Q:", fixed = TRUE)
  expect_match(dat$network_shares_create,
               "net use T:", fixed = TRUE)
})


test_that("can write a runner batch file", {
  skip("rework")
  config <- example_config(r_version = numeric_version("4.0.5"))
  root <- init_quietly(config$workdir)
  id <- hermod_task_create_explicit(quote(sessionInfo()), root = root)
  write_batch_task_run(root, config, id)
  expect_true(file.exists(file.path(root$path$tasks, id, "run.bat")))
})


test_that("can create temp drive if not listed", {
  skip("rework")
  config <- example_config()
  root <- init_quietly(config$workdir)
  dat1 <- template_data(root, config)
  config$shares <- config$shares["home"]
  dat2 <- template_data(root, config)
  expect_equal(dat1, dat2)
})
