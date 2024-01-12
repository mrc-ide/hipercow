test_that("Check cluster usage", {
  valid <- valid_clusters()
  expect_silent(client_check("wpia-hn", valid))
  expect_error(
    client_check("wpia-hn", character(0)),
    "You do not have access to any cluster")
  expect_error(
    client_check("fi--dideclusthn", "wpia-hn"),
    "You do not have access to 'fi--dideclusthn'; try 'wpia-hn'")
  expect_error(
    client_check("fi--didegpu", c("a", "b")),
    "You do not have access to 'fi--didegpu'; try one of 'a', 'b'")
})


test_that("Construct a submit body", {
  p <- "\\\\fi--host\\\\path"
  resources <- list(
    cores = list(computed = 1), 
    exclusive = list(computed = FALSE),
    queue = list(computed = "GeneralNodes"))
  
  d <- client_body_submit(p, "name", resources, "fi--dideclusthn",
                          c("1", "2"))
  expect_setequal(
    names(d),
    c("cluster", "template", "rc", "rt", "jn", "wd", "se", "so",
      "jobs", "dep", "hpcfunc"))
  expect_equal(d$cluster, encode64("fi--dideclusthn"))
  expect_equal(d$template, encode64("GeneralNodes"))
  expect_equal(d$rc, encode64("1"))
  expect_equal(d$rt, encode64("Cores"))
  expect_equal(d$wd, "") # we might set this in future though
  expect_equal(d$se, "") # we might set this in future though
  expect_equal(d$so, "") # we might set this in future though
  expect_equal(d$jobs, encode64(sprintf('call "%s"', p)))
  expect_equal(d$dep, encode64("1,2"))
  expect_equal(d$hpcfunc, "submit")
})


test_that("submission body validates path", {
  p <- "\\\\fi--host\\\\path"
  expect_error(
    client_body_submit(gsub("\\", "/", p, fixed = TRUE), "name", 
                       resources = NULL, "fi--dideclusthn", 
                       character(0)),
    "All paths must be Windows network paths")
})


test_that("Construct a cancel body", {
  cluster <- "fi--dideclusthn"
  expect_equal(
    client_body_cancel("123456", cluster),
    list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel"),
         c123456 = "123456"))
  expect_equal(
    client_body_cancel(c("123456", "234567"), cluster),
    list(cluster = encode64(cluster),
         hpcfunc = encode64("cancel"),
         c123456 = "123456",
         c234567 = "234567"))
  expect_error(
    client_body_cancel(character(0), cluster),
    "Need at least one task to cancel")
})
