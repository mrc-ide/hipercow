test_that("can fetch cached web client", {
  mock_credentials <- mockery::mock(example_credentials())
  mockery::stub(get_web_client, "dide_credentials", mock_credentials)
  cache$web_client <- NULL
  on.exit(cache$web_client <- NULL)

  client <- get_web_client()
  expect_false(client$logged_in())
  mockery::expect_called(mock_credentials, 1)
  expect_identical(client, cache$web_client)

  expect_identical(get_web_client(), client)
  mockery::expect_called(mock_credentials, 1)
})


test_that("Can create api client", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  expect_false(cl$logged_in())
  expect_equal(cl$username(), credentials$username)
})


test_that("login sends sensible data", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  mock_login <- mockery::mock(cycle = TRUE)
  mock_post <- mockery::mock(mock_response(200), mock_response(403))
  mockery::stub(cl$login, "api_client_login", mock_login)
  mockery::stub(cl$logged_in, "self$POST", mock_post)

  cl$login(public = TRUE)
  expect_false(cl$logged_in())
  mockery::expect_called(mock_login, 0)
  mockery::expect_called(mock_post, 0)

  cl$login()
  mockery::expect_called(mock_login, 1)
  expect_equal(mockery::mock_args(mock_login)[[1]],
               unname(credentials))

  expect_true(cl$logged_in())
  mockery::expect_called(mock_post, 1)
  expect_equal(mockery::mock_args(mock_post)[[1]],
               list("/_listheadnodes.php", list(user = "")))

  cl$login(refresh = FALSE)
  mockery::expect_called(mock_login, 1)
  mockery::expect_called(mock_post, 1)

  cl$login(refresh = TRUE)
  mockery::expect_called(mock_login, 2)

  expect_false(cl$logged_in())
  mockery::expect_called(mock_post, 2)
})


test_that("logout uses correct endpoint", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  private <- r6_private(cl)
  private$has_logged_in <- TRUE

  mock_logout <- mockery::mock(cycle = TRUE)
  mock_get <- mockery::mock(mock_response(200))
  mockery::stub(cl$logout, "self$GET", mock_get)

  cl$logout()
  expect_false(private$has_logged_in)
  mockery::expect_called(mock_get, 1)
  expect_equal(
    mockery::mock_args(mock_get)[[1]],
    list("/logout.php", public = TRUE))
})


test_that("request handles http requests", {
  verb <- mockery::mock(mock_response(200),
                        mock_response(400))
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  data <- list(a = 1, b = 2)
  cl$request(verb, "/path/to", data = data, public = TRUE)
  expect_error(
    cl$request(verb, "/path/to", data = data, public = TRUE),
    "400")

  mockery::expect_called(verb, 2)
  expect_equal(
    mockery::mock_args(verb),
    rep(list(list("https://mrcdata.dide.ic.ac.uk/hpc/path/to",
                  data = data, request_timeout(NULL))), 2))
})


test_that("request logs back in after expiry", {
  verb <- mockery::mock(mock_response(403),
                        mock_response(200))
  credentials <- example_credentials()
  cl <- api_client$new(credentials)

  mock_login <- mockery::mock()
  mockery::stub(cl$request, "self$login", mock_login)

  expect_message(
    r <- cl$request(verb, "/path/to", data = data, public = FALSE),
    "Trying to login again, previous session likely expired")
  expect_equal(r, mock_response(200))

  mockery::expect_called(verb, 2)
  expect_equal(
    mockery::mock_args(verb),
    rep(list(list("https://mrcdata.dide.ic.ac.uk/hpc/path/to",
                  data = data, request_timeout(NULL))), 2))
})


test_that("GET forwards args to request", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  mock_request <- mockery::mock()
  mockery::stub(cl$GET, "self$request", mock_request)
  cl$GET("/api/v1/cluster_software/", public = TRUE)
  mockery::expect_called(mock_request, 1L)
  expect_equal(
    mockery::mock_args(mock_request)[[1]],
    list(httr::GET, "/api/v1/cluster_software/", public = TRUE))
})


test_that("POST forwards args to request", {
  credentials <- example_credentials()
  cl <- api_client$new(credentials)
  mock_request <- mockery::mock()
  mockery::stub(cl$POST, "self$request", mock_request)
  data <- list(a = "a", b = "b")
  cl$POST("/_listheadnodes.php", data, public = TRUE)
  mockery::expect_called(mock_request, 1L)
  ## Many more options here than above:
  expect_equal(
    mockery::mock_args(mock_request)[[1]],
    list(httr::POST, "/_listheadnodes.php", body = data, public = TRUE,
         httr::accept("text/plain"), encode = "form"))
})


test_that("Can send sensible login request", {
  mock_post <- mockery::mock(
    mock_response(403, content = "Some error"),
    mock_response(200,
                  content = "<p>You don't seem to have any HPC access</p>"),
    mock_response(200,
                  content = "Welcome"))
  mockery::stub(api_client_login, "httr::POST", mock_post)

  expect_error(
    api_client_login("username", "password"), "403")
  expect_error(
    api_client_login("username", "password"),
    "You do not have HPC access - please contact Wes")
  expect_silent(api_client_login("username", "password"))

  mockery::expect_called(mock_post, 3)
  expect_equal(
    mockery::mock_args(mock_post)[[1]],
    list("https://mrcdata.dide.ic.ac.uk/hpc/index.php",
         body = list(us = encode64("username"),
                     pw = encode64("password"),
                     hpcfunc = encode64("login")),
         encode = "form"))
})


test_that("Create client", {
  credentials <- example_credentials()
  cl <- web_client$new(credentials, login = FALSE)
  expect_s3_class(cl, "web_client")
  expect_false(cl$logged_in())
  expect_s3_class(cl$api_client(), "api_client")
})


test_that("login uses client to login and logout", {
  mock_client <- list(
    login = mockery::mock(),
    logout = mockery::mock())

  cl <- web_client$new(login = FALSE, client = mock_client)
  mockery::expect_called(mock_client$login, 0)
  cl$login()
  mockery::expect_called(mock_client$login, 1)
  expect_equal(mockery::mock_args(mock_client$login),
               list(list(refresh = TRUE)))

  cl <- web_client$new(login = TRUE, client = mock_client)
  mockery::expect_called(mock_client$login, 2)
  expect_equal(mockery::mock_args(mock_client$login),
               rep(list(list(refresh = TRUE)), 2))

  cl$logout()
  mockery::expect_called(mock_client$logout, 1)
  expect_equal(mockery::mock_args(mock_client$logout)[[1]], list())
})


test_that("client checks access", {
  mock_client <- list(
    login = function() NULL)
  mock_headnodes <- mockery::mock(
    character(0),
    "other",
    "wpia-hn",
    cycle = TRUE)
  cl <- web_client$new(cluster_default = "wpia-hn", client = mock_client)
  mockery::stub(cl$check_access, "self$headnodes", mock_headnodes)

  expect_error(
    cl$check_access(),
    "You do not have access to any cluster")
  expect_error(
    cl$check_access(),
    "You do not have access to 'wpia-hn'; try 'other'")
  expect_silent(cl$check_access())

  expect_error(
    cl$check_access("wpia-hn"),
    "You do not have access to any cluster")
  expect_silent(cl$check_access("other"))
  expect_silent(cl$check_access("wpia-hn"))
})


test_that("submit sends correct payload", {
  dide_id <- "12345"
  content <- sprintf("Job has been submitted. ID: %s.\n", dide_id)
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  path <- "\\\\host\\path"
  resources <- list(
    cores = 1,
    exclusive = FALSE,
    queue = "AllNodes")

  expect_equal(
    cl$submit(path, "name", resources = resources,
              depends_on = c("123", "456")),
    dide_id)
  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/submit_1.php",
         client_body_submit(path, "name", resources, "wpia-hn",
                            c("123", "456"))))

  expect_equal(
    cl$submit(path, "name", resources, "wpia-hn",
              depends_on = character()),
    dide_id)
  mockery::expect_called(mock_client$POST, 2L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[2]],
    list("/submit_1.php",
         client_body_submit(path, "name", resources, "wpia-hn",
                            character())))
})

test_that("hipercow_resources processed into web api call", {
  res <- hipercow::hipercow_resources(
    hold_until = "2m", queue = "AllNodes", max_runtime = "2h30m",
    priority = "low", memory_per_node = "3200G", memory_per_process = "100G",
    requested_nodes = c("wpia-063", "wpia-065"),
    exclusive = TRUE, cores = Inf)
  path <- "\\\\host\\path"
  cbs <- client_body_submit(path = path, name = "Cow", res, "hermod",
                            depends_on = c(123, 456))

  expect_setequal(names(cbs),
                  c("cluster", "template", "jn", "wd", "se", "so",
                    "jobs", "dep", "hpcfunc", "rc", "rt", "exc", "mpn",
                    "epm", "rnt", "hu", "rn", "pri", "ver"))
  expect_equal(length(names(cbs)), length(unique(names(cbs))))

  expect_equal(cbs$cluster, encode64("hermod"))
  expect_equal(cbs$template, encode64("AllNodes"))
  expect_equal(cbs$jn, encode64("Cow"))
  expect_equal(cbs$wd, encode64(""))
  expect_equal(cbs$se, encode64(""))
  expect_equal(cbs$so, encode64(""))
  expect_equal(cbs$jobs, encode64(sprintf("call \"%s\"", path)))
  expect_equal(cbs$dep, encode64("123,456"))
  expect_equal(cbs$hpcfunc, "submit")
  expect_equal(cbs$rc, encode64("1"))
  expect_equal(cbs$rt, encode64("Nodes"))
  expect_equal(cbs$exc, encode64("1"))
  expect_equal(cbs$mpn, encode64("3200000"))
  expect_equal(cbs$epm, encode64("100000"))
  expect_equal(cbs$rnt, encode64("150"))
  expect_equal(cbs$hu, encode64("2"))
  expect_equal(cbs$rn, encode64("wpia-063,wpia-065"))
  expect_equal(cbs$pri, encode64("low"))

  now <- Sys.time() + 1
  res$hold_until <- now
  res$cores <- 3
  cbs <- client_body_submit(path = path, name = "Cow", res, "hermod",
                            depends_on = c(123, 456))

  expect_equal(cbs$hu, encode64(format(now, "\"%Y-%m-%d %H:%M:%S\"")))
  expect_equal(cbs$rc, encode64("3"))
  expect_equal(cbs$rt, encode64("Cores"))


})


test_that("cancel sends correct payload", {
  dide_id <- "12345"
  content <- sprintf("%s\tOK\n", dide_id)
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$cancel(dide_id), setNames("OK", dide_id))

  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/cancel.php",
         client_body_cancel(dide_id, "wpia-hn"),
         timeout = Inf))
})


test_that("status sends correct payload", {
  content <- read_lines("responses/status.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r),
                      username = mockery::mock("bob"))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$status_user(), client_parse_status(content))

  mockery::expect_called(mock_client$username, 1L)
  expect_equal(mockery::mock_args(mock_client$username)[[1]], list())

  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/_listalljobs.php",
         client_body_status("*", "bob", "wpia-hn")))
})


test_that("log sends correct payload", {
  dide_id <- "12345"
  content <- read_lines("responses/log.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$log(dide_id), client_parse_log(content))

  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/showjobfail.php",
         client_body_log(dide_id, "wpia-hn")))
})


test_that("status job sends correct payload", {
  dide_id <- "12345"
  r <- mock_response(200, content = "Running")
  mock_client <- list(GET = mockery::mock(r))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_equal(cl$status_job(dide_id, "wpia-hn"),
               "running")

  mockery::expect_called(mock_client$GET, 1L)
  expect_equal(
    mockery::mock_args(mock_client$GET)[[1]],
    list("/api/v1/get_job_status/",
         query = list(scheduler = "wpia-hn",
                      jobid = dide_id)))
})


test_that("headnodes sends correct payload", {
  content <- paste0(valid_clusters(), "\n", collapse = "")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))
  cl <- web_client$new(login = FALSE, client = mock_client)
  expect_null(r6_private(cl)$headnodes_)

  expect_equal(cl$headnodes(), valid_clusters())
  expect_equal(r6_private(cl)$headnodes_, valid_clusters())
  mockery::expect_called(mock_client$POST, 1L)
  expect_equal(
    mockery::mock_args(mock_client$POST)[[1]],
    list("/_listheadnodes.php", list(user = "")))

  expect_equal(cl$headnodes(), valid_clusters())
  mockery::expect_called(mock_client$POST, 1L)

  expect_equal(cl$headnodes(TRUE), valid_clusters())
  mockery::expect_called(mock_client$POST, 2L)
  expect_equal(mockery::mock_args(mock_client$POST)[[1]],
               mockery::mock_args(mock_client$POST)[[2]])
})


test_that("load endpoints are correct", {
  content <- read_lines("responses/load.txt")
  r <- mock_response(200, content = content)
  mock_client <- list(POST = mockery::mock(r, cycle = TRUE))

  cl <- web_client$new(login = FALSE, client = mock_client)
  private <- r6_private(cl)
  private$headnodes_ <- c("wpia-hn", "fi--didemrchnb")

  cmp1 <- client_parse_load_cluster(content, "wpia-hn")
  cmp2 <- client_parse_load_overall(
    lapply(private$headnodes_, client_parse_load_cluster, txt = content))
  expect_equal(cl$load_node(), cmp1)
  expect_equal(cl$load_overall(), cmp2)

  expect_output(
    expect_equal(
      withVisible(cl$load_show()), list(value = cmp1, visible = FALSE)),
    "wpia-dideclus35")
  expect_output(
    expect_equal(
      withVisible(cl$load_show(TRUE)), list(value = cmp2, visible = FALSE)),
    "didehpc")
})


test_that("version endpoint can be called", {
  testthat::skip_if_offline()
  client <- web_client$new("bob")
  versions <- client$r_versions()
  expect_setequal(names(versions), c("windows", "linux", "linux_make"))
  expect_s3_class(versions$windows, "numeric_version")
  expect_s3_class(versions$linux, "numeric_version")
})


test_that("version endpoint is correct", {
  content <-
  '{"software": [
     {"name": "R", "version": "4.0.5"},
     {"name": "R", "version": "4.1.3"}],
    "linuxsoftware": [
     {"name": "R", "version": "4.0.6"},
     {"name": "R", "version": "4.1.4"}]}'

  r <- mock_response(200, content = content)
  mock_client <- list(GET = mockery::mock(r, cycle = TRUE))

  cl <- web_client$new(login = FALSE, client = mock_client)
  private <- r6_private(cl)
  res <- cl$r_versions()
  expect_setequal(names(res), c("windows", "linux", "linux_make"))
  expect_equal(res$windows, numeric_version(c("4.0.5", "4.1.3")))
  expect_equal(res$linux, numeric_version(c("4.0.6", "4.1.4")))
})


test_that("can select sensible timeout", {
  withr::with_options(list("hipercow.dide.timeout" = NULL), {
    expect_equal(request_timeout(NULL), httr::timeout(10))
    expect_equal(request_timeout(20), httr::timeout(20))
    expect_null(request_timeout(Inf))
  })

  withr::with_options(list("hipercow.dide.timeout" = 30), {
    expect_equal(request_timeout(NULL), httr::timeout(30))
    expect_equal(request_timeout(20), httr::timeout(20))
    expect_null(request_timeout(Inf))
  })
})


test_that("request logs back in timeout failure", {
  verb <- mockery::mock(stop("Timeout was reached: more details"),
                        mock_response(200))
  credentials <- example_credentials()
  cl <- api_client$new(credentials)

  mock_login <- mockery::mock()
  mockery::stub(cl$request, "self$login", mock_login)

  expect_message(
    r <- cl$request(verb, "/path/to", data = data, public = FALSE),
    "Looks like your curl handle might be stale")
  expect_equal(r, mock_response(200))

  mockery::expect_called(verb, 2)
  expect_equal(
    mockery::mock_args(verb),
    rep(list(list("https://mrcdata.dide.ic.ac.uk/hpc/path/to",
                  data = data, request_timeout(NULL))), 2))
})
