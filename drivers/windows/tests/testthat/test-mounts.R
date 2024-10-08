## Quite a bit of setup here, so a test of quite a bit of
## functionality in one go:
test_that("can locate root path among paths", {
  tmp <- withr::local_tempfile()
  mounts <- cbind(local = file.path(tmp, c("a", "b", "c")),
                  remote = c("\\\\server-1\\path",
                             "\\\\server-2\\homes\\b",
                             "\\\\server-2\\homes\\c"))
  paths <- file.path(mounts[, "local"], c("sub", "dir"))
  fs::dir_create(paths)
  paths <- clean_path_local(paths)
  mounts[, "local"] <- clean_path_local(mounts[, "local"])
  shares <- Map(windows_path,
                mounts[, "local"],
                mounts[, "remote"],
                c("X:", "Y:", "Z:"))
  ## In this case, the user explicitly provides a share that contains
  ## their working directory
  expect_equal(dide_add_extra_root_share(shares, paths[[1]], mounts),
               shares)
  result <- windows_path(mounts[1, "local"], mounts[1, "remote"], "V:")

  ## More commonly, we work out where the working directory is from
  ## their mounts:
  if (!is_windows()) {
    ## This test is hard to get right on windows, because the remote
    ## path needs to be a drive, and it's not here generally.
    expect_equal(dide_add_extra_root_share(shares[2], paths[[1]], mounts),
                 c(shares[2], list(result)))
  }
  ## Usually when we fail to find a working directory it's because
  ## it's not on a network path:
  expect_error(
    dide_add_extra_root_share(shares, getwd(), mounts),
    "Can't map local directory '.+'")
  expect_error(
    dide_add_extra_root_share(NULL, getwd(), mounts),
    "Can't map local directory '.+'")
  ## This is extremely unlikely:
  expect_error(
    dide_add_extra_root_share(NULL, paths[[1]], mounts[c(1, 1, 2, 3), ]),
    "Having trouble determining the working root directory mount point")
})


test_that("detect_mounts uses correct implementation", {
  ## Pretty heavy mocking here!
  mock_is_windows <- mockery::mock(TRUE, FALSE)
  mock_dmw <- mockery::mock()
  mock_dmu <- mockery::mock()

  mockery::stub(detect_mounts, "is_windows", mock_is_windows)
  mockery::stub(detect_mounts, "detect_mounts_windows", mock_dmw)
  mockery::stub(detect_mounts, "detect_mounts_unix", mock_dmu)

  detect_mounts()
  mockery::expect_called(mock_is_windows, 1)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 0)

  detect_mounts()
  mockery::expect_called(mock_is_windows, 2)
  mockery::expect_called(mock_dmw, 1)
  mockery::expect_called(mock_dmu, 1)
})


test_that("return sensible data when no mounts found (linux)", {
  mock_system2 <- mockery::mock(character())
  mockery::stub(detect_mounts_unix, "system2", mock_system2)
  res <- detect_mounts_unix()
  expect_equal(res, cbind(remote = character(), local = character()))
})


test_that("Parse return value into sensible output (linux)", {
  tmp <- withr::local_tempdir()
  tmp <- normalize_path(tmp)
  paths <- file.path(tmp, c("other", "home", "malaria"))
  fs::dir_create(paths)
  dat <- c(
    "//projects/other on %s/other type cifs (rw,relatime)",
    "//qdrive/homes/bob on %s/home type cifs (rw,relatime)",
    "//wpia-hn/Malaria on %s/malaria type cifs (rw,relatime)")
  dat <- vcapply(dat, function(x) sprintf(x, tmp), USE.NAMES = FALSE)
  mock_system2 <- mockery::mock(dat)
  mockery::stub(detect_mounts_unix, "system2", mock_system2)
  res <- detect_mounts_unix()
  cmp <- cbind(remote = c("\\\\projects\\other",
                          "\\\\qdrive\\homes\\bob",
                          "\\\\wpia-hn\\Malaria"),
               local = paths)
  expect_equal(res, cmp)
})


test_that("Warn if given unexpected output (linux)", {
  tmp <- withr::local_tempdir()
  tmp <- normalize_path(tmp)
  paths <- file.path(tmp, c("other", "home", "malaria"))
  fs::dir_create(paths)
  dat <- c(
    "//projects/other on %s/other type cifs (rw,relatime)",
    "//qdrive/homes/bob sur %s/home type cifs (rw,relatime)",
    "//wpia-hn/Malaria on %s/malaria type cifs (rw,relatime)")
  dat <- vcapply(dat, function(x) sprintf(x, tmp), USE.NAMES = FALSE)
  mock_system2 <- mockery::mock(dat)
  mockery::stub(detect_mounts_unix, "system2", mock_system2)
  expect_warning(
    res <- detect_mounts_unix(),
    "Ignoring mounts")
  cmp <- cbind(remote = c("\\\\projects\\other",
                          "\\\\wpia-hn\\Malaria"),
               local = paths[-2])
  expect_equal(res, cmp)
})


test_that("Can parse wmic output", {
  x <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\qdrive\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\projects\\tmp,OK\r")
  expect_equal(
    wmic_parse(x),
    cbind(remote = c("\\\\qdrive\\homes\\bob", "\\\\projects\\tmp"),
          local = c("q:", "T:")))
})


test_that("Ignore disconnected mounts", {
  x <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\qdrive\\homes\\bob,OK\r",
         "BROKEN,Disconnected,T:,\\\\projects\\broken,Degraded\r")
  expect_equal(
    wmic_parse(x),
    cbind(remote = c("\\\\qdrive\\homes\\bob"),
          local = c("q:")))
})


test_that("Can validate wmic output", {
  x <- c("\r",
         "node,connectionstate,localname,remotename,status\r",
         "BUILDERHV,Connected,q:,\\\\qdrive\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\projects\\tmp,OK\r")
  expect_error(
    wmic_parse(x),
    "Failed to find expected names in wmic output: RemoteName, LocalName")
})


test_that("detect_mounts_windows tries different methods in turn", {
  err <- list(success = FALSE,
              result = tryCatch(stop("some error"), error = identity))
  res <- list(success = TRUE,
              result = cbind(remote = "\\\\fi--remote\\path", local = "Q:"))
  mock_wmic_call <- mockery::mock(err, res)
  mockery::stub(detect_mounts_windows, "wmic_call", mock_wmic_call)

  expect_equal(detect_mounts_windows(), res$result)
  win_dir <- Sys.getenv("windir", "C:\\Windows")
  mockery::expect_called(mock_wmic_call, 2)
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list(sprintf("%s\\System32\\wbem\\en-US\\csv", win_dir))))
})


test_that("detect_mounts_windows errors if no method found", {
  err <- list(success = FALSE, result = "some error")
  mock_wmic_call <- mockery::mock(err, cycle = TRUE)
  mockery::stub(detect_mounts_windows, "wmic_call", mock_wmic_call)
  expect_error(
    detect_mounts_windows(),
    "Could not determine windows mounts using wmic.+some error")
  mockery::expect_called(mock_wmic_call, 3)
  win_dir <- Sys.getenv("windir", "C:\\Windows")
  expect_equal(
    mockery::mock_args(mock_wmic_call),
    list(list("csv"),
         list(sprintf("%s\\System32\\wbem\\en-US\\csv", win_dir)),
         list(sprintf("%s\\System32\\wbem\\en-GB\\csv", win_dir))))
})


test_that("wmic_call copes with command and parse errors", {
  res_err <- structure(character(0), status = 1)
  res_bad <- "lolno"
  res_good <- c("\r",
         "Node,ConnectionState,LocalName,RemoteName,Status\r",
         "BUILDERHV,Connected,q:,\\\\qdrive\\homes\\bob,OK\r",
         "BUILDERHV,Connected,T:,\\\\projects\\tmp,OK\r")

  mock_system <- mockery::mock(stop("Error running command"), res_bad, res_good)
  mockery::stub(wmic_call, "system_intern_check", mock_system)

  res1 <- wmic_call("csv")
  res2 <- wmic_call("csv")
  res3 <- wmic_call("csv")

  expect_equal(
    res1,
    list(success = FALSE, result = "Error running command"))
  expect_equal(
    res2,
    list(
      success = FALSE,
      result = paste("Failed to find expected names in wmic output:",
                     "RemoteName, LocalName, ConnectionState")))
  expect_equal(
    res3,
    list(success = TRUE, result = wmic_parse(res_good)))

  mockery::expect_called(mock_system, 3)
  expect_equal(
    mockery::mock_args(mock_system),
    rep(list(list('wmic netuse list brief /format:"csv"')), 3))
})


test_that("Find an available drive", {
  shares <- list(list(drive_remote = "V:"),
                 list(drive_remote = "W:"))
  expect_equal(available_drive(shares, "X:"), "X:")
  expect_equal(available_drive(shares, "/path"), "X:")
  expect_equal(available_drive(list(), "/path"), "V:")
})


test_that("Validate additional shares", {
  path <- withr::local_tempfile()
  mounts <- example_mounts(path)
  shares <- Map(windows_path,
                mounts[, "local"],
                mounts[, "remote"],
                c("O:", "Q:", "P:", "T:", "K:"),
                USE.NAMES = FALSE)
  expect_silent(dide_check_shares(shares))
  expect_equal(dide_check_shares(shares[[1]]), shares[1])
  expect_error(dide_check_shares(c(shares, TRUE)),
               "All elements of 'shares' must be a windows_path")
  expect_error(dide_check_shares(TRUE),
               "Invalid input for 'shares'")
  expect_null(dide_check_shares(list()))
  expect_null(dide_check_shares(NULL))
})


test_that("Prevent duplicated drives", {
  path <- withr::local_tempfile()
  mounts <- example_mounts(path)
  shares <- Map(windows_path,
                mounts[1:3, "local"],
                mounts[1:3, "remote"],
                c("X:", "Y:", "X:"))
  expect_error(
    dide_check_shares(shares),
    "Duplicate remote drive names: X:")
})


test_that("Remap nas regex - South Ken", {
  expect_equal(use_app_on_nas_south_ken("\\\\wpia-hn/X"), "\\\\wpia-hn-app/X")
  expect_equal(use_app_on_nas_south_ken("//wpia-hn/X"), "//wpia-hn-app/X")
  expect_equal(use_app_on_nas_south_ken(
    "\\\\wpia-hn.hpc.dide.ic.ac.uk\\X"),
    "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken(
    "//wpia-hn.hpc.dide.ic.ac.uk/X"),
    "//wpia-hn-app.hpc.dide.local/X")
  expect_equal(use_app_on_nas_south_ken(
    "\\\\wpia-hn.dide.local\\X"),
    "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken(
    "//wpia-hn.dide.local/X"),
    "//wpia-hn-app.hpc.dide.local/X")
  expect_equal(use_app_on_nas_south_ken(
    "\\\\wpia-hn.hpc.dide.local\\X"),
    "\\\\wpia-hn-app.hpc.dide.local\\X")
  expect_equal(use_app_on_nas_south_ken(
    "//wpia-hn.hpc.dide.local/X"),
    "//wpia-hn-app.hpc.dide.local/X")
})


test_that("Check app not used for non-infini shared", {
  expect_equal(use_app_on_nas_south_ken(
    "\\\\qdrive.dide.ic.ac.uk\\X"), "\\\\qdrive.dide.ic.ac.uk\\X")
})


test_that("can detect local mapping for drive", {
  mounts1 <- cbind(local = c("/a", "/b", "/c"),
                  remote = c("\\\\server-1\\path",
                             "\\\\server-2\\homes\\b",
                             "\\\\server-2\\homes\\c"))
  mounts2 <- cbind(local = c("/a", "/b", "/c"),
                   remote = c("//server-1.dide.ic.ac.uk/path",
                              "//server-2.dide.ic.ac.uk/homes/b",
                              "//server-2.dide.ic.ac.uk/homes/c"))
  expect_equal(
    dide_locally_resolve_unc_path("//server-1/path", mounts1),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("//server-1/path", mounts2),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-1\\path", mounts1),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-1\\path", mounts2),
    "/a")

  expect_equal(
    dide_locally_resolve_unc_path("//server-2.dide.ic.ac.uk/homes/b", mounts1),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("//server-2.dide.ic.ac.uk/homes/b", mounts2),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-2.dide.ic.ac.uk\\homes\\b",
                                  mounts1),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-2.dide.ic.ac.uk\\homes\\b",
                                  mounts2),
    "/b")

  expect_null(
    dide_locally_resolve_unc_path("//server-2/homes/a", mounts1))
  tmp <- withr::local_tempdir()
  expect_equal(dide_locally_resolve_unc_path(tmp, mounts1), tmp)
})
