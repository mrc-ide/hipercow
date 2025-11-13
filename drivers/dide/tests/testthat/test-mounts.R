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
  shares <- Map(dide_path,
                mounts[, "local"],
                mounts[, "remote"],
                c("X:", "Y:", "Z:"))
  ## In this case, the user explicitly provides a share that contains
  ## their working directory
  expect_equal(dide_add_extra_root_share(shares, paths[[1]], mounts),
               shares)
  result <- dide_path(mounts[1, "local"], mounts[1, "remote"], "V:")

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
  shares <- Map(dide_path,
                mounts[, "local"],
                mounts[, "remote"],
                c("O:", "Q:", "P:", "T:", "K:"),
                USE.NAMES = FALSE)
  expect_silent(dide_check_shares(shares))
  expect_equal(dide_check_shares(shares[[1]]), shares[1])
  expect_error(dide_check_shares(c(shares, TRUE)),
               "All elements of 'shares' must be a dide_path")
  expect_error(dide_check_shares(TRUE),
               "Invalid input for 'shares'")
  expect_null(dide_check_shares(list()))
  expect_null(dide_check_shares(NULL))
})


test_that("Prevent duplicated drives", {
  path <- withr::local_tempfile()
  mounts <- example_mounts(path)
  shares <- Map(dide_path,
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
    dide_locally_resolve_unc_path("//server-1/path", mounts1, TRUE),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("//server-1/path", mounts2, TRUE),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-1\\path", mounts1, TRUE),
    "/a")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-1\\path", mounts2, TRUE),
    "/a")

  expect_equal(
    dide_locally_resolve_unc_path("//server-2.dide.ic.ac.uk/homes/b", mounts1,
                                  TRUE),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("//server-2.dide.ic.ac.uk/homes/b", mounts2,
                                  TRUE),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-2.dide.ic.ac.uk\\homes\\b",
                                  mounts1, TRUE),
    "/b")
  expect_equal(
    dide_locally_resolve_unc_path("\\\\server-2.dide.ic.ac.uk\\homes\\b",
                                  mounts2, TRUE),
    "/b")

  expect_null(
    dide_locally_resolve_unc_path("//server-2/homes/a", mounts1, TRUE))
  tmp <- withr::local_tempdir()
  expect_equal(dide_locally_resolve_unc_path(tmp, mounts1), tmp)
})


test_that("Can convert home drive UNC path to Linux cluster node path", {

  for (path in c("wpia-san04.dide.ic.ac.uk",
                 "wpia-san04",
                 "qdrive.dide.ic.ac.uk",
                 "qdrive")) {

    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\homes\\alice", path),
      rel = "potato")), "/mnt/homes/alice/potato")
  }
})

test_that("Can convert wpia-hn roots to Linux cluster node path", {

  for (path in c("wpia-hn.hpc.dide.ic.ac.uk",
                 "wpia-hn.dide.ic.ac.uk",
                 "wpia-hn")) {
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage", path),
      rel = "folder/1")), "/mnt/cluster/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage", path),
      rel = "folder")), "/mnt/cluster/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage", path),
      rel = "")), "/mnt/cluster")
  }
})

test_that("Can convert wpia-hn deeper mount to Linux cluster node path", {
  for (path in c("wpia-hn.hpc.dide.ic.ac.uk",
                 "wpia-hn.dide.ic.ac.uk",
                 "wpia-hn")) {
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage\\a", path),
      rel = "folder/1")), "/mnt/cluster/a/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage\\b\\c", path),
      rel = "folder")), "/mnt/cluster/b/c/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\cluster-storage\\d\\e\\f", path),
      rel = "")), "/mnt/cluster/d/e/f")
  }
})

test_that("Can convert wpia-hn2 climate share to Linux cluster node path", {
  for (path in c("wpia-hn2.hpc.dide.ic.ac.uk",
                 "wpia-hn2.dide.ic.ac.uk",
                 "wpia-hn2")) {
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage", path),
      rel = "folder/1")), "/mnt/vimc-cc1/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage", path),
      rel = "folder")), "/mnt/vimc-cc1/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage", path),
      rel = "")), "/mnt/vimc-cc1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage\\a", path),
      rel = "folder/1")), "/mnt/vimc-cc1/a/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage\\b\\c", path),
      rel = "folder")), "/mnt/vimc-cc1/b/c/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\climate-storage\\d\\e\\f", path),
      rel = "")), "/mnt/vimc-cc1/d/e/f")
    }
})

test_that("Can convert wpia-hn2 vimc-cc2 share to Linux cluster node path", {
  for (path in c("wpia-hn2.hpc.dide.ic.ac.uk",
                 "wpia-hn2.dide.ic.ac.uk",
                 "wpia-hn2")) {
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage", path),
      rel = "folder/1")), "/mnt/vimc-cc2/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage", path),
      rel = "folder")), "/mnt/vimc-cc2/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage", path),
      rel = "")), "/mnt/vimc-cc2")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage\\a", path),
      rel = "folder/1")), "/mnt/vimc-cc2/a/folder/1")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage\\b\\c", path),
      rel = "folder")), "/mnt/vimc-cc2/b/c/folder")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\vimc-cc2-storage\\d\\e\\f", path),
      rel = "")), "/mnt/vimc-cc2/d/e/f")
  }
})

test_that("Can convert wpia-hn legacy shares", {
  mock_dir_exists <- mockery::mock(TRUE, cycle = TRUE)
  mockery::stub(unc_to_linux_hpc_mount, "unc_path_exist_windows", mock_dir_exists)

  for (path in c("wpia-hn",
                 "wpia-hn.dide.ic.ac.uk",
                 "wpia-hn.hpc.dide.ic.ac.uk")) {
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share", path),
      rel = "")), "/mnt/cluster/legacy_share")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share\\a", path),
      rel = "")), "/mnt/cluster/legacy_share/a")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share\\a\\b", path),
      rel = "")), "/mnt/cluster/legacy_share/a/b")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share", path),
      rel = "c")), "/mnt/cluster/legacy_share/c")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share\\a", path),
      rel = "c")), "/mnt/cluster/legacy_share/a/c")
    expect_equal(unc_to_linux_hpc_mount(list(
      path_remote = sprintf("\\\\%s\\legacy_share\\a\\b", path),
      rel = "c/d")), "/mnt/cluster/legacy_share/a/b/c/d")
  }
})

test_that("Can detect windows mounts with powershell", {
  res <- readLines("responses/powershell_smb.txt")
  mockery::stub(detect_mounts_windows, "system2", res)
  x <- detect_mounts_windows()
  expect_equal(nrow(x), 2)
  expect_equal(x[, 1], c(r"{\\wpia-hn\hipercow}", r"{\\wpia-didef4\tmp}"))
  expect_equal(x[, 2], c("I:", "T:"))
})
