test_that("can create a path mapping", {
  p <- getwd()
  m <- windows_path("home", p, "//fi--san03/homes/bob", "Q:")
  expect_s3_class(m, "windows_path")
  str <- as.character(m)
  expect_match(str, "\\(local\\) .+ => .+ \\(remote\\)")
  expect_output(print(m), str, fixed = TRUE)
})


test_that("can validate creation of path mapping", {
  expect_error(
    windows_path("home", tempfile(), "//fi--san03/homes/bob", "Q:"),
    "Local mount point does not exist: ")
  expect_error(
    windows_path("home", "Q:", "Q://fi--san03/homes/bob", "Q:"),
    "path_remote must be a network path, starting with")
  expect_error(
    windows_path("home", getwd(), "//fi--san03/homes/bob", "Q"),
    "drive_remote must be of the form 'X:'")
})


test_that("Can clean a remote path", {
  expect_equal(
    clean_path_remote("//fi--san03/homes/bob"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//fi--san03.dide.local/homes/bob"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//fi--san03.dide.ic.ac.uk/homes/bob/"),
    "\\\\fi--san03.dide.ic.ac.uk\\homes\\bob")
})


test_that("Can deal with wpia-hn (.hpc) paths", {
  answer <- "\\\\wpia-hn.hpc.dide.ic.ac.uk\\share\\data"

  expect_equal(clean_path_remote("//wpia-hn/share/data"),
               answer)
  expect_equal(clean_path_remote("//wpia-hn.dide.local/share/data"),
               answer)
  expect_equal(clean_path_remote("//wpia-hn.dide.ic.ac.uk/share/data"),
               answer)
  expect_equal(clean_path_remote("//wpia-hn.hpc/share/data"),
               answer)
  expect_equal(clean_path_remote("//wpia-hn.hpc.dide.local/share/data"),
               answer)
  expect_equal(clean_path_remote("//wpia-hn.hpc.dide.ic.ac.uk/share/data"),
               answer)
})


test_that("Can detect a path into a share", {
  p <- dirname(getwd())
  t <- withr::local_tempdir()
  t <- normalize_path(t)
  shares <- list(
    windows_path("home", p, "//fi--san03/homes/bob", "Q:"),
    windows_path("temp", tempdir(), "//fi--san03/tmp", "T:"))

  x <- prepare_path(t, shares)
  expect_equal(x$rel, basename(t))
  expect_s3_class(x, "windows_path")
  expect_equal(x[names(x) != "rel"], shares[[2]][])
  str <- as.character(x)
  expect_match(str, "\\[rel: .+\\] \\(local\\) .+ => .+ => T: \\(remote\\)")
})


test_that("prepare_path rejects nonexistant paths", {
  expect_error(
    prepare_path(tempfile(tmpdir = getwd()), list()),
    "path does not exist:")
})


test_that("prepare_path handles unmapped paths", {
  expect_error(
    prepare_path(getwd(), list()),
    "did not find network mapping for path")
  expect_null(
    prepare_path(getwd(), list(), FALSE))
})


test_that("Can create a remote path", {
  p <- dirname(getwd())
  t <- withr::local_tempdir()
  t <- normalize_path(t)
  shares <- list(
    home = windows_path("home", p, "//fi--san03/homes/bob", "Q:"),
    temp = windows_path("temp", tempdir(), "//fi--san03/tmp", "T:"))
  res <- remote_path(t, shares)
  expect_equal(
    res,
    paste0("\\\\fi--san03.dide.ic.ac.uk\\tmp\\", basename(t)))
})
