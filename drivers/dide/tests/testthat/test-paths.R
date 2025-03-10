test_that("can create a path mapping", {
  p <- getwd()
  m <- dide_path(p, "//qdrive/homes/bob", "Q:")
  expect_s3_class(m, "dide_path")
  str <- as.character(m)
  expect_match(str, "\\(local\\) .+ => .+ \\(remote\\)")
  expect_output(print(m), str, fixed = TRUE)
})

test_that("cannot create a path mapping on I:", {
  p <- getwd()
  expect_error(dide_path(p, "//qdrive/homes/bob", "I:"),
               "You cannot use I:")
})

test_that("can validate creation of path mapping", {
  expect_error(
    dide_path(tempfile(), "//qdrive/homes/bob", "Q:"),
    "Local mount point does not exist.")
  expect_error(
    dide_path("Q:", "Q://qdrive/homes/bob", "Q:"),
    "path_remote must be a network path")
  expect_error(
    dide_path(getwd(), "//qdrive/homes/bob", "Q"),
    "drive_remote must be of the form 'X:'")
})


test_that("Can clean a remote path", {
  expect_equal(
    clean_path_remote("//qdrive/homes/bob"),
    "\\\\qdrive.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//qdrive.dide.local/homes/bob"),
    "\\\\qdrive.dide.ic.ac.uk\\homes\\bob")
  expect_equal(
    clean_path_remote("//qdrive.dide.ic.ac.uk/homes/bob/"),
    "\\\\qdrive.dide.ic.ac.uk\\homes\\bob")
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
    dide_path(p, "//qdrive/homes/bob", "Q:"),
    dide_path(tempdir(), "//qdrive/tmp", "T:"))

  x <- prepare_path(t, shares)
  expect_equal(x$rel, basename(t))
  expect_s3_class(x, "dide_path")
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
    home = dide_path(p, "//qdrive/homes/bob", "Q:"),
    temp = dide_path(tempdir(), "//qdrive/tmp", "T:"))

  res <- remote_path(t, shares, "windows")
  expect_equal(res, paste0("T:/", basename(t)))

  d <- file.path(t, "foo", "bar")
  fs::dir_create(d)
  res <- remote_path(d, shares, "windows")
  expect_equal(res, paste0("T:/", basename(t), "/foo/bar"))
})


test_that("dide_path agrees with interface in hipercow", {
  expect_identical(formals(dide_path), formals(hipercow::dide_path))
})
