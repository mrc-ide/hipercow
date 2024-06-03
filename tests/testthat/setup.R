withr::local_options(
  hipercow.auto_install_missing_packages = FALSE,
  hipercow.timeout = 30,
  hipercow.default_envvars = hipercow_envvars(),
  .local_envir = teardown_env()
)

## See pkgdepends readme; this is required for the examples to succeed
## under R CMD check
withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env()
)
