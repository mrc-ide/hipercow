withr::local_options(
  hipercow.auto_install_missing_packages = FALSE,
  .local_envir = teardown_env()
)
