withr::local_options(
  hipercow.auto_install_missing_packages = FALSE,
  hipercow.timeout = 30,
  .local_envir = teardown_env()
)
