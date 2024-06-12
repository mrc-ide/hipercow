message("Installing most recent version of 'cmdstanr' package")
local({
  lib <- tempfile()
  loadNamespace("remotes")
  dir.create(lib, FALSE, TRUE)
  .libPaths(lib)
  if ({{cmdstanr_use_remotes}}) {
    remotes::install_gituhb("stan-dev/cmdstanr", lib = lib)
  } else {
    install.packages("cmdstanr",
                     repos = c("https://stan-dev.r-universe.dev",
                               "https://cloud.r-project.org"),
                     lib = lib)
  }
  loadNamespace("cmdstanr")
})
Sys.setenv(CMDSTANR_USE_RTOOLS = "TRUE",
           RTOOLS44_HOME = "I:/rtools/rtools44")
dir.create("I:/cmdstan", FALSE, TRUE)
cmdstanr::install_cmdstan(dir = "I:/cmdstan",
                          version = {{cmdstan_version}},
                          overwrite = {{overwrite}})
