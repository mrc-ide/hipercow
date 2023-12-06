script <- 'install.packages("dplyr")'

script <- 'remotes::install_github("mrc-ide/whatever")'

script <- 'install.packages("myfile.tar.gz", repos = NULL)'


script <- c(
  'options(repos = c(CRAN = "https://cloud.r-project.org", other = "whatever"))',
  'install.packages(c("a", "b"))'
  'library(a)',
  'remotes::install_github("a/b")')
