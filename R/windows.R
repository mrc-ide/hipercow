##' Register DIDE windows credentials.
##'
##' In order to be able to communicate with the Windows DIDE HPC
##' system, we need to be able to communicate with the HPC portal
##' (<https::mrcdata.dide.ic.ac.uk/hpc>), and for this we need your
##' **DIDE** password and username. This is typically, but not always,
##' the same as your Imperial credentials.  We store this information
##' securely using the [keyring](https://keyring.r-lib.org/) package,
##' so when unlocking your credentials you will be prompted for your
##' **computer** password, which might differ from either your DIDE or
##' Imperial password if you are not on a windows machine.
##'
##' @title DIDE windows credentials
##'
##' @return Nothing, these functions are called for their side effects.
##'
##' @export
windows_credentials <- function() {
  ensure_package("hermod.windows")$dide_credentials()
}
