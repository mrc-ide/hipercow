##' Register DIDE windows credentials.
##'
##' In order to be able to communicate with the Windows DIDE HPC
##' system, we need to be able to communicate with the HPC portal
##' (<https::mrcdata.dide.ic.ac.uk/hpc>), and for this we need your
##' **DIDE** password and username. This is typically, but not always,
##' the same as your Imperial credentials.  We store this information
##' securely using the [keyring](https://keyring.r-lib.org/) package,
##' so when unlocking your credentials you will be prompted for your
##' **computer** password, which will be your DIDE password if you use a windows
##' machine connected to the DIDE domain, but will likely differ from either your DIDE or
##' Imperial password if you are outside the DIDE domain, or if you don't use Windows.
##'
##' @title DIDE windows credentials
##'
##' @return Nothing, these functions are called for their side effects.
##'
##' @export
windows_credentials <- function() {
  ns <- ensure_package("hermod.windows", rlang::current_env())
  ns$dide_credentials()
}


##' Describe a path mapping for use when setting up jobs on the cluster.
##' @title Describe a path mapping
##'
##' @param name Name of this map.  Can be anything at all, and is used
##'   for information purposes only.
##'
##' @param path_local The point where the drive is attached locally.
##'   On Windows this will be something like "Q:/", on Mac something
##'   like "/Volumes/mountname", and on Linux it could be anything at
##'   all, depending on what you used when you mounted it (or what is
##'   written in `/etc/fstab`)
##'
##' @param path_remote The network path for this drive.  It
##'   will look something like `\\\\fi--didef3.dide.ic.ac.uk\\tmp\\`.
##'   Unfortunately backslashes are really hard to get right here and
##'   you will need to use twice as many as you expect (so *four*
##'   backslashes at the beginning and then two for each separator).
##'   If this makes you feel bad know that you are not alone:
##'   https://xkcd.com/1638 -- alternatively you may use forward
##'   slashes in place of backslashes (e.g. `//fi--didef3.dide.ic.ac.uk/tmp`)
##'
##' @param drive_remote The place to mount the drive on the cluster.
##'   We're probably going to mount things at Q: and T: already so
##'   don't use those.  And things like C: are likely to be used.
##'   Perhaps there are some guidelines for this somewhere?
##'
##' @export
##' @author Rich FitzJohn
windows_path <- function(name, path_local, path_remote, drive_remote) {
  ns <- ensure_package("hermod.windows", rlang::current_env())
  ns$windows_path(name, path_local,path_remote, drive_remote)
}
