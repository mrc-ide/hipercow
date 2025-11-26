# Describe a path mapping

Describe a path mapping for use when setting up jobs on the cluster.

## Usage

``` r
dide_path(path_local, path_remote, drive_remote, call = NULL)
```

## Arguments

- path_local:

  The point where the drive is attached locally. On Windows this will be
  something like "Q:/", on Mac something like "/Volumes/mountname", and
  on Linux it could be anything at all, depending on what you used when
  you mounted it (or what is written in `/etc/fstab`)

- path_remote:

  The network path for this drive. It will look something like
  `\\\\projects.dide.ic.ac.uk\\tmp\\`. Unfortunately backslashes are
  really hard to get right here and you will need to use twice as many
  as you expect (so *four* backslashes at the beginning and then two for
  each separator). If this makes you feel bad know that you are not
  alone: https://xkcd.com/1638 – alternatively you may use forward
  slashes in place of backslashes (e.g. `//projects.dide.ic.ac.uk/tmp`)

- drive_remote:

  The place to mount the drive on the cluster. We're probably going to
  mount things at Q: and T: already so don't use those. And things like
  C: are likely to be used. Perhaps there are some guidelines for this
  somewhere?

- call:

  The name of the calling function, for error reporting.

## Examples

``` r
if (FALSE) {

# Suppose you have mounted your malaria share at "~/net/malaria"
# (e.g., on a Linux machine).  You can tell the cluster to mount
# this as "M:" when running tasks by first creating a path
# mapping:
share <- dide_path("~/net/malaria",
                   "//wpia-hn.hpc.dide.ic.ac.uk/Malaria",
                   "M:")

# This share object contains information about how to relate your
# local and remote paths:
share

# When configuring the cluster you might pass this:
hipercow_configure("dide-windows", shares = share)
}
```
