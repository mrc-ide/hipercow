# hipercow <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/mrc-ide/hipercow/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/hipercow/actions)
[![codecov](https://codecov.io/github/mrc-ide/hipercow/graph/badge.svg?token=jeEaIEwE8P)](https://codecov.io/github/mrc-ide/hipercow)
![Moo](https://img.shields.io/badge/hipercow-says%20moo-pink?logo=HappyCow&logoColor=white)
<!-- badges: end -->

**NOTICE**: This will only be of use to people at DIDE, as it uses our [cluster web portal](https://mrcdata.dide.ic.ac.uk/hpc), local cluster, and local network file systems.

## What is this?

This is a package for interfacing with the DIDE cluster directly from R.  It is meant to make jobs running on the cluster appear as if they are running locally but asynchronously.  The idea is to let the cluster appear as an extension of your own computer so you can get using it within an R project easily.

This package supercedes didehpc (2015-2023).

## Documentation

* New to this? [The main vignette](https://mrc-ide.github.io/hipercow/articles/hipercow.html) contains full instructions and explanations about why some bits are needed.
* Trying to install packages on the cluster? Check the [packages vignette](https://mrc-ide.github.io/hipercow/articles/packages.html) for ways of controlling this.
* Having problems? Check the [troubleshooting guide](https://mrc-ide.github.io/hipercow/articles/troubleshooting.html).
* Check that everything is ok to go:
  - [DIDE Cluster (wpia-hn)](https://mrc-ide.github.io/hipercow/articles/dide-cluster.html#does-it-work)
* [All long-form documentation](https://mrc-ide.github.io/hipercow/articles/), available on the hipercow website
* [Reference documentation for each function](https://mrc-ide.github.io/hipercow/reference/index.html)

## Issues

* Check the [issue tracker](https://github.com/mrc-ide/hipercow/issues) for known problems, or to create a new one
* Use the "Cluster" channel on Teams, which Rich and Wes keep an eye on

## Installation

To install `hipercow`:

```r
install.packages(
  "hipercow",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

## License

MIT © Imperial College of Science, Technology and Medicine
