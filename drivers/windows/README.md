# hipercow.windows

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/hipercow.windows/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/hipercow.windows/actions)
[![Build status]()](https://buildkite.com/mrc-ide/mrcide/hipercow-dot-windows?branch=main)
[![codecov.io](https://codecov.io/github/mrc-ide/hipercow.windows/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/hipercow.windows?branch=main)
<!-- badges: end -->

## Installation

To install `hipercow.windows`:

```r
remotes::install_github("mrc-ide/hipercow.windows", upgrade = FALSE)
```


## Usage

On a network share

```
hipercow::hipercow_init(".")
hipercow::hipercow_configure("windows", r_version = "4.3.1")
id <- hipercow::task_create_expr(quote(sessionInfo()))
hipercow::task_submit(id, "windows")
```

## Creating the temporary library for testing

* Log into a windows host by RDP (or Wes from his desktop)
* Open the most recent version of R you can find there
* Ensure that `T:/hemod-testing` is in fact the library
* Then run:

```r
.libPaths("T:/hipercow-testing")
remotes::install_github("mrc-ide/hipercow@<branchname>")
```

remotes::install_github("mrc-ide/hipercow@prototype-drivers")

## Creation of bootstrap libraries

We need a few things available in libraries on the temp (or other) for this to work. Eventually we'll do this on a share that we (RESIDE) can write to, and everyone (DIDE) can read from, as that will prevent any unfortunate issues with updating files. We'll never "update" these, only create new ones.

The bootstrap libraries will be stored as

```
<some path>/<r version>/<id>/{hipercow,ids,remotes,...}
```

with 'id' being some identifier, possibly an integer. To find the current id, we read the file `<some path>/<r version>/current` which contains the id. Each time we create a new library we create a new id, written only at the point where the installation has completed. Each time we launch job (the first time in the session) we'll read the id to get the current bootstrap path.

Boring maintenance job would be to go through and remove all but (say) the last two every month or so, or just do that after creation of a new library.

These can be safely run as cluster jobs via hipercow.

## License

MIT © Imperial College of Science, Technology and Medicine
