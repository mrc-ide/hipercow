# hermod.windows

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/hermod.windows/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/hermod.windows/actions)
[![Build status]()](https://buildkite.com/mrc-ide/mrcide/hermod-dot-windows?branch=main)
[![codecov.io](https://codecov.io/github/mrc-ide/hermod.windows/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/hermod.windows?branch=main)
<!-- badges: end -->

## Installation

To install `hermod.windows`:

```r
remotes::install_github("mrc-ide/hermod.windows", upgrade = FALSE)
```


## Usage

On a network share

```
hermod::hermod_init(".")
hermod::hermod_configure("windows", r_version = "4.3.1")

hermod::hermod_task_create_explicit(quote(sessionInfo()), submit = "windows")

id <- hermod::hermod_task_create_explicit(quote(sessionInfo()))
hermod::hermod_task_submit(id, "windows")
```

## Creating the temporary library for testing

* Log into a windows host by RDP (or Wes from his desktop)
* Open the most recent version of R you can find there
* Ensure that `T:/hemod-testing` is in fact the library
* Then run:

```r
.libPaths("T:/hermod-testing")
remotes::install_github("mrc-ide/hermod@<branchname>")
```

remotes::install_github("mrc-ide/hermod@prototype-drivers")

## License

MIT © Imperial College of Science, Technology and Medicine
