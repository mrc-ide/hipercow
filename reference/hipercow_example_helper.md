# Example helper

A helper used in running examples and docs. This function will change
your working directory into a new temporary hipercow root, and start a
worker process that will quietly run tasks. This is not intended for
people to use outside of running examples!

## Usage

``` r
hipercow_example_helper(
  runner = TRUE,
  with_logging = FALSE,
  new_directory = TRUE,
  initialise = TRUE
)
```

## Arguments

- runner:

  Start a runner? If `TRUE` (the default) we start a background process
  with [`callr::r_bg`](https://callr.r-lib.org/reference/r_bg.html) that
  will pick tasks off a queue and run them.

- with_logging:

  Run each task with logging; this is quite a bit slower, but enables
  examples that use
  [task_log_show](https://mrc-ide.github.io/hipercow/reference/task_log.md)
  etc. Only has an effect if `runner` is `TRUE`.

- new_directory:

  Create new empty (temporary) directory? If `FALSE` then we just use
  the current directory. This is used in the vignettes where the
  directory is set already.

- initialise:

  Initialise? If `FALSE` then no initialisation is done. This is
  intended for examples that will use
  [`hipercow_init()`](https://mrc-ide.github.io/hipercow/reference/hipercow_init.md)
  later.

## Value

A function that can be called (with no arguments) to return to the
original working directory and clean up all files created for the
example.
