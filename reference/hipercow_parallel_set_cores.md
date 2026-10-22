# Set various environment variables that report the number of cores available for execution.

Sets the environment variables `MC_CORES`, `OMP_NUM_THREADS`,
`OMP_THREAD_LIMIT`, `R_DATATABLE_NUM_THREADS` and `HIPERCOW_CORES` to
the given number of cores. This is used to help various thread-capable
packages use the correct number of cores. You can also call it yourself
if you know specifically how many cores you want to be available to code
that looks up these environment variables.

## Usage

``` r
hipercow_parallel_set_cores(cores, envir = NULL)
```

## Arguments

- cores:

  Number of cores to be used.

- envir:

  Environment in which the variables will be set to limit their
  lifetime. This should not need setting in general, but see
  [`withr::local_envvar`](https://withr.r-lib.org/reference/with_envvar.html)
  for example use.
