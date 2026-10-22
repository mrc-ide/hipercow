# Load an environment in a parallel context

Load an environment into a parallel worker. This is a helper function
designed for use from parallel backends, and is exported mostly because
it makes it easier for us to work with. Users should never need to call
this function directly.

## Usage

``` r
hipercow_parallel_load_environment(name, envir = .GlobalEnv)
```

## Arguments

- envir:

  The name of the environment

## Value

Nothing, called for side effects only
