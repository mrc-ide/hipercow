# Environment variables

Create environment variables for use with a hipercow task.

## Usage

``` r
hipercow_envvars(..., secret = FALSE)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Named environment variable. If unnamed, it is assumed to refer to an
  environment variable that exists. Use an `NA` value to unset an
  environment variable.

- secret:

  Are these environment variables secret? If so we will encrypt them at
  saving and decrypt on use.

## Value

A list with class `hipercow_envvars` which should not be modified.

## Examples

``` r
# Declare environment variables as key-value pairs:
hipercow_envvars("MY_ENVVAR1" = "value1", "MY_ENVVAR2" = "value2")
#>         name  value secret
#> 1 MY_ENVVAR1 value1  FALSE
#> 2 MY_ENVVAR2 value2  FALSE

# If an environment variable already exists in your environment
# and you want to duplicate this into a task, you can use this
# shorthand:
Sys.setenv(HIPERCOW_EXAMPLE_ENVVAR = "moo") # suppose this exists already
hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR")
#>                      name value secret
#> 1 HIPERCOW_EXAMPLE_ENVVAR   moo  FALSE
hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR", ANOTHER_ENVVAR = "value")
#>                      name value secret
#> 1 HIPERCOW_EXAMPLE_ENVVAR   moo  FALSE
#> 2          ANOTHER_ENVVAR value  FALSE

# Secret envvars are still printed (at the moment at least) but
# once passed into a task they will be encrypted at rest.
hipercow_envvars("MY_SECRET" = "password", secret = TRUE)
#>        name    value secret
#> 1 MY_SECRET password   TRUE

# Secret and public environment variables should be created
# separately and concatenated together:
env_public <- hipercow_envvars("HIPERCOW_EXAMPLE_ENVVAR")
env_secret <- hipercow_envvars("MY_PASSWORD" = "secret", secret = TRUE)
c(env_public, env_secret)
#>                      name  value secret
#> 1 HIPERCOW_EXAMPLE_ENVVAR    moo  FALSE
#> 2             MY_PASSWORD secret   TRUE

# Cleanup
Sys.unsetenv("HIPERCOW_EXAMPLE_ENVVAR")
```
