# Hello world

Hello world in hipercow. This function sends a tiny test task through
the whole system to confirm that everything is configured correctly.

## Usage

``` r
hipercow_hello(progress = NULL, timeout = NULL, driver = NULL)
```

## Arguments

- progress:

  Logical value, indicating if a progress spinner should be used. The
  default `NULL` uses the option `hipercow.progress`, and if unset
  displays a progress bar in an interactive session.

- timeout:

  The time to wait for the task to complete. The default is to wait
  forever.

- driver:

  The driver to use to send the test task. This can be omitted where you
  have exactly one driver, but we error if not given when you have more
  than one driver, or if you have not configured any drivers.

## Value

The string "Moo", direct from your cluster.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_hello()
#> ✔ Submitted task '9e7c2b4e16cb37cecf0d81d78d7b6d45' using 'example'
#> ✔ Successfully ran test task '9e7c2b4e16cb37cecf0d81d78d7b6d45'

cleanup()
#> ℹ Cleaning up example
```
