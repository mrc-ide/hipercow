# Remove a driver from a hipercow configuration

Remove a driver configured by
[hipercow_configure](https://mrc-ide.github.io/hipercow/reference/hipercow_configure.md).
This will not affect tasks already submitted with this driver, but will
prevent any future tasks being submitted with it.

## Usage

``` r
hipercow_unconfigure(driver, root = NULL)
```

## Arguments

- driver:

  The name of the driver to remove

- root:

  Hipercow root, usually best `NULL`

## Value

Nothing, called for its side effects only.

## See also

[hipercow_configuration](https://mrc-ide.github.io/hipercow/reference/hipercow_configuration.md),
which shows currently enabled drivers.
