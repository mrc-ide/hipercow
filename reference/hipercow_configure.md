# Configure your hipercow root

Configure your hipercow root. `hipercow_configure` creates the
configuration and `hipercow_configuration` looks it up

## Usage

``` r
hipercow_configure(driver, ..., root = NULL)
```

## Arguments

- driver:

  The hipercow driver; we support two at present: `"dide-windows"` and
  `"dide-linux"`.

- ...:

  Arguments passed to your driver; see Details for information about
  what is supported (this varies by driver).

- root:

  Hipercow root, usually best `NULL`

## DIDE Cluster - Windows nodes

Options supported by the `dide-windows` driver:

- `shares`: Information about shares (additional to the one mounted as
  your working directory) that should be made available to the cluster
  job. The use case here is where you need access to some files that are
  present on a shared drive and you will access these by absolute path
  (say `M:/gis/shapefiles/`) from your tasks. You can provide a share as
  a `windows_path` object, or a list of such objects. You will not
  typically need to use this option.

- `r_version`: Control the R version used on the cluster. Typically
  hipercow will choose a version close to the one you are using to
  submit jobs, of the set available on the cluster. You can use this
  option to choose a specific version (e.g., pass "4.3.0" to select
  exactly that version).

- `redis_url`: Control the URL used to connect to Redis. You can use
  this to use an alternative Redis host, which is unlikely unless we
  have suggested you do this. The default (`NULL`) uses the Redis server
  on the headnode.

See
[`vignette("details")`](https://mrc-ide.github.io/hipercow/articles/details.md)
for more information about these options.

## See also

[hipercow_unconfigure](https://mrc-ide.github.io/hipercow/reference/hipercow_unconfigure.md),
which removes a driver

## Examples

``` r
if (FALSE) {
hipercow_configure("dide-windows", r_version = "4.3.0")
}
```
