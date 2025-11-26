# List installations

List previous successful installations of this hipercow root.

## Usage

``` r
hipercow_provision_list(driver = NULL, root = NULL)

hipercow_provision_check(
  method = NULL,
  ...,
  driver = NULL,
  environment = "default",
  root = NULL
)
```

## Arguments

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- root:

  The hipercow root

- method:

  The provisioning method to use, defaulting to `NULL`, which indicates
  we should try and detect the best provisioning mechanism for you; this
  should typically work well unless you are manually adding packages
  into your library (see Details). If given, must be one of `auto`,
  `pkgdepends`, `script` or `renv`; each of these are described in the
  Details and in
  [`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md).

- ...:

  Arguments passed through to conan. See Details.

- environment:

  The name of the environment to provision (see
  [hipercow_environment_create](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  for details).

## Value

A [data.frame](https://rdrr.io/r/base/data.frame.html) with columns:

- `name`: the name of the installation. This might be useful with
  `conan_compare`

- `time`: the time the installation was started

- `hash`: the installation hash

- `method`: the method used for the installation

- `args`: the arguments to the installation (as a list column)

- `current`: if using `hipercow_provision_check`, does this installation
  match the arguments provided?

This object also has class `conan_list` so that it prints nicely, but
you can drop this with `as.data.frame`.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
writeLines("data.table", "pkgdepends.txt")

# Before any installation has happened:
hipercow_provision_list()
#> ! No conan installations recorded
hipercow_provision_check()
#> ✔ Selected provisioning method 'pkgdepends'
#> ! No conan installations recorded

# After installation:
hipercow_provision()
#> ✔ Selected provisioning method 'pkgdepends'
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20251126-186c424af8d2
#> Library paths:
#>   - /home/runner/work/_temp/hv-20251126-186c424af8d2/hipercow/lib
#>   - /opt/R/4.5.2/lib/R/site-library
#>   - /opt/R/4.5.2/lib/R/library
#> id: 20251126125521
#> Logs from pkgdepends follow:
#> 
#> -------------------------------------------------------------------------------
#> 
#> 
#> ── repos 
#> • https://cloud.r-project.org
#> 
#> ── refs 
#> • data.table
#> 
#> ✔ Updated metadata database: 4.81 MB in 9 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + data.table   1.17.8 [bld][cmp][dl]
#> ℹ Getting 1 pkg with unknown size
#> ✔ Got data.table 1.17.8 (source) (5.81 MB)
#> ℹ Building data.table 1.17.8
#> ✔ Built data.table 1.17.8 (18.9s)
#> ✔ Installed data.table 1.17.8  (105ms)
#> ✔ Summary:   1 new  in 19s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/.conan/20251126125521'
#> Done!
hipercow_provision_list()
#> ℹ 1 conan installation recorded
#> • 1: 20251126125521 (moments ago) [0]
hipercow_provision_check()
#> ✔ Selected provisioning method 'pkgdepends'
#> ℹ 1 conan installation recorded
#> • 1: 20251126125521 (moments ago) [0] (*)
#> ℹ The entry marked with '*' matches the provided installation hash

# After a different installation:
hipercow_provision("pkgdepends", refs = "knitr")
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20251126-186c424af8d2
#> Library paths:
#>   - /home/runner/work/_temp/hv-20251126-186c424af8d2/hipercow/lib
#>   - /opt/R/4.5.2/lib/R/site-library
#>   - /opt/R/4.5.2/lib/R/library
#> id: 20251126125550
#> Logs from pkgdepends follow:
#> 
#> -------------------------------------------------------------------------------
#> 
#> 
#> ── repos 
#> • https://cloud.r-project.org
#> 
#> ── refs 
#> • knitr
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> + evaluate   1.0.5  [bld]
#> + highr      0.11   [bld]
#> + knitr      1.50   [bld]
#> + xfun       0.54   [bld][cmp]
#> + yaml       2.3.10 [bld][cmp]
#> ℹ No downloads are needed, 5 pkgs are cached
#> ✔ Got evaluate 1.0.5 (source) (39.32 kB)
#> ✔ Got highr 0.11 (source) (13.85 kB)
#> ✔ Got yaml 2.3.10 (source) (94.56 kB)
#> ✔ Got xfun 0.54 (source) (169.16 kB)
#> ✔ Got knitr 1.50 (source) (534.26 kB)
#> ℹ Building evaluate 1.0.5
#> ℹ Building xfun 0.54
#> ℹ Building yaml 2.3.10
#> ✔ Built evaluate 1.0.5 (1.7s)
#> ✔ Installed evaluate 1.0.5  (22ms)
#> ✔ Built xfun 0.54 (4.1s)
#> ✔ Installed xfun 0.54  (55ms)
#> ℹ Building highr 0.11
#> ✔ Built yaml 2.3.10 (5.1s)
#> ✔ Installed yaml 2.3.10  (20ms)
#> ✔ Built highr 0.11 (1s)
#> ✔ Installed highr 0.11  (1s)
#> ℹ Building knitr 1.50
#> ✔ Built knitr 1.50 (4.2s)
#> ✔ Installed knitr 1.50  (37ms)
#> ✔ Summary:   5 new  in 17.3s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/.conan/20251126125550'
#> Done!
hipercow_provision_list()
#> ℹ 2 conan installations recorded
#> • 1: 20251126125521 (less than a minute ago) [-1]
#> • 2: 20251126125550 (moments ago) [0]
hipercow_provision_check()
#> ✔ Selected provisioning method 'pkgdepends'
#> ℹ 2 conan installations recorded
#> • 1: 20251126125521 (less than a minute ago) [-1] (*)
#> • 2: 20251126125550 (moments ago) [0]
#> ℹ The entry marked with '*' matches the provided installation hash

cleanup()
#> ℹ Cleaning up example
```
