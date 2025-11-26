# Compare installations

Compare installations performed into your libraries by conan.

## Usage

``` r
hipercow_provision_compare(curr = 0, prev = -1, driver = NULL, root = NULL)
```

## Arguments

- curr:

  The previous installation to compare against. Can be a name (see
  [hipercow_provision_list](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  to get names), a negative number where `-n` indicates "`n`
  installations ago" or a positive number where `n` indicates "the `n`th
  installation". The default value of 0 corresponds to the current
  installation.

- prev:

  The previous installation to compare against. Can be a name (see
  [hipercow_provision_list](https://mrc-ide.github.io/hipercow/reference/hipercow_provision_list.md)
  to get names), a negative number where `-n` indicates "`n`
  installations ago" or a positive number where `n` indicates "the `n`th
  installation". The default of -1 indicates the previous installation.
  Must refer to an installation before `curr`. Use `NULL` or `-Inf` if
  you want to compare against the empty installation.

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- root:

  The hipercow root

## Value

An object of class `conan_compare`, which can be printed nicely.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_provision("pkgdepends", refs = "knitr")
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20251126-186c3380109f
#> Library paths:
#>   - /home/runner/work/_temp/hv-20251126-186c3380109f/hipercow/lib
#>   - /opt/R/4.5.2/lib/R/site-library
#>   - /opt/R/4.5.2/lib/R/library
#> id: 20251126125436
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
#> 
#> ✔ Updated metadata database: 4.81 MB in 9 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + evaluate   1.0.5  [bld][dl]
#> + highr      0.11   [bld][dl]
#> + knitr      1.50   [bld][dl]
#> + xfun       0.54   [bld][cmp][dl]
#> + yaml       2.3.10 [bld][cmp][dl]
#> ℹ Getting 5 pkgs with unknown sizes
#> ✔ Got highr 0.11 (source) (13.85 kB)
#> ✔ Got evaluate 1.0.5 (source) (39.32 kB)
#> ✔ Got yaml 2.3.10 (source) (94.56 kB)
#> ✔ Got xfun 0.54 (source) (169.16 kB)
#> ✔ Got knitr 1.50 (source) (534.26 kB)
#> ℹ Building evaluate 1.0.5
#> ℹ Building xfun 0.54
#> ℹ Building yaml 2.3.10
#> ✔ Built evaluate 1.0.5 (1.9s)
#> ✔ Installed evaluate 1.0.5  (35ms)
#> ✔ Built xfun 0.54 (4.1s)
#> ✔ Installed xfun 0.54  (20ms)
#> ℹ Building highr 0.11
#> ✔ Built yaml 2.3.10 (5.1s)
#> ✔ Installed yaml 2.3.10  (18ms)
#> ✔ Built highr 0.11 (1.1s)
#> ✔ Installed highr 0.11  (1s)
#> ℹ Building knitr 1.50
#> ✔ Built knitr 1.50 (4.3s)
#> ✔ Installed knitr 1.50  (35ms)
#> ✔ Summary:   5 new  in 17.7s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/.conan/20251126125436'
#> Done!
hipercow_provision("pkgdepends", refs = "data.table")
#> /`-'\  _______  ___  ___ ____
#> \,T./ / __/ _ \/ _ \/ _ `/ _ \
#>   |   \__/\___/_//_/\_,_/_//_/
#>   |   ---- THE  LIBRARIAN ----
#> 
#> Bootstrapping from: /home/runner/work/_temp/Library
#> Installing into library: hipercow/lib
#> Using method pkgdepends
#> Running in path: /home/runner/work/_temp/hv-20251126-186c3380109f
#> Library paths:
#>   - /home/runner/work/_temp/hv-20251126-186c3380109f/hipercow/lib
#>   - /opt/R/4.5.2/lib/R/site-library
#>   - /opt/R/4.5.2/lib/R/library
#> id: 20251126125457
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
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> + data.table   1.17.8 [bld][cmp]
#> ℹ No downloads are needed, 1 pkg is cached
#> ✔ Got data.table 1.17.8 (source) (5.81 MB)
#> ℹ Building data.table 1.17.8
#> ✔ Built data.table 1.17.8 (19s)
#> ✔ Installed data.table 1.17.8  (68ms)
#> ✔ Summary:   1 new  in 19.1s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/.conan/20251126125457'
#> Done!
hipercow_provision_compare()
#> ── Comparing conan installations ───────────────────────────────────────────────
#> • 20251126125436 1st; previous installation (less than a minute ago)
#> • 20251126125457 2nd; current installation (moments ago)
#> 
#> ── 5 unchanged packages ──
#> 
#> ℹ To show unchanged packages, print with 'show_unchanged = TRUE'
#> 
#> ── 1 added package ──
#> 
#> • data.table (1.17.8) CRAN

cleanup()
#> ℹ Cleaning up example
```
