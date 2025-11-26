# Provision cluster library

Provision a library. This runs a small task on the cluster to set up
your packages. If you have changed your R version you will need to rerun
this. See
[`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md)
for much more on this process.

## Usage

``` r
hipercow_provision(
  method = NULL,
  ...,
  driver = NULL,
  environment = "default",
  check_running_tasks = TRUE,
  root = NULL
)
```

## Arguments

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

- driver:

  The name of the driver to use, or you can leave blank if only one is
  configured (this will be typical).

- environment:

  The name of the environment to provision (see
  [hipercow_environment_create](https://mrc-ide.github.io/hipercow/reference/hipercow_environment.md)
  for details).

- check_running_tasks:

  Logical, indicating if we should check that no tasks are running
  before starting installation. Generally, installing packages while
  tasks are running is harmful as you may get unexpected results, a task
  may start while a package is in an inconsistent state, and on windows
  you may get a corrupted library if a package is upgraded while it is
  loaded. You can disable this check by passing `FALSE`. Not all drivers
  respond to this argument, but the windows driver does.

- root:

  The hipercow root

## Value

Nothing

## Details

Our hope is that that most of the time you will not need to pass any
options through `...`, and that most of the time hipercow will do the
right thing. Please let us know if that is not the case and you're
having to routinely add arguments here.

## Manually adding packages to an installation

One case where we do expect that you will pass options through to
`hipercow_provision` is where you are manually adding packages to an
existing library. The usage here will typically look like:

    hipercow_provision("pkgdepends", refs = c("pkg1", "pkg2"))

where `pkg1` and `pkg2` are names of packages or pkgdepends references
(e.g., `username/repo` for a GitHub package; see
[`vignette("packages")`](https://mrc-ide.github.io/hipercow/articles/packages.md)
for details).

## Supported methods and options

There are four possible methods: `pkgdepends`, `auto`, `script` and
`renv`.

The canonical source of documentation for all of these approaches is
[`conan2::conan_configure`](https://mrc-ide.github.io/conan2/reference/conan_configure.html).

### `pkgdepends`

The simplest method to understand, and probably most similar to the
approach in `didehpc`. This method installs packages from a list in
`pkgdepends.txt` in your hipercow root, or via a vector of provided
package references. Uses [pkgdepends](https://pkgdepends.r-lib.org) for
the actual dependency resolution and installation.

Supported options (passed via `...`)

- `refs`: A character vector of package references to override
  `pkgdepends.txt`

- `policy`: the policy argument to
  [`pkgdepends::new_pkg_installation_proposal`](https://r-lib.github.io/pkgdepends/reference/pkg_installation_proposal.html)
  (accepts `lazy` and `upgrade`)

### `auto`

Uses `pkgdepends` internally but tries to do everything automatically
based on your declared environments (see `hipercow_environment_create`
and
[`vignette("hipercow")`](https://mrc-ide.github.io/hipercow/articles/hipercow.md))
and the installation information recorded in the locally installed
versions of the required packages.

This is experimental and we'd love to know how it works for you.

No options are supported, the idea is it's automatic :)

### `script`

Runs a script (by default `provision.R`) on the cluster to install
things however you want. Very flexible but you're on your own mostly.
The intended use case of this option is where `pkgdepends` fails to
resolve your dependencies properly and you need to install things
manually. The `remotes` package will be pre-installed for you to use
within your script.

Your script will run on a special build queue, which will run even when
the cluster is very busy. However, this is restricted in other ways,
allowing a maximum of 30 minutes and disallowing parallel running.

Supports one option:

- `script`: The path for the script to run, defaulting to `provision.R`

### `renv`

Uses [`renv`](https://rstudio.github.io/renv) to recreate your renv
environment. You must be using `renv` locally for this to work, and at
present your renv project root must be the same as your hipercow root.

No options are currently supported, but we may pass some renv options in
the future; if you need more flexibility here please let us know.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
writeLines(c("knitr", "data.table"), "pkgdepends.txt")
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
#> Running in path: /home/runner/work/_temp/hv-20251126-186c3e973b04
#> Library paths:
#>   - /home/runner/work/_temp/hv-20251126-186c3e973b04/hipercow/lib
#>   - /opt/R/4.5.2/lib/R/site-library
#>   - /opt/R/4.5.2/lib/R/library
#> id: 20251126125403
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
#> • data.table
#> 
#> ✔ Updated metadata database: 4.81 MB in 9 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> + data.table   1.17.8 [bld][cmp][dl]
#> + evaluate     1.0.5  [bld][dl]
#> + highr        0.11   [bld][dl]
#> + knitr        1.50   [bld][dl]
#> + xfun         0.54   [bld][cmp][dl]
#> + yaml         2.3.10 [bld][cmp][dl]
#> ℹ Getting 6 pkgs with unknown sizes
#> ✔ Got highr 0.11 (source) (13.85 kB)
#> ✔ Got evaluate 1.0.5 (source) (39.32 kB)
#> ✔ Got xfun 0.54 (source) (169.16 kB)
#> ✔ Got knitr 1.50 (source) (534.26 kB)
#> ✔ Got yaml 2.3.10 (source) (94.56 kB)
#> ✔ Got data.table 1.17.8 (source) (5.81 MB)
#> ℹ Building evaluate 1.0.5
#> ℹ Building xfun 0.54
#> ℹ Building yaml 2.3.10
#> ℹ Building data.table 1.17.8
#> ✔ Built evaluate 1.0.5 (1.9s)
#> ✔ Installed evaluate 1.0.5  (30ms)
#> ✔ Built xfun 0.54 (5.7s)
#> ✔ Installed xfun 0.54  (21ms)
#> ℹ Building highr 0.11
#> ✔ Built yaml 2.3.10 (6s)
#> ✔ Installed yaml 2.3.10  (127ms)
#> ✔ Built highr 0.11 (1s)
#> ✔ Installed highr 0.11  (16ms)
#> ℹ Building knitr 1.50
#> ✔ Built knitr 1.50 (4.7s)
#> ✔ Installed knitr 1.50  (39ms)
#> ✔ Built data.table 1.17.8 (21.9s)
#> ✔ Installed data.table 1.17.8  (64ms)
#> ✔ Summary:   6 new  in 41.6s
#> 
#> -------------------------------------------------------------------------------
#> Writing library description to 'hipercow/lib/.conan/20251126125403'
#> Done!
hipercow_provision_list()
#> ℹ 1 conan installation recorded
#> • 1: 20251126125403 (moments ago) [0]

cleanup()
#> ℹ Cleaning up example
```
