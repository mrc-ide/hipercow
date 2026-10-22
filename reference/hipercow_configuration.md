# Report on hipercow configuration

Report on your hipercow configuration. We will always want you to post
this along side any problems; it has lots of useful information in it
that will help us see how your set up is configured.

## Usage

``` r
hipercow_configuration(show = TRUE, root = NULL)
```

## Arguments

- show:

  Display the configuration to the screen

- root:

  Hipercow root, usually best `NULL`

## Value

A list with a machine readable form of this information, invisibly.

## Examples

``` r
cleanup <- hipercow_example_helper()
#> ℹ This example uses a special helper
hipercow_configuration()
#> 
#> ── hipercow root at /home/runner/work/_temp/hv-20261022-1a6a3c674e82 ───────────
#> ✔ Working directory '.' within root
#> ℹ R version 4.6.1 on Linux (runner@runnervmfese6)
#> 
#> ── Packages ──
#> 
#> ℹ This is hipercow 1.1.9
#> ℹ Installed: conan2 (1.9.104), logwatch (0.1.1), rrq (0.7.24)
#> ✖ hipercow.dide is not installed
#> 
#> ── Environments ──
#> 
#> ── default 
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── empty 
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── Drivers ──
#> 
#> ✔ 1 driver configured ('example')
#> 
#> ── example 
#> (unconfigurable)

# If you have saved additional environments, they will be listed here:
file.create("functions.R")
#> [1] TRUE
hipercow_environment_create(
  name = "other",
  packages = "knitr",
  sources = "functions.R")
#> ✔ Created environment 'other'
hipercow_configuration()
#> 
#> ── hipercow root at /home/runner/work/_temp/hv-20261022-1a6a3c674e82 ───────────
#> ✔ Working directory '.' within root
#> ℹ R version 4.6.1 on Linux (runner@runnervmfese6)
#> 
#> ── Packages ──
#> 
#> ℹ This is hipercow 1.1.9
#> ℹ Installed: conan2 (1.9.104), logwatch (0.1.1), rrq (0.7.24)
#> ✖ hipercow.dide is not installed
#> 
#> ── Environments ──
#> 
#> ── default 
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── empty 
#> • packages: (none)
#> • sources: (none)
#> • globals: (none)
#> 
#> ── other 
#> • packages: knitr
#> • sources: functions.R
#> • globals: (none)
#> 
#> ── Drivers ──
#> 
#> ✔ 1 driver configured ('example')
#> 
#> ── example 
#> (unconfigurable)

cleanup()
#> ℹ Cleaning up example
```
