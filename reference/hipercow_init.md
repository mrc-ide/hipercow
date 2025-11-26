# Create a hipercow root

Create a hipercow root. This marks the directory where your task
information will be saved, along with a local copy of your R packages (a
"library" for the cluster). Immediately after running this the first
time, you probably want to run
[`hipercow_configure()`](https://mrc-ide.github.io/hipercow/reference/hipercow_configure.md)
in order to control how we set up your projects network paths and R
version.

## Usage

``` r
hipercow_init(root = ".", driver = NULL, ...)
```

## Arguments

- root:

  The path to the root, defaulting the current directory.

- driver:

  Optionally, the name of a driver to configure

- ...:

  Arguments passed through to
  [hipercow_configure](https://mrc-ide.github.io/hipercow/reference/hipercow_configure.md)
  if `driver` is non-NULL.

## Value

Invisibly, the root object

## Examples

``` r
# Create an empty root
path <- withr::local_tempfile()
hipercow_init(path)
#> ✔ Initialised hipercow at '/tmp/RtmpDLa3Fh/file186c52c5a4b9'
#> ℹ Next, call 'hipercow_configure()'
```
