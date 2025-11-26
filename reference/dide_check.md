# Check we can use the DIDE cluster

Perform some basic checks to make that your system is configured to use
the DIDE cluster properly. Calling this when something goes wrong is
never a bad idea.

## Usage

``` r
dide_check(path = getwd())
```

## Arguments

- path:

  Path to check; typically this will be your working directory.

## Value

Invisibly, a logical; `TRUE` if all checks succeed and `FALSE`
otherwise.

## Examples

``` r
if (FALSE) {

dide_check()
}
```
