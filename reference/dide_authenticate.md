# DIDE credentials

Register DIDE credentials.

## Usage

``` r
dide_authenticate()
```

## Value

Nothing, this function is called for its side effect of setting or
updating your credentials within the keyring.

## Details

In order to be able to communicate with the DIDE HPC system, we need to
be able to communicate with the HPC portal
(<https::mrcdata.dide.ic.ac.uk/hpc>), and for this we need your **DIDE**
password and username. This is typically, but not always, the same as
your Imperial credentials. We store this information securely using the
[keyring](https://keyring.r-lib.org/) package, so when unlocking your
credentials you will be prompted for your **computer** password, which
will be your DIDE password if you use a windows machine connected to the
DIDE domain, but will likely differ from either your DIDE or Imperial
password if you are outside the DIDE domain, or if you don't use
Windows.

## Examples

``` r
if (FALSE) {

dide_authenticate()
}
```
