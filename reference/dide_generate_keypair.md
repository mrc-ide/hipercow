# Generate keypair

Generate or delete a keypair for encrypting small data to send to the
DIDE cluster. This can be used to encrypt environment variables, and
possibly other workflows in future involving relatively small amounts of
data. By default, if you have ever created a keypair we do not replace
it if it already exists, unless you set `update = TRUE` so you may call
this function safely to ensure that you do have a keypair set up.

## Usage

``` r
dide_generate_keypair(update = FALSE)

dide_delete_keypair()
```

## Arguments

- update:

  Replace the existing keypair. You will need to use this if you
  accidentally remove the `.hipercow/` directory from your network home
  share, or if you want to renew your key.

## Value

Nothing, called for its side effect

## Warning

If you run `dide_delete_keypair()` then your keys are deleted
(naturally), but this means that any data encrypted with these keys will
be impossible to read. Usually this is what you want, but be warned
there is no going back and no confirmation prompt.

## Examples

``` r
if (FALSE) {

# Generate a new keypair, if one does not exist
dide_generate_keypair()
}
```
