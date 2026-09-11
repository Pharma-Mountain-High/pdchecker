# Get pdchecker Global Options

Display the current values of all pdchecker global options.

## Usage

``` r
get_pdchecker_options()
```

## Value

A named list of current pdchecker option values. Options that have not
been set will show their default values.

## See also

[`set_pdchecker_options()`](https://insightsengineering.github.io/r.pkg.template/reference/set_pdchecker_options.md)
for setting option values

Other options:
[`set_pdchecker_options()`](https://insightsengineering.github.io/r.pkg.template/reference/set_pdchecker_options.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# View current options
get_pdchecker_options()

# Set some options and verify
set_pdchecker_options(sv_dataset = "SV", ex_datasets = c("EC"))
get_pdchecker_options()
} # }
```
