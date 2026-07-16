# Check if elements could be SAS missing data variants

Missing data from sas7bdat imported datasets could be imported in
different ways. This function checks for 'NA', NA, '.', and ” (empty
string).

## Usage

``` r
is_sas_na(x)
```

## Arguments

- x:

  Vector with data to check

## Value

Logical vector indicating which elements are SAS missing values

## Note

The following values are considered SAS missing:

- R's NA values

- The string "NA"

- A single period "."

- Empty strings "" (after trimming whitespace)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check for SAS missing values
x <- c("1", ".", "", NA, "NA", "valid")
is_sas_na(x)
# Returns: FALSE, TRUE, TRUE, TRUE, TRUE, FALSE
} # }
```
