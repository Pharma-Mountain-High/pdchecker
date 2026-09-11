# Capture and parse output from multiple check functions

Runs multiple check functions, captures their console output, parses it,
and combines the results into a single data frame. This is useful for
batch processing multiple data quality checks.

## Usage

``` r
capture_check_results(..., data = NULL)
```

## Arguments

- ...:

  Named functions to run. Each should be a function that accepts a data
  parameter. The names will be used as check identifiers if check names
  cannot be extracted from the output

- data:

  Data object to pass to each check function. Can be NULL if the
  functions don't use it (optional, defaults to NULL)

## Value

Data frame with combined check results from all functions, with columns
depending on the parsed output structure

## Details

Each function in `...` should accept a single `data` parameter and
should print formatted output to the console. The console output is
captured and parsed using
[`parse_check_output`](https://insightsengineering.github.io/r.pkg.template/reference/parse_check_output.md).

If a function produces no output, the function attempts to use the
return value directly via
[`as_check_df`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md).
If the check name cannot be extracted from the output, the parameter
name is used as the check name.

## See also

[`parse_check_output`](https://insightsengineering.github.io/r.pkg.template/reference/parse_check_output.md),
[`as_check_df`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md),
[`combine_check_results`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)

Other check result utilities:
[`as_check_df()`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md),
[`combine_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md),
[`parse_check_output()`](https://insightsengineering.github.io/r.pkg.template/reference/parse_check_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Define check functions
check_age <- function(data) {
  print("Age check passed")
}
check_gender <- function(data) {
  print("Gender check passed")
}

# Capture results from multiple checks
results <- capture_check_results(
  age_check = check_age,
  gender_check = check_gender,
  data = my_data
)
} # }
```
