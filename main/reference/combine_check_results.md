# Combine multiple check results into a single data frame

Combines multiple check result objects into a single data frame by
converting each result using
[`as_check_df`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md)
and then row-binding them together. NULL results are automatically
skipped.

## Usage

``` r
combine_check_results(...)
```

## Arguments

- ...:

  Multiple check result objects (lists) to combine. NULL values are
  ignored

## Value

Data frame with combined check results. Each row represents either a
check summary or a detailed deviation record, depending on the input
check results

## Details

This is a convenience function for aggregating results from multiple
data quality checks. Each check result is converted to a standardized
data frame format before combining, ensuring consistent structure in the
output.

## See also

[`as_check_df`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md)
for converting individual check results

Other check result utilities:
[`as_check_df()`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md),
[`capture_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/capture_check_results.md),
[`parse_check_output()`](https://insightsengineering.github.io/r.pkg.template/reference/parse_check_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine multiple check results
result1 <- list(has_deviation = TRUE, messages = "Issue A", details = NULL)
result2 <- list(has_deviation = FALSE, messages = "No issues", details = NULL)
combined <- combine_check_results(result1, result2)
} # }
```
