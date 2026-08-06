# Convert check results to a standardized data frame

Converts check result objects (lists) into standardized data frames for
easier manipulation and reporting. This function is particularly useful
for combining results from multiple checks.

## Usage

``` r
as_check_df(check_result, check_name = NULL)
```

## Arguments

- check_result:

  List containing check results from any check function

- check_name:

  Character string. Name of the check (optional, will be extracted from
  the object's class attribute if not provided)

## Value

Data frame with standardized check results containing columns:

- check_name:

  Character. Name of the check

- has_deviation:

  Logical. Whether deviation was found

- message:

  Character. Summary message (collapsed from messages vector)

- PDNO:

  Character. Protocol deviation number (NA when details is empty)

- SUBJID:

  Character. Subject ID (NA when details is empty)

- DESCRIPTION:

  Character. Description of the deviation (NA when details is empty)

- ...:

  Additional columns from details if present

## Details

The function expects check_result to be a list with at least the
following components:

- has_deviation:

  Logical. Whether any deviation was found

- messages:

  Character vector. Summary messages

- details:

  Data frame (optional). Detailed deviation information

If details are present and non-empty, the function returns the detailed
data frame with added check_name and has_deviation columns. Otherwise,
it returns a summary data frame with one row.

## See also

Other check result utilities:
[`capture_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/capture_check_results.md),
[`combine_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md),
[`parse_check_output()`](https://insightsengineering.github.io/r.pkg.template/reference/parse_check_output.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert a check result to data frame
check_result <- list(
  has_deviation = TRUE,
  messages = c("Found 2 deviations"),
  details = data.frame(SUBJID = c("001", "002"), issue = c("A", "B"))
)
df <- as_check_df(check_result, check_name = "my_check")
} # }
```
