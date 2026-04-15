# Generate markdown report from check results

Generate markdown report from check results

## Usage

``` r
generate_markdown_report(
  checks_df,
  output_file = NULL,
  include_no_deviation = FALSE,
  title = "Study Deviation Report"
)
```

## Arguments

- checks_df:

  Data frame containing combined check results

- output_file:

  Path to output markdown file (optional)

- include_no_deviation:

  Whether to include checks with no deviations (default: FALSE)

- title:

  Report title (default: "Study Deviation Report")

## Value

Invisibly returns the report content as a character string

## See also

[`combine_check_results`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)
for combining check results

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine check results first
all_results <- combine_check_results(res_1, res_2, res_3)

# Generate markdown report to file
generate_markdown_report(all_results, output_file = "report.md")

# Get report content as string
report_text <- generate_markdown_report(all_results)

# Include checks with no deviations
generate_markdown_report(all_results,
  output_file = "full_report.md",
  include_no_deviation = TRUE
)
} # }
```
