# Generate HTML report from check results

Generate HTML report from check results

## Usage

``` r
generate_html_report(
  checks_df,
  output_file,
  include_no_deviation = FALSE,
  title = "Study Deviation Report",
  css_file = NULL
)
```

## Arguments

- checks_df:

  Data frame containing combined check results

- output_file:

  Path to output HTML file

- include_no_deviation:

  Whether to include checks with no deviations (default: FALSE)

- title:

  Report title (default: "Study Deviation Report")

- css_file:

  Path to CSS file for styling (optional)

## Value

Invisibly returns the output file path

## See also

[`combine_check_results`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)
for combining check results

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine check results first
all_results <- combine_check_results(res_1, res_2, res_3)

# Generate HTML report
generate_html_report(all_results, output_file = "report.html")

# With custom title
generate_html_report(all_results,
  output_file = "report.html",
  title = "PD Report - Study ABC"
)
} # }
```
