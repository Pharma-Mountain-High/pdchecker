# Generate Excel report from check results

Generate Excel report from check results

## Usage

``` r
generate_excel_report(
  checks_df,
  output_file,
  title = "Study Deviation Report",
  report_cols = c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")
)
```

## Arguments

- checks_df:

  Data frame containing combined check results. Must contain `PDNO`,
  `SUBJID`, and `has_deviation`. `PDNO` and `SUBJID` are used for
  sorting; they need not appear in `report_cols`.

- output_file:

  Path to output Excel file

- title:

  Report title (default: "Study Deviation Report")

- report_cols:

  Character vector of column names to include in the "All Deviations"
  sheet. Columns not found in `checks_df` will be skipped with a
  warning. Default:
  `c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")`.

## Value

Invisibly returns the output file path

## Details

The “Summary” sheet lists every `check_name`, including those with
`has_deviation == FALSE`. The “All Deviations” sheet contains only rows
where `has_deviation` is `TRUE`.

## See also

[`combine_check_results`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)
for combining check results

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine check results first
all_results <- combine_check_results(res_1, res_2, res_3)

# Generate Excel report
generate_excel_report(all_results, output_file = "pd_report.xlsx")

# Specify custom columns to output
generate_excel_report(all_results,
  output_file = "custom_report.xlsx",
  report_cols = c("PDNO", "SITEID", "SUBJID", "TBNAME", "PDCAT", "DESCRIPTION")
)
} # }
```
