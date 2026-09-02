# Parse console output from check functions into a data frame

Parses the console output from check functions and converts it into a
standardized data frame format. Can either parse existing text or
capture output directly from a function call.

## Usage

``` r
parse_check_output(text = NULL, capture_output = FALSE, check_fn = NULL, ...)
```

## Arguments

- text:

  Character string or vector containing check function console output,
  or a connection object to read from. Used when capture_output is FALSE

- capture_output:

  Logical. If TRUE, the function will capture output from check_fn
  instead of using the text parameter

- check_fn:

  Function to call (only used if capture_output is TRUE)

- ...:

  Additional arguments to pass to check_fn (only used if capture_output
  is TRUE)

## Value

Data frame with standardized check results containing columns:

- SUBJID:

  Character. Subject ID (extracted from details if available, otherwise
  NA)

- check_name:

  Character. Name of the check

- has_deviation:

  Logical. Whether deviation was found

- message:

  Character. Summary findings message

- details:

  Character. Detailed deviation information

- PDNO:

  Character. Protocol deviation number (NA by default)

- DESCRIPTION:

  Character. Description of the deviation (NA by default)

## Details

The function expects output in a specific format with sections:

- Header: Check name, typically numbered (e.g., "8.4 Visit Window
  Check")

- Deviation status: "Has deviation: YES" or "Has deviation: NO"

- Findings: Summary messages following "Findings:"

- Deviation Details: Detailed information following "Deviation Details:"

When parsing deviation details, the function attempts to extract subject
IDs (SUBJID) from lines containing the Chinese character "Subject"
followed by numeric IDs. If multiple subjects are found, each gets a
separate row in the output.

## See also

[`capture_check_results`](https://insightsengineering.github.io/r.pkg.template/reference/capture_check_results.md)
for batch processing multiple checks

Other check result utilities:
[`as_check_df()`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md),
[`capture_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/capture_check_results.md),
[`combine_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Parse check output text
output_text <- "8.4 Visit Window Check\n====================\nHas deviation: YES\nFindings:\n- Found 1 issue"
result <- parse_check_output(text = output_text)

# Capture and parse output from a function
result <- parse_check_output(capture_output = TRUE, check_fn = my_check_function, data = my_data)
} # }
```
