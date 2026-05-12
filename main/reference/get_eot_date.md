# Get End of Treatment Date for Each Subject

Extract the end of treatment date from the EOT dataset for each subject.

## Usage

``` r
get_eot_date(
  data,
  eot_dataset = getOption("pdchecker.eot_dataset", "EOT"),
  eot_date_var = getOption("pdchecker.eot_date_var", "EOTDAT")
)
```

## Arguments

- data:

  List containing clinical trial datasets

- eot_dataset:

  Character string, EOT dataset name (default: "EOT")

- eot_date_var:

  Character string, EOT date variable name (default: "EOTDAT")

## Value

Data frame with columns:

- SUBJID:

  Character. Subject ID

- eot_date:

  Date. End of treatment date

## Details

### Calculation Rules

- Extracts the EOT date from the specified dataset and variable

- If the dataset or variable doesn't exist, returns empty data frame

- SAS missing values (NA, ".", "") are automatically excluded

- If multiple records exist for a subject, returns distinct values

## See also

[`get_eos_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_eos_date.md)
for extracting end of study dates

Other date extraction:
[`get_eos_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eos_date.md),
[`get_first_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_first_dose_date.md),
[`get_last_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_last_dose_date.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- list(
  EOT = data.frame(
    SUBJID = c("001", "002"),
    EOTDAT = c("2024-06-01", "2024-06-15"),
    stringsAsFactors = FALSE
  )
)
eot_dates <- get_eot_date(data)
} # }
```
