# Get End of Study Date for Each Subject

Extract the end of study date from the DS dataset for each subject.

## Usage

``` r
get_eos_date(
  data,
  ds_dataset = getOption("pdchecker.ds_dataset", "DS"),
  ds_date_var = getOption("pdchecker.ds_date_var", "DSDAT")
)
```

## Arguments

- data:

  List containing clinical trial datasets

- ds_dataset:

  Character string, DS dataset name (default: "DS")

- ds_date_var:

  Character string, EOS date variable name (default: "DSDAT")

## Value

Data frame with columns:

- SUBJID:

  Character. Subject ID

- eos_date:

  Date. End of study date

## Details

### Calculation Rules

- Extracts the EOS date from the specified dataset and variable

- If the dataset or variable doesn't exist, returns empty data frame

- SAS missing values (NA, ".", "") are automatically excluded

- If multiple records exist for a subject, returns distinct values

## See also

[`get_eot_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_eot_date.md)
for extracting end of treatment dates

Other date extraction:
[`get_eot_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eot_date.md),
[`get_first_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_first_dose_date.md),
[`get_last_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_last_dose_date.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- list(
  DS = data.frame(
    SUBJID = c("001", "002"),
    DSDAT = c("2024-09-01", "2024-09-15"),
    stringsAsFactors = FALSE
  )
)
eos_dates <- get_eos_date(data)
} # }
```
