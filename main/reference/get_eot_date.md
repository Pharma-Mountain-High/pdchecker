# Get End of Treatment Date for Each Subject

Extract the end of treatment date from EOT datasets for each subject.

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

  Character vector, EOT dataset names (default: "EOT"). Multiple
  datasets can be specified, e.g., c("EOT1", "EOT2")

- eot_date_var:

  Character vector, EOT date variable names (default: "EOTDAT").

  - If length 1, all datasets use the same column name

  - If length equals `eot_dataset`, corresponds one-to-one with datasets

## Value

Data frame with columns:

- SUBJID:

  Character. Subject ID

- eot_date:

  Date. End of treatment date

## Details

### Calculation Rules

- Extracts EOT dates from all specified datasets and variables

- For each subject, returns the **latest** EOT date across all records
  and datasets

- Missing datasets are silently skipped

- SAS missing values (NA, ".", "") are automatically excluded

### Multiple Datasets Support

When multiple datasets are specified:

- All datasets are combined

- For each subject, the maximum date across all datasets is returned

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

# Multiple datasets - take latest date per subject
data <- list(
  EOT1 = data.frame(
    SUBJID = "001",
    EOTDAT = "2024-06-01",
    stringsAsFactors = FALSE
  ),
  EOT2 = data.frame(
    SUBJID = "001",
    EOTDAT = "2024-07-15",
    stringsAsFactors = FALSE
  )
)
eot_dates <- get_eot_date(
  data,
  eot_dataset = c("EOT1", "EOT2"),
  eot_date_var = "EOTDAT"
)
# Subject 001: 2024-07-15 (max across EOT1 and EOT2)
} # }
```
