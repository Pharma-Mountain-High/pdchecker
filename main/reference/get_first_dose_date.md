# Get First Dose Date for Each Subject

Extract the earliest dosing start date from exposure datasets for each
subject.

## Usage

``` r
get_first_dose_date(
  data,
  ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
  ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT")
)
```

## Arguments

- data:

  List containing clinical trial datasets

- ex_datasets:

  Character vector, exposure dataset names (default: "EX"). Multiple
  datasets can be specified, e.g., c("EX1", "EX2")

- ex_date_var:

  Character vector, dosing start date variable names (default:
  "EXSTDAT").

  - If length 1, all datasets use the same column name

  - If length equals `ex_datasets`, corresponds one-to-one with datasets

## Value

Data frame with columns:

- SUBJID:

  Character. Subject ID

- first_dose_date:

  Date. First dose date

## Details

### Calculation Rules

- Extracts **earliest** dosing start date from all specified exposure
  datasets

- Always uses dosing **start date** variable (`ex_date_var`)

- Supports multiple datasets, automatically merges and takes minimum
  date per subject

- SAS missing values (NA, ".", "") are automatically excluded

### Multiple Datasets Support

When multiple datasets are specified:

- All datasets are combined

- For each subject, the minimum date across all datasets is returned

- Missing datasets are silently skipped

## See also

[`get_last_dose_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_last_dose_date.md)
for extracting last dose dates

Other date extraction:
[`get_eos_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eos_date.md),
[`get_eot_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eot_date.md),
[`get_last_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_last_dose_date.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with single dataset
data <- list(
  EX = data.frame(
    SUBJID = c("001", "001", "002"),
    EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05"),
    stringsAsFactors = FALSE
  )
)
first_dates <- get_first_dose_date(data)
# Subject 001: 2024-01-01 (min of 2024-01-01 and 2024-01-29)
# Subject 002: 2024-01-05

# Multiple datasets with same column name
data <- list(
  EX1 = data.frame(
    SUBJID = c("001", "001"),
    EXSTDAT = c("2024-01-15", "2024-02-01"),
    stringsAsFactors = FALSE
  ),
  EX2 = data.frame(
    SUBJID = c("001", "002"),
    EXSTDAT = c("2024-01-10", "2024-01-20"),
    stringsAsFactors = FALSE
  )
)
first_dates <- get_first_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = "EXSTDAT"
)
# Subject 001: 2024-01-10 (min across EX1 and EX2)
# Subject 002: 2024-01-20

# Multiple datasets with different column names
data <- list(
  EX1 = data.frame(
    SUBJID = "001",
    STDAT1 = "2024-01-15",
    stringsAsFactors = FALSE
  ),
  EX2 = data.frame(
    SUBJID = "001",
    STDAT2 = "2024-01-10",
    stringsAsFactors = FALSE
  )
)
first_dates <- get_first_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = c("STDAT1", "STDAT2")
)
# Subject 001: 2024-01-10
} # }
```
