# Get Last Dose Date for Each Subject

Extract the latest dosing date from exposure datasets for each subject.
Uses end date if specified, otherwise uses start date.

## Usage

``` r
get_last_dose_date(
  data,
  ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
  ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT"),
  ex_end_date_var = getOption("pdchecker.ex_end_date_var", NULL)
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
  "EXSTDAT"). Used when `ex_end_date_var` is not specified.

  - If length 1, all datasets use the same column name

  - If length equals `ex_datasets`, corresponds one-to-one with datasets

- ex_end_date_var:

  Character vector, dosing end date variable names (default: NULL). If
  specified, uses end date for last dose calculation.

  - If length 1, all datasets use the same column name

  - If length equals `ex_datasets`, corresponds one-to-one with datasets

## Value

Data frame with columns:

- SUBJID:

  Character. Subject ID

- last_dose_date:

  Date. Last dose date

## Details

### Calculation Rules

The function uses different logic based on whether `ex_end_date_var` is
specified:

#### When ex_end_date_var is NOT specified (default)

- Uses dosing **start date** (`ex_date_var`)

- Takes the **latest** dosing start date from all records

#### When ex_end_date_var IS specified

- Uses dosing **end date** (`ex_end_date_var`)

- Takes the **latest** dosing end date from all records

- Suitable for studies with explicit start and end dates per dose

### Multiple Datasets Support

When multiple datasets are specified:

- All datasets are combined

- For each subject, the maximum date across all datasets is returned

- Missing datasets are silently skipped

## See also

[`get_first_dose_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_first_dose_date.md)
for extracting first dose dates

Other date extraction:
[`get_eos_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eos_date.md),
[`get_eot_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_eot_date.md),
[`get_first_dose_date()`](https://insightsengineering.github.io/r.pkg.template/reference/get_first_dose_date.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage - using start date as last dose date
data <- list(
  EX = data.frame(
    SUBJID = c("001", "001", "002"),
    EXSTDAT = c("2024-01-01", "2024-02-15", "2024-01-05"),
    stringsAsFactors = FALSE
  )
)
last_dates <- get_last_dose_date(data)
# Subject 001: 2024-02-15 (max of start dates)
# Subject 002: 2024-01-05

# Using end date for last dose
data <- list(
  EX = data.frame(
    SUBJID = c("001", "001"),
    EXSTDAT = c("2024-01-01", "2024-02-01"),
    EXENDAT = c("2024-01-28", "2024-02-28"),
    stringsAsFactors = FALSE
  )
)
last_dates <- get_last_dose_date(
  data,
  ex_end_date_var = "EXENDAT"
)
# Subject 001: 2024-02-28 (max of end dates)

# Multiple datasets with same column name
data <- list(
  EX1 = data.frame(
    SUBJID = c("001", "001"),
    EXSTDAT = c("2024-01-01", "2024-02-01"),
    stringsAsFactors = FALSE
  ),
  EX2 = data.frame(
    SUBJID = c("001", "002"),
    EXSTDAT = c("2024-03-01", "2024-01-15"),
    stringsAsFactors = FALSE
  )
)
last_dates <- get_last_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = "EXSTDAT"
)
# Subject 001: 2024-03-01 (max across EX1 and EX2)
# Subject 002: 2024-01-15

# Multiple datasets with different column names
data <- list(
  EX1 = data.frame(
    SUBJID = "001",
    STDAT1 = "2024-01-15",
    stringsAsFactors = FALSE
  ),
  EX2 = data.frame(
    SUBJID = "001",
    STDAT2 = "2024-03-20",
    stringsAsFactors = FALSE
  )
)
last_dates <- get_last_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = c("STDAT1", "STDAT2")
)
# Subject 001: 2024-03-20
} # }
```
