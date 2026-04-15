# Check if any time variables occur before informed consent time

Check if any time variables occur before informed consent time

## Usage

``` r
check_icf_time_deviation(
  data,
  ic_dataset = getOption("pdchecker.ic_dataset", "IC"),
  ic_date_var = getOption("pdchecker.ic_date_var", "ICDAT"),
  ignore_vars = "BRTHDAT",
  exclude_datasets = NULL,
  tb_name_var = getOption("pdchecker.tb_name_var", NULL),
  pdno = "2.1.1"
)
```

## Arguments

- data:

  List of data frames containing study data

- ic_dataset:

  Character string specifying the name of the IC dataset (default: "IC")

- ic_date_var:

  Character string specifying the name of the IC date variable (default:
  "ICDAT")

- ignore_vars:

  Character vector of variables to ignore in the check (default:
  "BRTHDAT"). Can specify multiple variables, e.g., c("BRTHDAT",
  "MHSTDAT")

- exclude_datasets:

  Character vector of dataset names to exclude from the check (default:
  NULL). Can specify multiple datasets, e.g., c("DM", "DS")

- tb_name_var:

  Character string specifying the variable name to use as TBNAME in the
  output (default: NULL). If NULL, the TBNAME column in the output will
  be empty.

- pdno:

  Character string specifying the protocol deviation number for this
  check (default: "2.1.1")

## Value

A list of class "icf_time_deviation" with the following components:

- has_deviation:

  Logical. TRUE if any time deviations were found, FALSE otherwise

- messages:

  Character vector. Summary message describing the deviation

- details:

  Data frame. Contains detailed deviation records with columns:

  - PDNO: Protocol deviation number specified by `pdno` parameter

  - SUBJID: Subject identifier

  - action: Dataset and variable name (format: "dataset.variable")

  - event_datetime: Date when the event occurred

  - icf_datetime: Date when informed consent was obtained

  - diff_date: Numeric. Difference in days (negative values indicate
    event occurred before IC)

  - TBNAME: Table name from the variable specified by `tb_name_var`,
    empty if `tb_name_var` is NULL

  - DESCRIPTION: Description of the deviation for each record

## Note

Date variables in other datasets are identified by variable names ending
with "DAT" (e.g., LBDAT, VSDAT, AEENDAT). Variables not following this
naming convention will not be checked.

## See also

Other deviation checks:
[`check_screen_without_ic()`](https://insightsengineering.github.io/r.pkg.template/reference/check_screen_without_ic.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic usage with default parameters
data <- list(
  IC = data.frame(
    SUBJID = c("001", "002", "003"),
    ICDAT = c("2024-01-15", "2024-02-01", "2024-03-01")
  ),
  LB = data.frame(
    SUBJID = c("001", "002", "003"),
    LBDAT = c("2024-01-10", "2024-02-05", "2024-03-05")
  )
)
result <- check_icf_time_deviation(data)
print(result)

# Example 2: Exclude specific datasets from check
result <- check_icf_time_deviation(
  data,
  exclude_datasets = c("DM", "DS")
)

# Example 3: Ignore additional date variables
result <- check_icf_time_deviation(
  data,
  ignore_vars = c("BRTHDAT", "MHSTDAT", "DSSTDAT")
)

# Example 4: Use custom IC dataset and variable names
data_custom <- list(
  ICF = data.frame(
    SUBJID = c("001", "002"),
    ICFDAT = c("2024-01-15", "2024-02-01")
  ),
  VS = data.frame(
    SUBJID = c("001", "002"),
    VSDAT = c("2024-01-10", "2024-02-05")
  )
)
result <- check_icf_time_deviation(
  data_custom,
  ic_dataset = "ICF",
  ic_date_var = "ICFDAT"
)
} # }
```
