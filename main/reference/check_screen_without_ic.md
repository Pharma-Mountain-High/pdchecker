# Check for screened subjects without informed consent

Check for screened subjects without informed consent

## Usage

``` r
check_screen_without_ic(
  data,
  sv_dataset = getOption("pdchecker.sv_dataset", "SV"),
  ic_dataset = getOption("pdchecker.ic_dataset", "IC"),
  sv_visit_var = getOption("pdchecker.sv_visit_var", "VISIT"),
  visit_pattern = "Screening|screening",
  ic_date_var = getOption("pdchecker.ic_date_var", "ICDAT"),
  tb_name_var = getOption("pdchecker.tb_name_var", NULL),
  pdno = "2.4.1"
)
```

## Arguments

- data:

  List of data frames containing study data

- sv_dataset:

  Character string specifying which dataset to use for visit data
  (default: "SV")

- ic_dataset:

  Character string specifying which dataset to use for informed consent
  data (default: "IC")

- sv_visit_var:

  Character string specifying the variable name for visit in visit
  dataset (default: "VISIT")

- visit_pattern:

  Character string or vector specifying the pattern(s) to identify
  screening visits (default: "Screening\|screening")

- ic_date_var:

  Character string specifying the variable name for informed consent
  date in IC dataset (default: "ICDAT")

- tb_name_var:

  Character string specifying the variable name to use as TBNAME in the
  output (default: NULL). If NULL, the TBNAME column in the output will
  be empty.

- pdno:

  Character string specifying the protocol deviation number for this
  check (default: "2.4.1")

## Value

A list of class "screen_ic_check" containing:

- has_deviation:

  Logical value indicating whether any deviation was found. `TRUE` if
  subjects with screening visits but without informed consent were
  found, `FALSE` otherwise.

- messages:

  Character vector of deviation messages.

- details:

  Data frame containing the following columns:

  PDNO

  :   Protocol deviation number specified by `pdno` parameter.

  SUBJID

  :   Subject IDs who have screening visits but no informed consent
      date.

  VISIT

  :   Visit name from the visit dataset.

  TBNAME

  :   Table name from the variable specified by `tb_name_var`, empty if
      `tb_name_var` is NULL.

  DESCRIPTION

  :   Description of the deviation for each subject.

  Returns empty data frame with these columns if no deviations found.

## See also

[`check_icf_time_deviation()`](https://insightsengineering.github.io/r.pkg.template/reference/check_icf_time_deviation.md)
for checking events before informed consent

Other deviation checks:
[`check_icf_time_deviation()`](https://insightsengineering.github.io/r.pkg.template/reference/check_icf_time_deviation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic usage with default parameters
data <- list(
  SV = data.frame(
    SUBJID = c("001", "002", "003"),
    VISIT = c("Screening", "Screening", "C1D1")
  ),
  IC = data.frame(
    SUBJID = c("001", "002"),
    ICDAT = c("2024-01-01", "2024-01-02")
  )
)
result <- check_screen_without_ic(data)
print(result)

# Example 2: Using custom dataset names
data2 <- list(
  VIS = data.frame(
    SUBJID = c("001", "002"),
    VISIT = c("Screening", "Screening")
  ),
  ICF = data.frame(
    SUBJID = c("001"),
    ICDAT = c("2024-01-01")
  )
)
result2 <- check_screen_without_ic(
  data2,
  sv_dataset = "VIS",
  ic_dataset = "ICF"
)

# Example 3: Using custom variable names and visit pattern
data3 <- list(
  SV = data.frame(
    SUBJID = c("001", "002", "003"),
    VISITNAME = c("Scr", "Scr", "Treatment")
  ),
  IC = data.frame(
    SUBJID = c("001", "002"),
    ICFDAT = c("2024-01-01", "2024-01-02")
  )
)
result3 <- check_screen_without_ic(
  data3,
  sv_visit_var = "VISITNAME",
  visit_pattern = "Scr",
  ic_date_var = "ICFDAT"
)
} # }
```
