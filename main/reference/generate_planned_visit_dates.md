# Generate Planned Visit Dates

Calculate planned visit dates and visit window ranges for each subject
based on visit schedule data and clinical trial data. Supports
screening, treatment, end of treatment, and follow-up visit date
calculations.

## Usage

``` r
generate_planned_visit_dates(
  data,
  visitcode = NULL,
  ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
  ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT"),
  ex_end_date_var = getOption("pdchecker.ex_end_date_var", NULL),
  sv_dataset = getOption("pdchecker.sv_dataset", "SV"),
  sv_visit_var = getOption("pdchecker.sv_visit_var", "VISIT"),
  sv_visitnum_var = getOption("pdchecker.sv_visitnum_var", "VISITNUM"),
  sv_date_var = getOption("pdchecker.sv_date_var", "SVDAT"),
  eot_dataset = getOption("pdchecker.eot_dataset", "EOT"),
  eot_date_var = getOption("pdchecker.eot_date_var", "EOTDAT"),
  ds_dataset = getOption("pdchecker.ds_dataset", "DS"),
  ds_date_var = getOption("pdchecker.ds_date_var", "DSDAT"),
  tb_name_var = getOption("pdchecker.tb_name_var", NULL),
  cycle_days = 28
)
```

## Arguments

- data:

  List containing all clinical trial datasets

- visitcode:

  Data frame, visit schedule data from
  [`read_visitcode_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md)
  (default: NULL). If NULL, the function will look for a variable named
  `visitcode` in the calling environment. If not found, an error will be
  raised. Must contain columns: VISIT, VISITNUM (numeric), WP, CYCLE,
  VISITDAY, type, wpvalue (numeric)

- ex_datasets:

  Character vector, exposure dataset names (default: "EX"). Multiple
  datasets can be specified, e.g., c("EX1", "EX2")

- ex_date_var:

  Character vector, dosing start date variable names (default:
  "EXSTDAT").

  - If length 1, all datasets use the same column name

  - If length equals `ex_datasets`, corresponds one-to-one with datasets

  - Example: c("EXSTDAT1", "EXSTDAT2") corresponds to c("EX1", "EX2")

- ex_end_date_var:

  Character vector, dosing end date variable names (default: NULL).

  - If NULL or empty, last dose date uses `ex_date_var` (dosing start
    date)

  - If specified, last dose date uses dosing end date

  - If length 1, all datasets use the same column name

  - If length equals `ex_datasets`, corresponds one-to-one with datasets

- sv_dataset:

  Character string, visit dataset name (default: "SV")

- sv_visit_var:

  Character string, visit name variable in visit dataset (default:
  "VISIT")

- sv_visitnum_var:

  Character string, visit number variable in visit dataset (default:
  "VISITNUM")

- sv_date_var:

  Character string, visit date variable in visit dataset (default:
  "SVDAT")

- eot_dataset:

  Character string, end of treatment dataset name (default: "EOT"). If
  dataset doesn't exist, EOT-related planned dates will be NA

- eot_date_var:

  Character string, end of treatment date variable (default: "EOTDAT")

- ds_dataset:

  Character string, disposition dataset name (default: "DS"). If dataset
  doesn't exist, EOS-related planned dates will be NA

- ds_date_var:

  Character string, end of study date variable (default: "DSDAT")

- tb_name_var:

  Character string specifying the variable name to use as TBNAME in the
  output (default: NULL). If NULL or the column does not exist in the
  visit dataset, the TBNAME column in the output will be empty.

- cycle_days:

  Numeric, treatment cycle length in days (default: 28). Used to
  calculate subsequent cycle D1 planned dates

## Value

Data frame with the following columns:

- SUBJID:

  Character. Subject ID

- VISIT:

  Character. Visit name

- VISITNUM:

  Numeric. Visit number

- visittype:

  Character. Visit type (cycle information)

- visitday:

  Character. Visit day

- visit_category:

  Character. Visit category (screening, treatment, end_of_treatment,
  follow_up, or unknown)

- planned_date:

  Date. Planned visit date

- wp_start:

  Date. Visit window start date

- wp_end:

  Date. Visit window end date

- wp_type:

  Character. Window type (+/-, +, -, etc.)

- wp_value:

  Numeric. Window value in days

- actual_date:

  Date. Actual visit date (NA if not available)

- status:

  Character. Visit status ("completed" or "missing")

- first_dose_date:

  Date. Subject's first dose date

- last_dose_date:

  Date. Subject's last dose date

- eot_date:

  Date. Subject's end of treatment date

- eos_date:

  Date. Subject's end of study date

- TBNAME:

  Character. Table name from the variable specified by `tb_name_var`,
  empty if `tb_name_var` is NULL

## Details

### Visit Categories

The function handles four types of visits:

|  |  |
|----|----|
| Category | Description |
| Screening | Based on first dose date |
| Treatment | D1 visits use iterative cycle calculation; non-D1 based on cycle D1 date |
| End of Treatment | Based on EOT date or EOS date |
| Follow-up | Based on EOT date or last dose date |

For detailed calculation rules of each visit type, see the internal
calculation functions in `planned_date_calculation.R`.

### Visit Window Calculation

Window ranges are calculated based on the window type specified in visit
schedule:

|             |                              |
|-------------|------------------------------|
| Window Type | Window Range                 |
| +/-Nd       | `[planned - N, planned + N]` |
| +Nd         | `[planned, planned + N]`     |
| -Nd         | `[planned - N, planned]`     |

## See also

[`read_visitcode_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md)
for reading visit schedule files

[`check_visit_window`](https://insightsengineering.github.io/r.pkg.template/reference/check_visit_window.md)
for checking visit window deviations

[`check_missing_visit`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_visit.md)
for checking missing visits

[`get_first_dose_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_first_dose_date.md)
for extracting first dose dates

[`get_last_dose_date`](https://insightsengineering.github.io/r.pkg.template/reference/get_last_dose_date.md)
for extracting last dose dates

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare visit schedule data
visit_schedule <- data.frame(
  VISIT = c("Screening", "C1D1", "C1D8", "C2D1", "EOT"),
  VISITNUM = c(0, 1, 2, 3, 99),
  WP = c("", "+/-3d", "+/-2d", "+/-3d", "+7d"),
  CYCLE = c("Screening", "Cycle 1", "Cycle 1", "Cycle 2", "End of Treatment"),
  VISITDAY = c("0", "1", "8", "1", "EOT"),
  type = c(NA, "+/-", "+/-", "+/-", "+"),
  wpvalue = c(NA, 3, 2, 3, 7),
  stringsAsFactors = FALSE
)

# Prepare clinical trial data
data <- list(
  EX = data.frame(
    SUBJID = c("001", "001", "002"),
    EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05"),
    stringsAsFactors = FALSE
  ),
  SV = data.frame(
    SUBJID = c("001", "001", "002"),
    VISIT = c("C1D1", "C1D8", "C1D1"),
    VISITNUM = c(1, 2, 1),
    SVDAT = c("2024-01-01", "2024-01-08", "2024-01-05"),
    stringsAsFactors = FALSE
  ),
  EOT = data.frame(
    SUBJID = c("001", "002"),
    EOTDAT = c("2024-03-15", "2024-03-20"),
    stringsAsFactors = FALSE
  ),
  DS = data.frame(
    SUBJID = c("001", "002"),
    DSDAT = c("2024-04-15", "2024-04-20"),
    stringsAsFactors = FALSE
  )
)

# Generate planned visit dates (using default visitcode variable)
visitcode <- visit_schedule
result <- generate_planned_visit_dates(data = data)

# Or explicitly pass visitcode
result <- generate_planned_visit_dates(
  data = data,
  visitcode = visit_schedule
)

# With custom cycle days (21-day cycle)
result <- generate_planned_visit_dates(
  data = data,
  visitcode = visit_schedule,
  cycle_days = 21
)

# With multiple exposure datasets
result <- generate_planned_visit_dates(
  data = data,
  visitcode = visit_schedule,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = c("EXSTDAT1", "EXSTDAT2")
)
} # }
```
