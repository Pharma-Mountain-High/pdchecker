# Check Missing Visits

Check for missing visits based on planned visit dates. Receives planned
visit data from
[`generate_planned_visit_dates`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md)
and uses different cutoff date criteria based on visit type to determine
if a visit should have been completed.

## Usage

``` r
check_missing_visit(planned_dates, cutoffdt = Sys.Date(), pdno = "8.2.1")
```

## Arguments

- planned_dates:

  Data frame from
  [`generate_planned_visit_dates`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md).
  Must contain: SUBJID, VISIT, VISITNUM, visittype, visit_category,
  planned_date, status, eot_date, eos_date

- cutoffdt:

  Date, data cutoff date (default: current date). Used to determine if
  visit should be completed

- pdno:

  Character string specifying the protocol deviation number for this
  check (default: "8.2.1")

## Value

List with the following components:

- has_deviation:

  Logical. TRUE if there are missing visits

- messages:

  Character vector. Deviation description messages

- details:

  Data frame. Missing visit details with columns:

  - PDNO: Protocol deviation number specified by `pdno` parameter

  - SUBJID: Subject ID

  - first_dose_date: Subject's first dose date

  - VISIT: Missing visit name

  - VISITNUM: Visit number

  - planned_date: Planned visit date

  - visittype: Visit type

  - eot_date: Subject's end of treatment date

  - eos_date: Subject's end of study date

  - cutoffdt: Data cutoff date

  - valid_visits_count: Total visits that should be completed

  - completed_visits_count: Actual completed visits

  - TBNAME: Table name from planned_dates, empty if not available

  - DESCRIPTION: Description of the deviation for each record

- planned_visits:

  Data frame. Complete input planned visit information

## Details

### Usage Workflow

This function is designed to work with `generate_planned_visit_dates`:

    # Step 1: Read visit schedule and generate planned visit dates
    visitcode <- read_visitcode_file("visit_schedule.xlsx")
    planned_dates <- generate_planned_visit_dates(data = study_data)

    # Step 2: Check missing visits
    result <- check_missing_visit(
      planned_dates = planned_dates,
      cutoffdt = as.Date("2024-12-31")
    )

### Visit Completion Criteria

Different visit types use different termination date criteria:

#### Screening, Pre-treatment and Treatment Visits

Planned date must be \< min(end of treatment date, end of study date,
cutoff date)

#### End of Treatment and Follow-up Visits

Planned date must be \<= min(end of study date, cutoff date)

### Visit Type Classification

Visit types are automatically identified by
`generate_planned_visit_dates`:

- screening: Screening visits

- pre_treatment: Pre-treatment visits

- treatment: Treatment visits

- end_of_treatment: End of treatment visits

- follow_up: Follow-up visits

## See also

[`generate_planned_visit_dates`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md)
for generating planned visit data
[`check_visit_window`](https://insightsengineering.github.io/r.pkg.template/reference/check_visit_window.md)
for checking visit window deviations

Other visit checks:
[`check_visit_window()`](https://insightsengineering.github.io/r.pkg.template/reference/check_visit_window.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load data and visit schedule
data <- read_raw_data("path/to/data")
visitcode <- read_visitcode_file("path/to/visitcode.xlsx")

# Generate planned visit dates (uses visitcode variable automatically)
planned_dates <- generate_planned_visit_dates(data = data)

# Check missing visits with default cutoff (current date)
result <- check_missing_visit(planned_dates)
print(result)

# Check missing visits with specific cutoff date
result <- check_missing_visit(
  planned_dates = planned_dates,
  cutoffdt = as.Date("2024-06-30")
)

# Access deviation details
if (result$has_deviation) {
  missing_details <- result$details
}
} # }
```
