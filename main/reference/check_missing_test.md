# Check Missing Tests

For each subject's completed visit records, check if a specific test
item was performed.

## Usage

``` r
check_missing_test(
  data,
  test_var = NULL,
  test = NULL,
  missing_de = TRUE,
  pdno = "8.3.1"
)
```

## Arguments

- data:

  Data frame, test data to check. Must be prepared by
  [`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md)
  with standardized column names

- test_var:

  Character string, variable name for filtering specific tests (default:
  NULL). If NULL, checks all tests. Typically use "TESTDE" (test name)
  or "TESTCAT" (test category)

- test:

  Character vector, one or more specific values for test_var (default:
  NULL). E.g., "RBC Count" or c("RBC Count", "WBC Count"). Ignored if
  test_var is NULL

- missing_de:

  Logical, whether to check individual TESTDE missing (default: TRUE).
  If TRUE, checks all three missing types; If FALSE, only checks first
  two types (TESTCAT empty, entire TESTCAT missing)

- pdno:

  Character string specifying the protocol deviation number for this
  check (default: "8.3.1")

## Value

List with the following components:

- has_deviation:

  Logical. TRUE if there are missing tests

- messages:

  Character vector. Deviation description messages

- details:

  Data frame. Missing test details with columns:

  - PDNO: Protocol deviation number specified by `pdno` parameter

  - SUBJID: Subject ID

  - VISIT: Visit name

  - VISITNUM: Visit number

  - visit_date: Actual visit date

  - TBNAME: Test dataset name (e.g., LB, VS, EG)

  - test_name: Test name

  - missing_type: Missing type

  - DESCRIPTION: Detailed description of the missing test

## Details

### Usage Workflow

This function checks if corresponding tests were completed in actual
visit records. It is recommended to first prepare data using
[`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md):

    # Step 1: Read config file (required)
    testconfig <- read_testconfig_file("config/test_config.xlsx")

    # Step 2: Prepare test data (uses testconfig automatically)
    prepared_data <- prepare_test_data(
      data = list(LB = lb_data, SV = sv_data),
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_result_var = "ORRES"
    )

    # Step 3: Check all tests (including overall missing and individual missing)
    result <- check_missing_test(data = prepared_data)

    # Or check specific tests (e.g., RBC count)
    result <- check_missing_test(
      data = prepared_data,
      test_var = "TESTDE",
      test = "RBC Count"
    )

    # Or check multiple tests at once
    result <- check_missing_test(
      data = prepared_data,
      test_var = "TESTDE",
      test = c("RBC Count", "WBC Count")
    )

    # Only check overall test missing, not individual indicators
    result <- check_missing_test(
      data = prepared_data,
      missing_de = FALSE
    )

### Missing Test Logic

This function distinguishes three types of missing:

**1. TESTCAT is empty** (visit has no test records)

- Condition: TESTCAT is empty or NA

- Output: One record per subject/visit/TBNAME combination

- Format: e.g., "LB missing"

- Reason: "TBNAME test not performed"

**2. TESTCAT not empty, but entire TESTCAT missing** (e.g., entire CBC
not done)

- Condition: TESTCAT not empty, but TESTDAT is empty or TESTYN != "Yes"

- Output: One record per subject/visit/TESTCAT combination

- Format: e.g., "CBC missing"

- Reason: "Entire test category not performed"

**3. TESTCAT not empty, but individual TESTDE missing** (e.g., WBC count
missing)

- Condition: TESTCAT not empty, TESTDAT not empty, but ORRES is empty

- Output: One record for each missing TESTDE per subject/visit/TESTCAT

- Format: e.g., "CBC-WBC Count missing"

- Reason: "Test result is empty"

Note: Input data must be prepared by
[`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md),
which standardizes column names to SUBJID, VISIT, VISITNUM, SVDAT,
TESTCAT, TESTDE, TESTYN, TESTDAT, ORRES, etc.

## See also

[`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md)
for preparing test data
[`check_missing_visit`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_visit.md)
for checking missing visits

## Examples

``` r
if (FALSE) { # \dontrun{
# Step 1: Read config and prepare test data
testconfig <- read_testconfig_file("test_config.xlsx")
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_result_var = "ORRES"
)

# Step 2: Check all missing tests
result <- check_missing_test(data = prepared_lb)
print(result)

# Check specific test only
result <- check_missing_test(
  data = prepared_lb,
  test_var = "TESTDE",
  test = "RBC Count"
)

# Only check overall missing, skip individual indicators
result <- check_missing_test(data = prepared_lb, missing_de = FALSE)
} # }
```
