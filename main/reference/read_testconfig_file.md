# Read Test Configuration File

Read test-visit configuration from Excel or CSV file and expand VISITNUM
column into multiple rows for subsequent use in
[`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md).

## Usage

``` r
read_testconfig_file(file_path, sheet_name = "Sheet1", visitcode = NULL)
```

## Arguments

- file_path:

  Character. File path (.xlsx, .xls, or .csv). Must be a non-empty
  string pointing to an existing file.

- sheet_name:

  Character. Excel sheet name (default: "Sheet1"). Ignored for CSV
  files.

- visitcode:

  Data frame. Result from
  [`read_visitcode_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md)
  (default: NULL). If NULL, the function will look for a variable named
  `visitcode` in the calling environment. If found, VISIT column will be
  joined by VISITNUM. The visitcode data frame must contain VISITNUM and
  VISIT columns.

## Value

A data frame with expanded rows:

- TESTCAT:

  Test category (character)

- VISITNUM:

  Visit number (character), one per row

- VISIT:

  Visit name (character), if visitcode is provided

- ...:

  All other columns from input file

## Details

### Purpose

This function reads external configuration files that define which tests
should be performed at which visits. The output can be reused multiple
times with
[`prepare_test_data`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md)
without re-reading the file.

### Usage Workflow

    # Step 1: Read config file once
    testconfig <- read_testconfig_file("test_config.xlsx")

    # Step 2: Use config multiple times
    lb_data <- prepare_test_data(data, "LB", config = testconfig)
    eg_data <- prepare_test_data(data, "EG", config = testconfig)

### VISITNUM Expansion

The VISITNUM column in config file can contain comma-separated visit
numbers (e.g., "1,2,3"). This function expands them into separate rows:

Input:

|         |          |
|---------|----------|
| TESTCAT | VISITNUM |
| CBC     | 1,2,3    |

Output:

|         |          |
|---------|----------|
| TESTCAT | VISITNUM |
| CBC     | 1        |
| CBC     | 2        |
| CBC     | 3        |

### Required Columns

The input file must contain:

- TESTCAT: Test category name

- VISITNUM: Visit numbers (comma-separated string or single number)

### Joining with Visit Code

If `visitcode` parameter is provided (from
[`read_visitcode_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md)),
the VISIT column will be joined to the result by VISITNUM:

    # Read visit code first
    visitcode <- read_visitcode_file("visit_schedule.xlsx")

    # Read test config with visit names
    testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
    # Result will contain: TESTCAT, VISITNUM, VISIT, ...

## Examples

``` r
if (FALSE) { # \dontrun{
# Read from Excel file
testconfig <- read_testconfig_file("test_config.xlsx")

# Read from specific sheet
testconfig <- read_testconfig_file("test_config.xlsx", sheet_name = "Config")

# Read from CSV file
testconfig <- read_testconfig_file("test_config.csv")

# Read with visit names (recommended workflow)
visitcode <- read_visitcode_file("visit_schedule.xlsx")
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
# testconfig now contains: TESTCAT, VISITNUM, VISIT, ...

# Use in prepare_test_data
prepared_data <- prepare_test_data(
  data = all_data,
  test_dataset = "LB",
  config = testconfig
)
} # }
```
