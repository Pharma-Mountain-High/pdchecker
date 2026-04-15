# Prepare Test Data for Missing Test Checks

Extract specified test dataset from a list of all data, merge with visit
dataset, and create standardized column names (TBNAME, TESTCAT, TESTDE,
TESTDAT, TESTYN, ORRES) for subsequent missing test checks.

## Usage

``` r
prepare_test_data(
  data,
  test_dataset,
  test_date_var = getOption("pdchecker.test_date_var", "LBDAT"),
  test_yn_var = getOption("pdchecker.test_yn_var", "YN"),
  test_result_var = getOption("pdchecker.test_result_var", "ORRES"),
  test_cat_var = getOption("pdchecker.test_cat_var", "LBCAT"),
  test_de_var = getOption("pdchecker.test_de_var", NULL),
  tb_name_var = getOption("pdchecker.tb_name_var", NULL),
  sv_dataset = getOption("pdchecker.sv_dataset", "SV"),
  sv_visit_var = getOption("pdchecker.sv_visit_var", "VISIT"),
  sv_visitnum_var = getOption("pdchecker.sv_visitnum_var", "VISITNUM"),
  sv_date_var = getOption("pdchecker.sv_date_var", "SVDAT"),
  config = NULL,
  config_cat = NULL,
  filter_cond = NULL
)
```

## Arguments

- data:

  List containing all clinical trial datasets

- test_dataset:

  Character string, test dataset name to extract (e.g., "LB")

- test_date_var:

  Character string, original test date variable (default: "LBDAT")

- test_yn_var:

  Character string, original test performed variable (default: "YN")

- test_result_var:

  Character string, original test result variable (default: "ORRES")

- test_cat_var:

  Character string, original test category variable (default: "LBCAT").
  If NULL or empty, TESTCAT column will be NA

- test_de_var:

  Character string, original test name variable (default: NULL). If NULL
  or empty, TESTDE column will be NA. Used for specific test names

- tb_name_var:

  Character string, original dataset name variable (default: NULL). If
  NULL or empty, TBNAME column uses test_dataset value (e.g., "LB")

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

- config:

  Data frame, test-visit configuration (required, default: NULL). If
  NULL, the function will look for a variable named `testconfig` in the
  calling environment; if not found, an error is raised. Must be a data
  frame, typically created by
  [`read_testconfig_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_testconfig_file.md)
  which expands VISITNUM to one per row. Config must contain: TESTCAT
  (test category), VISITNUM (visit number, one per row). Generates
  expected visit-test combinations as skeleton, showing empty records
  (TESTDAT=NA) even when original data has no records. Use
  [`read_testconfig_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_testconfig_file.md)
  to read config from Excel/CSV files

- config_cat:

  Character vector, test categories to filter from config (default:
  NULL). If NULL, uses all TESTCAT in config; If provided (e.g.,
  c("CBC", "Chemistry")), only uses those TESTCAT records

- filter_cond:

  Character string, subject filter condition (default: NULL). multiple
  conditions separated by semicolons (intersection/AND logic):

  - "ENROL\|ENRYN=='Y'" - Filter enrolled subjects

  - "SUBJECT\|SEX=='M'" - Filter males from SUBJECT dataset

  - "SUBJECT\|AGE\>=18" - Filter age \>= 18 from SUBJECT

  - "ENROL\|ENRYN=='Y';SUBJECT\|SEX=='M'" - Enrolled males
    (intersection) If NULL, no subject filtering is applied

## Value

Data frame with column order:

- SUBJID:

  Subject ID (from SV dataset)

- VISIT:

  Visit name (from SV dataset)

- VISITNUM:

  Visit number (from SV dataset)

- SVDAT:

  Visit date (from SV dataset)

- TBNAME:

  Dataset name (derived, mapped from tb_name_var or test_dataset)

- TESTCAT:

  Test category (from config skeleton, always populated)

- TESTCAT_ORIG:

  Original test category from test dataset.

- TESTDE:

  Test name (derived, mapped from test_de_var)

- TESTYN:

  Test performed flag (derived, mapped from test_yn_var)

- TESTDAT:

  Test date (derived, mapped from test_date_var)

- ORRES:

  Test result (derived, mapped from test_result_var)

- ...:

  All other original columns from test dataset

Note: SV dataset only keeps SUBJID, VISIT, VISITNUM, SVDAT in output

## Details

### Usage Workflow

This function is used for data preparation before
[`check_missing_test`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_test.md):

    # Step 1: Read config file once (if using config)
    testconfig <- read_testconfig_file("test_config.xlsx")

    # Step 2: Prepare test data (can reuse testconfig multiple times)
    lb_prepared <- prepare_test_data(
      data = list(LB = lb_data, SV = sv_data),
      test_dataset = "LB",
      config = testconfig
    )

    eg_prepared <- prepare_test_data(
      data = list(EG = eg_data, SV = sv_data),
      test_dataset = "EG",
      config = testconfig
    )

    # Step 3: Check missing
    result <- check_missing_test(
      data = lb_prepared,
      sv_data = sv_data,
      test_var = "LBTEST",
      test = "CBC"
    )

### Data Processing Logic

1.  Extract specified test dataset (e.g., "LB") from data list

2.  Extract visit dataset (default "SV") from data list, keeping only:
    SUBJID, VISIT, VISITNUM, SVDAT

3.  Left join test dataset to visit dataset by SUBJID, VISIT, VISITNUM

4.  Create standardized column names:

    - TBNAME: Dataset name (mapped from original column or uses
      test_dataset)

    - TESTCAT: Test category (mapped from original category column)

    - TESTDE: Test name (mapped from original test column), e.g., "RBC
      Count"

    - TESTDAT: Test date (mapped from original date column)

    - TESTYN: Test performed flag (mapped from original flag column)

    - ORRES: Test result (mapped from original result column)

5.  Return dataset with original test columns and derived columns in
    order: SUBJID, VISIT, VISITNUM, TBNAME, TESTCAT, TESTDE, TESTYN,
    TESTDAT, ORRES, other original columns

### Notes

- If a mapped column doesn't exist (e.g., test_yn_var), the derived
  column will be NA

- Left join is based on visit dataset, so records follow visit records

- SV dataset only keeps SUBJID, VISIT, VISITNUM, SVDAT; other columns
  excluded

- All original columns from test dataset are preserved

- Derived columns are placed first for easy viewing

## See also

[`check_missing_test`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_test.md)
for checking missing tests
[`read_testconfig_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_testconfig_file.md)
for reading test config files

## Examples

``` r
if (FALSE) { # \dontrun{
# Read test config
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)

# Prepare LB test data
lb_prepared <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_cat_var = "LBCAT",
  test_result_var = "ORRES",
  config = testconfig
)

# Prepare EG test data with subject filter
eg_prepared <- prepare_test_data(
  data = data,
  test_dataset = "EG",
  test_date_var = "EGDAT",
  test_cat_var = "EGCAT",
  config = testconfig,
  filter_cond = "ENROL|ENRYN=='Y'"
)

# Then check for missing tests
result <- check_missing_test(data = lb_prepared)
} # }
```
