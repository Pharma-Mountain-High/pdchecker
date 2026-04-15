# Read raw data

Scan specified directory and read all SAS format files

## Usage

``` r
read_raw_data(folder, iwrs_file = NULL, format_file = NULL)
```

## Arguments

- folder:

  Path to data directory containing SAS files

- iwrs_file:

  Path to iwrs CSV file. Defaults to NULL (no iwrs file). If provided,
  the CSV file should have 2 header rows that will be skipped during
  import. If NULL or if the file does not exist, iwrs data will be
  skipped with a message/warning.

- format_file:

  Optional path to Excel format mapping file (.xlsx). If provided, will
  be used to map coded values to labels. The Excel file should contain
  columns: (table name), (variable name), (coded value), (label), and
  (match status).

## Value

A named list of data.frames with the following structure:

- iwrs:

  iwrs data frame with randomization information. Present as the first
  element only if iwrs_file is provided and successfully read.

- \<DATASET_NAME\>:

  Additional datasets read from SAS files (e.g., DM, AE, LB, VS, etc.).
  Dataset names are converted to uppercase and derived from the SAS
  filename without the .sas7bdat extension.

All column names within each data frame are converted to uppercase.
Format mapping is applied if format files are available. Datasets that
failed to read are excluded from the returned list (with a warning
message). Empty datasets (0 rows) are included but reported via warning.

## Note

- Column names in all returned data frames are converted to uppercase.

- The FORMATS file (if found) is excluded from the returned data list.

- IWRS CSV file is expected to have 2 header rows that will be skipped.

- Format mapping priority: Excel format file \> SAS format file.

## See also

[`read_raw_data_with_formats()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data_with_formats.md)
for reading data with SAS catalog file

Other data reading functions:
[`read_raw_data_with_formats()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data_with_formats.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read SAS data files from a directory
data <- read_raw_data(
  folder = "path/to/sas/files",
  iwrs_file = "path/to/iwrs.csv"
)

# Read with additional Excel format mapping file
data <- read_raw_data(
  folder = "path/to/sas/files",
  iwrs_file = "path/to/iwrs.csv",
  format_file = "path/to/formats.xlsx"
)

# Access individual datasets
dm <- data$DM
ae <- data$AE
} # }
```
