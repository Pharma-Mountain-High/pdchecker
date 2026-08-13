# Read raw data with SAS format catalog

This function reads raw SAS datasets using a format catalog file to
apply formats. Formats are applied by converting labelled columns to
factors using the catalog labels.

## Usage

``` r
read_raw_data_with_formats(
  data_dir,
  catalog_file,
  iwrs_file = NULL,
  encoding = "UTF-8"
)
```

## Arguments

- data_dir:

  Directory containing SAS data files (.sas7bdat)

- catalog_file:

  Path to SAS format catalog file (.sas7bcat). This file contains the
  format definitions that will be applied to the data.

- iwrs_file:

  Path to iwrs CSV file. Defaults to NULL (no iwrs file). If provided,
  the CSV file should have 2 header rows that will be skipped during
  import. If NULL or if the file does not exist, iwrs data will be
  skipped with a message/warning.

- encoding:

  Character encoding to use when reading SAS files and catalog file.
  Defaults to "UTF-8". Common alternatives include "latin1", "GBK", etc.

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
Columns with SAS format labels (labelled data) are automatically
converted to factors, with factor levels corresponding to the format
labels from the catalog file. Datasets that failed to read are excluded
from the returned list (with a warning message). Empty datasets (0 rows)
are included but reported via warning.

## Note

- Column names in all returned data frames are converted to uppercase.

- Labelled columns from SAS are automatically converted to factors.

- IWRS CSV file is expected to have 2 header rows that will be skipped.

- The same encoding is applied to both data files and catalog file.

## See also

[`read_raw_data()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data.md)
for reading data with SAS format data file

Other data reading functions:
[`read_raw_data()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read SAS data files with format catalog
data <- read_raw_data_with_formats(
  data_dir = "path/to/sas/files",
  catalog_file = "path/to/formats.sas7bcat",
  iwrs_file = "path/to/iwrs.csv"
)

# Read with custom encoding (e.g., for Chinese characters)
data <- read_raw_data_with_formats(
  data_dir = "path/to/sas/files",
  catalog_file = "path/to/formats.sas7bcat",
  iwrs_file = "path/to/iwrs.csv",
  encoding = "GBK"
)

# Access individual datasets
dm <- data$DM
ae <- data$AE
} # }
```
