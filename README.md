# pdchecker

Protocol Deviation Checker for Clinical Trials

## Project Status

🚧 **Under Development** - Data reading module is complete. Protocol deviation checking functions are in progress.

## Overview

`pdchecker` is an R package for clinical trial data quality control, designed to automate the detection of protocol deviations in clinical trials.

## Implemented Features

### Data Reading

#### `read_raw_data()` - Read Raw Data

Batch read SAS format files from specified directory with support for format mapping and IWRS data integration.

**Key Features:**
- Automatically scan and read all `.sas7bdat` files in the directory
- Support SAS format files with automatic code mapping
- Optional Excel format mapping file for additional format conversion
- Support reading IWRS (randomization information) CSV data
- Automatically standardize column names to uppercase
- Comprehensive error handling and warning messages

**Usage Examples:**

```r
library(pdchecker)

# Basic usage: read SAS files only
data <- read_raw_data(folder = "path/to/rawdata")

# Include IWRS data
data <- read_raw_data(
  folder = "path/to/rawdata",
  iwrs_file = "path/to/iwrs.csv"
)

# Use additional Excel format mapping file
data <- read_raw_data(
  folder = "path/to/rawdata",
  iwrs_file = "path/to/iwrs.csv",
  format_file = "path/to/format_mapping.xlsx"
)

# View loaded datasets
names(data)  # Display all dataset names
str(data$DM) # View DM dataset structure
```

**Parameters:**
- `folder`: Path to directory containing SAS data files (required)
- `iwrs_file`: Path to IWRS CSV file (optional, default: NULL)
  - First 2 rows of CSV file will be skipped
- `format_file`: Path to Excel format mapping file (optional, default: NULL)
  - Must contain columns: 表 (table), 变量 (variable), 编码值 (coded value), 编码说明 (label), 匹配状态 (match status)

**Return Value:**
- Returns a named list containing all successfully loaded datasets
- If IWRS file is provided, `IWRS` will be the first element
- Other datasets are named by filename (without extension), all converted to uppercase

#### `read_raw_data_with_formats()` - Read Data with Format Catalog

Read data using SAS format catalog file (`.sas7bcat`) with automatic format definition application.

**Usage Example:**

```r
# Use SAS format catalog file
data <- read_raw_data_with_formats(
  data_dir = "path/to/rawdata",
  catalog_file = "path/to/rawdata/formats.sas7bcat",
  iwrs_file = "path/to/iwrs.csv",
  encoding = "UTF-8"
)
```

**Parameters:**
- `data_dir`: Directory containing SAS data files (required)
- `catalog_file`: Path to SAS format catalog file (required, `.sas7bcat` file)
- `iwrs_file`: Path to IWRS CSV file (optional)
- `encoding`: Character encoding, default: "UTF-8"

**Features:**
- Automatically convert labelled data to factor types
- Format definitions from SAS catalog file
- Support custom character encoding (e.g., "GBK", "latin1", etc.)

## Data Requirements

### SAS Datasets
- Support standard `.sas7bdat` format files
- Format file name must contain "formats"

### IWRS Data
- CSV format
- First 2 rows are header rows (automatically skipped during reading)

### Excel Format Mapping File (Optional)
Must contain the following columns:
- **表** (Table): Dataset name
- **变量** (Variable): Variable name  
- **编码值** (Coded Value): Original coded value
- **编码说明** (Label): Label description
- **匹配状态** (Match Status): Must be "完全匹配" (exact match)

## Planned Features

The following features are under development:

- ✅ Data reading functionality
- 🚧 Informed consent missing and time deviation checks
- 🚧 Visit missing and window deviation checks
- 🚧 Laboratory test missing and window deviation checks
- 🚧 Inclusion criteria checks (age, laboratory parameters, etc.)
- 🚧 Exclusion criteria checks (pregnancy, liver function, virology, etc.)


## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("/pdchecker")
```

## Dependencies

- `haven`: Read SAS data files
- `dplyr`: Data manipulation
- `readxl`: Read Excel files
- `readr`: Read CSV files
- `lubridate`: Date and time processing
- `magrittr`: Pipe operator
- `rlang`: R language tools
- `tibble`: Modern data frames

## License

This project is licensed under Apache License 2.0.

## Notes

- All column names are converted to uppercase after processing
- Files that fail to read are excluded with warning messages
- Empty datasets are retained but warnings are displayed
- Date and time variables maintain their original format

---

**Development Status**: This package is under active development. API may change.
