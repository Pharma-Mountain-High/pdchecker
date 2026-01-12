# pdchecker

Protocol Deviation Checker for Clinical Trials

## Project Status

🚧 **Under Active Development**

| Module                   | Status         | Description                                         |
| ------------------------ | -------------- | --------------------------------------------------- |
| Data Reading             | ✅ Complete    | Read SAS datasets with format mapping               |
| Visit Schedule           | ✅ Complete    | Parse visit schedule and window periods             |
| Date Extraction          | ✅ Complete    | Extract key dates (first dose, last dose, EOT, EOS) |
| Planned Visit Generation | ✅ Complete    | Generate planned visit dates with window ranges     |
| Informed Consent Checks  | ✅ Complete    | Check ICF-related deviations                        |
| Visit Checks             | ✅ Complete    | Check missing visits and window deviations          |
| Test/Lab Checks          | 🚧 In Progress | Check missing tests and lab results                 |

## Overview

`pdchecker` is an R package designed to automate the detection of protocol deviations in clinical trials. It provides a comprehensive set of tools for clinical trial data quality control.

## Implemented Features

### 1. Data Reading Module

- **`read_raw_data()`** - Batch read SAS files with optional IWRS and Excel format mapping
- **`read_raw_data_with_formats()`** - Read SAS files using format catalog (`.sas7bcat`)

### 2. Visit Schedule Module

- **`read_visitcode_file()`** - Read visit schedule from Excel/CSV and parse window periods (e.g., `+/-3d`, `<=24h`)

### 3. Date Extraction Module

- **`get_first_dose_date()`** - Extract earliest dosing date per subject
- **`get_last_dose_date()`** - Extract latest dosing date per subject
- **`get_eot_date()`** - Extract end of treatment date
- **`get_eos_date()`** - Extract end of study date

### 4. Planned Visit Generation Module

- **`generate_planned_visit_dates()`** - Calculate planned visit dates and window ranges for each subject based on visit schedule and clinical data

### 5. Informed Consent Check Module

- **`check_screen_without_ic()`** - Identify subjects with screening visits but missing informed consent
- **`check_icf_time_deviation()`** - Detect study procedures performed before informed consent was obtained

### 6. Visit Check Module

- **`check_missing_visit()`** - Check for missing visits based on planned visit dates and cutoff criteria
- **`check_visit_window()`** - Check if completed visits were conducted within the specified visit window

### 7. Test/Lab Check Module (In Progress)

- **`prepare_test_data()`** - Prepare and standardize test data for missing test checks
- **`check_missing_test()`** - Check for missing tests at each visit

## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("your-username/pdchecker")
```

## Dependencies

- `haven` - Read SAS data files
- `dplyr` - Data manipulation
- `readxl` - Read Excel files
- `readr` - Read CSV files
- `lubridate` - Date and time processing
- `magrittr` - Pipe operator
- `rlang` - R language tools
- `tibble` - Modern data frames
- `purrr` - Functional programming tools

## License

This project is licensed under Apache License 2.0.

## Notes

- All column names are converted to uppercase after processing
- SAS missing values (NA, ".", "") are handled automatically
- Date variables maintain their original format
- Comprehensive error handling with informative warning messages

---

**Development Status**: This package is under active development. API may change.
