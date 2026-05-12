# Read Visit Schedule Data and Parse Window Periods

Read visit schedule Excel or CSV file and parse window period column
(WP) into two new variables:

- type: Distinguishes window types (\<=, +/-, +, -, range, etc.)

- wpvalue: Specific window time value (automatically converts hours to
  days)

Supported window period format examples:

- "+/-3d" -\> type: +/-, value: 3

- "\<=24h" -\> type: \<=, value: 1

- "+2d" -\> type: +, value: 2

- "-1d" -\> type: -, value: 1

- "1w" -\> type: +, value: 7 (1 week = 7 days)

- "+/-2w" -\> type: +/-, value: 14 (2 weeks = 14 days)

## Usage

``` r
read_visitcode_file(file_path, sheet_name = "Sheet1")
```

## Arguments

- file_path:

  Character. File path (.xlsx, .xls, or .csv). Must be a non-empty
  string pointing to an existing file.

- sheet_name:

  Character. Excel sheet name (default: "Sheet1"). Ignored for CSV
  files.

## Value

A tibble with all columns from input file, plus new (or overwritten)
columns:

- type:

  Window type (character): +/-, \<=, \>=, +, -, range, other, or NA

- wpvalue:

  Window value (numeric) in days. NA if unparseable (e.g., range or
  other types)

- visit_category:

  Visit category (character): screening, treatment, end_of_treatment,
  follow_up, or unknown. Only generated if CYCLE column exists.

## Details

### Window Period Column

The input file must contain a column named "WP" (case-sensitive).

### Time Unit Conversion

All time units are converted to days:

- Hours (h): divided by 24

- Days (d): unchanged

- Weeks (w): multiplied by 7

### Encoding Support

CSV files default to UTF-8 encoding, with fallback to system default.

### Column Name Conflicts

If type or wpvalue columns already exist, they will be overwritten with
a message.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read from Excel file
data <- read_visitcode_file("visit_schedule.xlsx")

# Read from specific sheet
data <- read_visitcode_file("visit_schedule.xlsx", sheet_name = "Schedule")

# Read from CSV file
data <- read_visitcode_file("visit_schedule.csv")

# Example output structure:
#   visit  WP      type   wpvalue
#   V1     +/-3d   +/-    3
#   V2     <=24h   <=     1
#   V3     +2d     +      2
#   V4     -1d     -      1
#   V5     1w      +      7
} # }
```
