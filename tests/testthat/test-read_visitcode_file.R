library(testthat)
library(tibble)
library(readxl)

# Helper function: create temporary Excel file
create_temp_excel_file <- function(data, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  writexl::write_xlsx(data, temp_file)
  temp_file
}

# Helper function: create temporary CSV file
create_temp_csv_file <- function(data, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  write.csv(data, temp_file, row.names = FALSE)
  temp_file
}

# ============================================================================
# Tests for parse_window_period() function
# ============================================================================

test_that("parse_window_period handles missing values", {
  result <- pdchecker:::parse_window_period(NA)
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))

  result <- pdchecker:::parse_window_period("")
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))

  result <- pdchecker:::parse_window_period(NULL)
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))
})

test_that("parse_window_period parses ± type", {
  result <- pdchecker:::parse_window_period("±3d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("±24h")
  expect_equal(result$type, "±")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("±2周")
  expect_equal(result$type, "±")
  expect_equal(result$value, 14)

  result <- pdchecker:::parse_window_period("±1w")
  expect_equal(result$type, "±")
  expect_equal(result$value, 7)
})

test_that("parse_window_period parses ≤ type", {
  result <- pdchecker:::parse_window_period("≤24h")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("≤1d")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("<=2天")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 2)
})

test_that("parse_window_period parses ≥ type", {
  result <- pdchecker:::parse_window_period("≥1d")
  expect_equal(result$type, "≥")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period(">=3天")
  expect_equal(result$type, "≥")
  expect_equal(result$value, 3)
})

test_that("parse_window_period parses + type", {
  result <- pdchecker:::parse_window_period("+2d")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)

  result <- pdchecker:::parse_window_period("+3天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("+48h")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)
})

test_that("parse_window_period parses - type", {
  result <- pdchecker:::parse_window_period("-1d")
  expect_equal(result$type, "-")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("-2天")
  expect_equal(result$type, "-")
  expect_equal(result$value, 2)
})

test_that("parse_window_period parses range type", {
  result <- pdchecker:::parse_window_period("-2到+4")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-2到+4")

  result <- pdchecker:::parse_window_period("1至3天")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "1至3天")
})

test_that("parse_window_period parses numeric without prefix", {
  result <- pdchecker:::parse_window_period("2d")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)

  result <- pdchecker:::parse_window_period("3天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("1w")
  expect_equal(result$type, "+")
  expect_equal(result$value, 7)
})

test_that("parse_window_period converts time units", {
  # Hours to days
  result <- pdchecker:::parse_window_period("±24h")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("±12小时")
  expect_equal(result$value, 0.5)

  # Weeks to days
  result <- pdchecker:::parse_window_period("±1w")
  expect_equal(result$value, 7)

  result <- pdchecker:::parse_window_period("±2周")
  expect_equal(result$value, 14)

  # Days
  result <- pdchecker:::parse_window_period("±3d")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("±4天")
  expect_equal(result$value, 4)

  result <- pdchecker:::parse_window_period("±5日")
  expect_equal(result$value, 5)
})

test_that("parse_window_period handles other formats", {
  result <- pdchecker:::parse_window_period("固定访视")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "固定访视")
})

test_that("parse_window_period handles input with whitespace", {
  result <- pdchecker:::parse_window_period("  ±3d  ")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)
})

test_that("parse_window_period handles decimal time units", {
  # Decimal days
  result <- pdchecker:::parse_window_period("±1.5d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 1.5)

  # Decimal hours
  result <- pdchecker:::parse_window_period("±6h")
  expect_equal(result$type, "±")
  expect_equal(result$value, 0.25)

  # Decimal weeks
  result <- pdchecker:::parse_window_period("+0.5w")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3.5)
})

test_that("parse_window_period handles zero values", {
  result <- pdchecker:::parse_window_period("±0d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 0)

  result <- pdchecker:::parse_window_period("0天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 0)
})

test_that("parse_window_period handles large values", {
  result <- pdchecker:::parse_window_period("±100d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 100)

  result <- pdchecker:::parse_window_period("±52w")
  expect_equal(result$type, "±")
  expect_equal(result$value, 364)
})

test_that("parse_window_period handles invalid numeric values", {
  # Invalid values produce NA with warning
  # Use regex to match both English and Chinese warning messages
  expect_warning(
    result <- pdchecker:::parse_window_period("±abc天"),
    "NAs introduced by coercion|强制改变过程中产生了NA"
  )
  expect_equal(result$type, "±")
  expect_true(is.na(result$value))

  expect_warning(
    result <- pdchecker:::parse_window_period("≤xyz小时"),
    "NAs introduced by coercion|强制改变过程中产生了NA"
  )
  expect_equal(result$type, "≤")
  expect_true(is.na(result$value))
})

test_that("parse_window_period handles different date unit notations", {
  # Test "日" as unit
  result <- pdchecker:::parse_window_period("±3日")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)

  # Test "小时" as unit
  result <- pdchecker:::parse_window_period("+6小时")
  expect_equal(result$type, "+")
  expect_equal(result$value, 0.25)
})

test_that("parse_window_period handles pure numbers without units", {
  result <- pdchecker:::parse_window_period("7")
  expect_equal(result$type, "+")
  expect_equal(result$value, 7)

  result <- pdchecker:::parse_window_period("+5")
  expect_equal(result$type, "+")
  expect_equal(result$value, 5)
})

test_that("parse_window_period handles negative with range keywords", {
  # Contains "到" or "至" should be identified as range, not negative
  result <- pdchecker:::parse_window_period("-3到+7")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-3到+7")

  result <- pdchecker:::parse_window_period("-1至+1")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-1至+1")
})

test_that("parse_window_period handles various other formats", {
  result <- pdchecker:::parse_window_period("固定")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "固定")

  result <- pdchecker:::parse_window_period("不限")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "不限")

  result <- pdchecker:::parse_window_period("NA")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "NA")
})

# ============================================================================
# Tests for read_visitcode_file() function
# ============================================================================

test_that("read_visitcode_file reads Excel file", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3"),
    visit_name = c("Screening", "Baseline", "Follow-up 1"),
    WP = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_visitcode.xlsx")

  result <- read_visitcode_file(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("type" %in% names(result))
  expect_true("wpvalue" %in% names(result))

  expect_equal(result$type, c("±", "≤", "+"))
  expect_equal(result$wpvalue, c(3, 1, 2))

  unlink(temp_file)
})

test_that("read_visitcode_file reads CSV file", {
  test_data <- data.frame(
    visit_code = c("V1", "V2"),
    visit_name = c("Screening", "Baseline"),
    WP = c("-1d", "±2周"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_visitcode.csv")

  result <- read_visitcode_file(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$type, c("-", "±"))
  expect_equal(result$wpvalue, c(1, 14))

  unlink(temp_file)
})

test_that("read_visitcode_file handles data with NA values", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3"),
    WP = c("±3d", NA, ""),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_with_na.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type[1], "±")
  expect_true(is.na(result$type[2]))
  expect_true(is.na(result$type[3]))

  unlink(temp_file)
})

test_that("read_visitcode_file handles empty file with warning", {
  test_data <- data.frame(
    visit_code = character(0),
    WP = character(0),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_empty.csv")

  expect_warning(
    result <- read_visitcode_file(temp_file),
    "no data rows"
  )

  expect_equal(nrow(result), 0)
  expect_true("type" %in% names(result))
  expect_true("wpvalue" %in% names(result))

  unlink(temp_file)
})

test_that("read_visitcode_file errors when file not found", {
  expect_error(
    read_visitcode_file("nonexistent_file.xlsx"),
    "File not found"
  )
})

test_that("read_visitcode_file errors on unsupported file format", {
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "test.txt")
  writeLines("test", temp_file)

  expect_error(
    read_visitcode_file(temp_file),
    "Unsupported file format"
  )
})

test_that("read_visitcode_file errors when WP column is missing", {
  test_data <- data.frame(
    visit_code = c("V1", "V2"),
    visit_name = c("Screening", "Baseline"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_no_wp.csv")

  expect_error(
    read_visitcode_file(temp_file),
    "Missing required column 'WP'"
  )

  unlink(temp_file)
})

test_that("read_visitcode_file reads specified sheet", {
  test_data <- data.frame(
    visit_code = c("V1"),
    WP = c("±3d"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_sheet.xlsx")

  result <- read_visitcode_file(temp_file, sheet_name = "Sheet1")

  expect_equal(nrow(result), 1)
  expect_equal(result$type[1], "±")

  unlink(temp_file)
})

test_that("read_visitcode_file complete scenario test", {
  test_data <- data.frame(
    visit_code = c("V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"),
    visit_name = c(
      "Screening", "Baseline", "Visit 1", "Visit 2", "Visit 3",
      "Visit 4", "Visit 5", "Visit 6", "Visit 7"
    ),
    WP = c("±3d", "≤24h", "+2天", "-1d", "1w", "±2周", "≥1d", "-2到+4", "固定"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_complete.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 9)
  expect_equal(result$type, c("±", "≤", "+", "-", "+", "±", "≥", "范围", "其他"))
  expect_equal(result$wpvalue, c(3, 1, 2, 1, 7, 14, 1, NA, NA))

  # Verify original columns are preserved
  expect_true("visit_code" %in% names(result))
  expect_true("visit_name" %in% names(result))
  expect_true("WP" %in% names(result))

  unlink(temp_file)
})

test_that("read_visitcode_file handles column name conflicts", {
  test_data <- data.frame(
    visit_code = c("V1", "V2"),
    WP = c("±3d", "≤24h"),
    type = c("old_type1", "old_type2"),
    wpvalue = c("old_value1", "old_value2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_conflict.csv")

  # Should output message about overwriting
  expect_message(
    result <- read_visitcode_file(temp_file),
    "'type' column will be overwritten"
  )

  expect_message(
    result <- read_visitcode_file(temp_file),
    "'wpvalue' column will be overwritten"
  )

  # Verify columns are overwritten with newly parsed values
  expect_equal(result$type, c("±", "≤"))
  expect_equal(result$wpvalue, c(3, 1))

  unlink(temp_file)
})

test_that("read_visitcode_file handles mixed window period types", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3", "V4", "V5", "V6", "V7"),
    WP = c("±3d", "≤24h", "≥7天", "+2w", "-1d", "1至3天", "固定"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_mixed.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type, c("±", "≤", "≥", "+", "-", "范围", "其他"))
  expect_equal(result$wpvalue, c(3, 1, 7, 14, 1, NA, NA))

  unlink(temp_file)
})

test_that("read_visitcode_file handles decimal window periods", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3"),
    WP = c("±1.5d", "≤0.5天", "+2.5w"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_decimal.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type, c("±", "≤", "+"))
  expect_equal(result$wpvalue, c(1.5, 0.5, 17.5))

  unlink(temp_file)
})

test_that("read_visitcode_file preserves original column integrity", {
  test_data <- data.frame(
    visit_code = c("V1", "V2"),
    visit_name = c("Screening", "Baseline"),
    visit_date = c("Day 1", "Day 7"),
    WP = c("±3d", "≤24h"),
    notes = c("Note 1", "Note 2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_integrity.csv")

  result <- read_visitcode_file(temp_file)

  # Verify all original columns exist
  expect_true(all(c("visit_code", "visit_name", "visit_date", "WP", "notes") %in% names(result)))

  # Verify new columns exist
  expect_true(all(c("type", "wpvalue") %in% names(result)))

  # Verify original data is not modified
  expect_equal(result$visit_code, c("V1", "V2"))
  expect_equal(result$visit_name, c("Screening", "Baseline"))
  expect_equal(result$notes, c("Note 1", "Note 2"))

  unlink(temp_file)
})

test_that("read_visitcode_file handles mixed invalid data", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3", "V4"),
    WP = c("±3d", NA, "invalid", "≤24h"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_mixed_invalid.csv")

  result <- read_visitcode_file(temp_file)

  # V1 parsed normally
  expect_equal(result$type[1], "±")
  expect_equal(result$wpvalue[1], 3)

  # V2 is NA
  expect_true(is.na(result$type[2]))
  expect_true(is.na(result$wpvalue[2]))

  # V3 unrecognized, classified as "其他", wpvalue is NA (non-numeric)
  expect_equal(result$type[3], "其他")
  expect_true(is.na(result$wpvalue[3]))

  # V4 parsed normally
  expect_equal(result$type[4], "≤")
  expect_equal(result$wpvalue[4], 1)

  unlink(temp_file)
})

test_that("read_visitcode_file handles large dataset", {
  test_data <- data.frame(
    visit_code = paste0("V", 1:100),
    WP = rep(c("±3d", "≤24h", "+2天", "-1d", "1w"), 20),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_large.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 100)
  expect_true(all(c("type", "wpvalue") %in% names(result)))

  # Verify pattern of parsed results
  expected_types <- rep(c("±", "≤", "+", "-", "+"), 20)
  expect_equal(result$type, expected_types)

  unlink(temp_file)
})

test_that("read_visitcode_file handles single row file", {
  test_data <- data.frame(
    visit_code = "V1",
    WP = "±3d",
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_single_row.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 1)
  expect_equal(result$type[1], "±")
  expect_equal(result$wpvalue[1], 3)

  unlink(temp_file)
})

test_that("read_visitcode_file handles special characters in visit names", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3"),
    visit_name = c("Visit 1 (Baseline)", "Follow-up - Week 2", "End Visit/Exit"),
    WP = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_special_chars.csv")

  result <- read_visitcode_file(temp_file)

  # Verify special characters don't affect parsing
  expect_equal(nrow(result), 3)
  expect_equal(result$type, c("±", "≤", "+"))

  # Verify visit names are preserved
  expect_equal(result$visit_name[1], "Visit 1 (Baseline)")

  unlink(temp_file)
})

test_that("read_visitcode_file Excel and CSV results are consistent", {
  test_data <- data.frame(
    visit_code = c("V1", "V2", "V3"),
    WP = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  excel_file <- create_temp_excel_file(test_data, "test_consistency.xlsx")
  csv_file <- create_temp_csv_file(test_data, "test_consistency.csv")

  result_excel <- read_visitcode_file(excel_file)
  result_csv <- read_visitcode_file(csv_file)

  # Verify both file formats produce consistent results
  expect_equal(result_excel$type, result_csv$type)
  expect_equal(result_excel$wpvalue, result_csv$wpvalue)

  unlink(excel_file)
  unlink(csv_file)
})

# ============================================================================
# Tests for parameter validation
# ============================================================================

test_that("read_visitcode_file validates file_path parameter", {
  # file_path must be character
  expect_error(
    read_visitcode_file(123),
    "'file_path' must be a single character string"
  )

  # file_path must be length 1
  expect_error(
    read_visitcode_file(c("file1.xlsx", "file2.xlsx")),
    "'file_path' must be a single character string"
  )

  # file_path cannot be NA
  expect_error(
    read_visitcode_file(NA_character_),
    "'file_path' cannot be NA or empty"
  )

  # file_path cannot be empty
  expect_error(
    read_visitcode_file(""),
    "'file_path' cannot be NA or empty"
  )

  # file_path cannot be whitespace only
  expect_error(
    read_visitcode_file("   "),
    "'file_path' cannot be NA or empty"
  )
})

test_that("read_visitcode_file validates sheet_name parameter", {
  test_data <- data.frame(
    visit_code = c("V1"),
    WP = c("±3d"),
    stringsAsFactors = FALSE
  )
  temp_file <- create_temp_excel_file(test_data, "test_sheet_validation.xlsx")

  # sheet_name must be character
  expect_error(
    read_visitcode_file(temp_file, sheet_name = 123),
    "'sheet_name' must be a single character string"
  )

  # sheet_name must be length 1
  expect_error(
    read_visitcode_file(temp_file, sheet_name = c("Sheet1", "Sheet2")),
    "'sheet_name' must be a single character string"
  )

  # sheet_name cannot be NA
  expect_error(
    read_visitcode_file(temp_file, sheet_name = NA_character_),
    "'sheet_name' cannot be NA or empty"
  )

  # sheet_name cannot be empty
  expect_error(
    read_visitcode_file(temp_file, sheet_name = ""),
    "'sheet_name' cannot be NA or empty"
  )

  unlink(temp_file)
})
