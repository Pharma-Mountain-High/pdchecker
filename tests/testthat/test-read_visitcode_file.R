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
