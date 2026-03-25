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
# Tests for read_testconfig_file() function - Basic reading
# ============================================================================

test_that("read_testconfig_file reads Excel file", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry"),
    VISITNUM = c("1", "2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_config.xlsx")

  result <- read_testconfig_file(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("TESTCAT" %in% names(result))
  expect_true("VISITNUM" %in% names(result))
  expect_equal(result$TESTCAT, c("CBC", "Chemistry"))
  expect_equal(result$VISITNUM, c("1", "2"))

  unlink(temp_file)
})

test_that("read_testconfig_file reads CSV file", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry"),
    VISITNUM = c("1", "2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_config.csv")

  result <- read_testconfig_file(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$TESTCAT, c("CBC", "Chemistry"))

  unlink(temp_file)
})

# ============================================================================
# Tests for VISITNUM expansion
# ============================================================================

test_that("read_testconfig_file expands comma-separated VISITNUM", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry"),
    VISITNUM = c("1,2,3", "4,5"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_expand.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 5)
  expect_equal(result$TESTCAT, c("CBC", "CBC", "CBC", "Chemistry", "Chemistry"))
  expect_equal(result$VISITNUM, c("1", "2", "3", "4", "5"))

  unlink(temp_file)
})

test_that("read_testconfig_file expands Chinese comma-separated VISITNUM", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1，2，3"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_chinese_comma.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 3)
  expect_equal(result$VISITNUM, c("1", "2", "3"))

  unlink(temp_file)
})

test_that("read_testconfig_file preserves other columns during expansion", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1,2"),
    Description = c("Blood count test"),
    Priority = c("High"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_preserve_cols.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 2)
  expect_true("Description" %in% names(result))
  expect_true("Priority" %in% names(result))
  expect_equal(result$Description, c("Blood count test", "Blood count test"))
  expect_equal(result$Priority, c("High", "High"))

  unlink(temp_file)
})

test_that("read_testconfig_file handles mixed single and multi VISITNUM", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry", "Urinalysis"),
    VISITNUM = c("1,2,3", "4", "5,6"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_mixed.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 6)
  expect_equal(result$TESTCAT, c("CBC", "CBC", "CBC", "Chemistry", "Urinalysis", "Urinalysis"))
  expect_equal(result$VISITNUM, c("1", "2", "3", "4", "5", "6"))

  unlink(temp_file)
})

# ============================================================================
# Tests for visitcode parameter
# ============================================================================

test_that("read_testconfig_file joins VISIT from visitcode", {
  config_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1,2"),
    stringsAsFactors = FALSE
  )

  visitcode_data <- data.frame(
    VISITNUM = c("1", "2", "3"),
    VISIT = c("Screening", "Baseline", "Week 4"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(config_data, "test_join.csv")

  result <- read_testconfig_file(temp_file, visitcode = visitcode_data)

  expect_equal(nrow(result), 2)
  expect_true("VISIT" %in% names(result))
  expect_equal(result$VISIT, c("Screening", "Baseline"))

  unlink(temp_file)
})

test_that("read_testconfig_file reorders columns when visitcode provided", {
  config_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1"),
    Description = c("Test"),
    stringsAsFactors = FALSE
  )

  visitcode_data <- data.frame(
    VISITNUM = c("1"),
    VISIT = c("Screening"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(config_data, "test_reorder.csv")

  result <- read_testconfig_file(temp_file, visitcode = visitcode_data)

  # First three columns should be TESTCAT, VISITNUM, VISIT
  expect_equal(names(result)[1:3], c("TESTCAT", "VISITNUM", "VISIT"))

  unlink(temp_file)
})

test_that("read_testconfig_file handles unmatched VISITNUM in join", {
  config_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1,99"),
    stringsAsFactors = FALSE
  )

  visitcode_data <- data.frame(
    VISITNUM = c("1", "2"),
    VISIT = c("Screening", "Baseline"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(config_data, "test_unmatched.csv")

  result <- read_testconfig_file(temp_file, visitcode = visitcode_data)

  expect_equal(nrow(result), 2)
  expect_equal(result$VISIT[1], "Screening")
  expect_true(is.na(result$VISIT[2]))

  unlink(temp_file)
})

test_that("read_testconfig_file works without visitcode", {
  config_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1,2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(config_data, "test_no_visitcode.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 2)
  expect_false("VISIT" %in% names(result))

  unlink(temp_file)
})

# ============================================================================
# Tests for error handling
# ============================================================================

test_that("read_testconfig_file errors when file not found", {
  expect_error(
    read_testconfig_file("nonexistent_file.xlsx"),
    "File not found"
  )
})

test_that("read_testconfig_file errors on unsupported file format", {
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "test.txt")
  writeLines("test", temp_file)

  expect_error(
    read_testconfig_file(temp_file),
    "Unsupported file format"
  )
})

test_that("read_testconfig_file errors when TESTCAT column is missing", {
  test_data <- data.frame(
    Category = c("CBC", "Chemistry"),
    VISITNUM = c("1", "2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_no_testcat.csv")

  expect_error(
    read_testconfig_file(temp_file),
    "Missing required column 'TESTCAT'"
  )

  unlink(temp_file)
})

test_that("read_testconfig_file errors when VISITNUM column is missing", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry"),
    VisitNumber = c("1", "2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_no_visitnum.csv")

  expect_error(
    read_testconfig_file(temp_file),
    "Missing required column 'VISITNUM'"
  )

  unlink(temp_file)
})

test_that("read_testconfig_file handles empty file with warning", {
  test_data <- data.frame(
    TESTCAT = character(0),
    VISITNUM = character(0),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_empty.csv")

  expect_warning(
    result <- read_testconfig_file(temp_file),
    "no data rows"
  )

  expect_equal(nrow(result), 0)

  unlink(temp_file)
})

# ============================================================================
# Tests for parameter validation
# ============================================================================

test_that("read_testconfig_file validates file_path parameter", {
  expect_error(
    read_testconfig_file(123),
    "'file_path' must be a single character string"
  )

  expect_error(
    read_testconfig_file(c("file1.xlsx", "file2.xlsx")),
    "'file_path' must be a single character string"
  )

  expect_error(
    read_testconfig_file(NA_character_),
    "'file_path' cannot be NA or empty"
  )

  expect_error(
    read_testconfig_file(""),
    "'file_path' cannot be NA or empty"
  )

  expect_error(
    read_testconfig_file("   "),
    "'file_path' cannot be NA or empty"
  )
})

test_that("read_testconfig_file validates sheet_name parameter", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1"),
    stringsAsFactors = FALSE
  )
  temp_file <- create_temp_excel_file(test_data, "test_sheet_validation.xlsx")

  expect_error(
    read_testconfig_file(temp_file, sheet_name = 123),
    "'sheet_name' must be a single character string"
  )

  expect_error(
    read_testconfig_file(temp_file, sheet_name = c("Sheet1", "Sheet2")),
    "'sheet_name' must be a single character string"
  )

  expect_error(
    read_testconfig_file(temp_file, sheet_name = NA_character_),
    "'sheet_name' cannot be NA or empty"
  )

  expect_error(
    read_testconfig_file(temp_file, sheet_name = ""),
    "'sheet_name' cannot be NA or empty"
  )

  unlink(temp_file)
})

test_that("read_testconfig_file validates visitcode parameter", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1"),
    stringsAsFactors = FALSE
  )
  temp_file <- create_temp_csv_file(test_data, "test_visitcode_validation.csv")

  # visitcode must be a data frame
  expect_error(
    read_testconfig_file(temp_file, visitcode = "not a data frame"),
    "'visitcode' must be a data frame"
  )

  # visitcode must contain VISITNUM
  bad_visitcode <- data.frame(
    VisitNum = c("1"),
    VISIT = c("Screening"),
    stringsAsFactors = FALSE
  )
  expect_error(
    read_testconfig_file(temp_file, visitcode = bad_visitcode),
    "'visitcode' must contain VISITNUM column"
  )

  # visitcode must contain VISIT
  bad_visitcode2 <- data.frame(
    VISITNUM = c("1"),
    VisitName = c("Screening"),
    stringsAsFactors = FALSE
  )
  expect_error(
    read_testconfig_file(temp_file, visitcode = bad_visitcode2),
    "'visitcode' must contain VISIT column"
  )

  unlink(temp_file)
})

# ============================================================================
# Tests for sheet_name parameter
# ============================================================================

test_that("read_testconfig_file reads specified sheet", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_sheet.xlsx")

  result <- read_testconfig_file(temp_file, sheet_name = "Sheet1")

  expect_equal(nrow(result), 1)
  expect_equal(result$TESTCAT[1], "CBC")

  unlink(temp_file)
})

# ============================================================================
# Tests for edge cases
# ============================================================================

test_that("read_testconfig_file handles single row file", {
  test_data <- data.frame(
    TESTCAT = "CBC",
    VISITNUM = "1",
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_single_row.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 1)
  expect_equal(result$TESTCAT[1], "CBC")
  expect_equal(result$VISITNUM[1], "1")

  unlink(temp_file)
})

test_that("read_testconfig_file handles numeric VISITNUM", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c(1),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_numeric.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(result$VISITNUM[1], "1")
  expect_is(result$VISITNUM, "character")

  unlink(temp_file)
})

test_that("read_testconfig_file handles large dataset", {
  test_data <- data.frame(
    TESTCAT = rep(c("CBC", "Chemistry", "Urinalysis"), 50),
    VISITNUM = rep(c("1,2,3", "4,5", "6"), 50),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_large.csv")

  result <- read_testconfig_file(temp_file)

  # Each row expands: 3 + 2 + 1 = 6 rows per group, 50 groups = 300 rows
  expect_equal(nrow(result), 300)

  unlink(temp_file)
})

test_that("read_testconfig_file Excel and CSV results are consistent", {
  test_data <- data.frame(
    TESTCAT = c("CBC", "Chemistry"),
    VISITNUM = c("1,2", "3"),
    stringsAsFactors = FALSE
  )

  excel_file <- create_temp_excel_file(test_data, "test_consistency.xlsx")
  csv_file <- create_temp_csv_file(test_data, "test_consistency.csv")

  result_excel <- read_testconfig_file(excel_file)
  result_csv <- read_testconfig_file(csv_file)

  expect_equal(result_excel$TESTCAT, result_csv$TESTCAT)
  expect_equal(result_excel$VISITNUM, result_csv$VISITNUM)

  unlink(excel_file)
  unlink(csv_file)
})

test_that("read_testconfig_file handles VISITNUM with spaces", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1, 2, 3"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_spaces.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 3)
  expect_equal(result$VISITNUM, c("1", "2", "3"))

  unlink(temp_file)
})

test_that("read_testconfig_file handles decimal VISITNUM", {
  test_data <- data.frame(
    TESTCAT = c("CBC"),
    VISITNUM = c("1.1,1.2,2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_decimal.csv")

  result <- read_testconfig_file(temp_file)

  expect_equal(nrow(result), 3)
  expect_equal(result$VISITNUM, c("1.1", "1.2", "2"))

  unlink(temp_file)
})
