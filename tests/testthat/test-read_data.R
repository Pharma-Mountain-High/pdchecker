# Test read_data.R functions
# =============================================================================

library(testthat)
library(withr)
library(writexl)

# Helper function: create temporary CSV file
create_temp_csv_file <- function(data, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  write.csv(data, temp_file, row.names = FALSE)
  temp_file
}

# Helper function: create temporary CSV file with 2 header rows (for iwrs)
create_temp_iwrs_csv <- function(data, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  # IWRS file has 2 header rows to skip, then a real header + data
  skip_row1 <- paste(rep("Header1", ncol(data)), collapse = ",")
  skip_row2 <- paste(rep("Header2", ncol(data)), collapse = ",")
  col_names <- paste(names(data), collapse = ",")
  data_lines <- apply(data, 1, function(x) paste(x, collapse = ","))
  writeLines(c(skip_row1, skip_row2, col_names, data_lines), temp_file)
  temp_file
}

# Helper function: create temporary Excel file
create_temp_excel_file <- function(data, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  writexl::write_xlsx(data, temp_file)
  temp_file
}

# =============================================================================
# Tests for read_raw_data() - Parameter validation
# =============================================================================

test_that("read_raw_data validates folder parameter", {
  expect_error(
    read_raw_data(123),
    "'folder' must be a single character string"
  )

  expect_error(
    read_raw_data(c("path1", "path2")),
    "'folder' must be a single character string"
  )
})

test_that("read_raw_data validates iwrs_file parameter", {
  temp_dir <- withr::local_tempdir()
  expect_error(
    read_raw_data(temp_dir, iwrs_file = 123),
    "'iwrs_file' must be NULL or a single character string"
  )

  expect_error(
    read_raw_data(temp_dir, iwrs_file = c("a.csv", "b.csv")),
    "'iwrs_file' must be NULL or a single character string"
  )

  # NULL is valid
  expect_warning(
    result <- read_raw_data(temp_dir, iwrs_file = NULL),
    "No SAS files found"
  )
  expect_type(result, "list")
})

test_that("read_raw_data validates format_file parameter", {
  temp_dir <- withr::local_tempdir()
  expect_error(
    read_raw_data(temp_dir, format_file = 123),
    "'format_file' must be NULL or a single character string"
  )

  expect_error(
    read_raw_data(temp_dir, format_file = c("a.xlsx", "b.xlsx")),
    "'format_file' must be NULL or a single character string"
  )
})

test_that("read_raw_data errors when directory does not exist", {
  expect_error(
    read_raw_data("nonexistent_directory_xyz123"),
    "Specified directory does not exist"
  )
})

# =============================================================================
# Tests for read_raw_data() - Empty folder (no SAS files)
# =============================================================================

test_that("read_raw_data returns empty list when folder has no SAS files", {
  temp_dir <- withr::local_tempdir()

  expect_warning(
    result <- read_raw_data(temp_dir),
    "No SAS files found"
  )

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("read_raw_data with empty folder and iwrs file returns list with IWRS only", {
  temp_dir <- withr::local_tempdir()
  iwrs_data <- data.frame(
    SUBJID = c("001", "002"),
    TRT = c("A", "B"),
    stringsAsFactors = FALSE
  )
  iwrs_file <- create_temp_iwrs_csv(iwrs_data, "iwrs.csv", dir = temp_dir)

  expect_warning(
    result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
    "No SAS files found"
  )

  expect_type(result, "list")
  expect_length(result, 1)
  expect_true("IWRS" %in% names(result))
  expect_equal(nrow(result$IWRS), 2)
  expect_true("SUBJID" %in% names(result$IWRS))
  expect_true("TRT" %in% names(result$IWRS))
})

test_that("read_raw_data messages when iwrs_file is NULL", {
  temp_dir <- withr::local_tempdir()

  expect_message(
    expect_warning(
      read_raw_data(temp_dir, iwrs_file = NULL),
      "No SAS files found"
    ),
    "No iwrs file specified"
  )
})

test_that("read_raw_data warns when iwrs_file does not exist", {
  temp_dir <- withr::local_tempdir()

  expect_warning(
    result <- read_raw_data(temp_dir, iwrs_file = "nonexistent_iwrs.csv"),
    "iwrs file does not exist"
  )

  expect_type(result, "list")
  expect_length(result, 0)
})

# =============================================================================
# Tests for read_raw_data() - IWRS file reading
# =============================================================================

test_that("read_raw_data reads iwrs CSV with 2 header rows skipped", {
  temp_dir <- withr::local_tempdir()
  iwrs_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    ARM = c("Treatment", "Placebo", "Treatment"),
    SITEID = c("001", "002", "001"),
    stringsAsFactors = FALSE
  )
  iwrs_file <- create_temp_iwrs_csv(iwrs_data, "iwrs.csv", dir = temp_dir)

  # 空文件夹 + iwrs：验证 iwrs 正确读取（跳过 2 行表头）
  expect_warning(
    result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
    "No SAS files found"
  )

  expect_true("IWRS" %in% names(result))
  expect_equal(nrow(result$IWRS), 3)
  expect_equal(result$IWRS$SUBJID, c("001", "002", "003"))
  expect_true(all(names(result$IWRS) == toupper(names(result$IWRS))))
})

test_that("read_raw_data warns when iwrs file is empty", {
  temp_dir <- withr::local_tempdir()
  # 创建空的 iwrs 文件（2 行跳过 + 列名行，无数据行）
  iwrs_file <- file.path(temp_dir, "iwrs.csv")
  writeLines(
    c("Header1,Header2", "Header1,Header2", "SUBJID,ARM"),
    iwrs_file
  )

  expect_warning(
    result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
    "iwrs file is empty"
  )

  expect_true("IWRS" %in% names(result))
  expect_equal(nrow(result$IWRS), 0)
})

# =============================================================================
# Tests for read_raw_data() - format_file warning
# =============================================================================

test_that("read_raw_data warns when format_file is not xlsx", {
  temp_dir <- withr::local_tempdir()
  format_file <- file.path(temp_dir, "formats.txt")
  writeLines("test", format_file)

  expect_warning(
    read_raw_data(temp_dir, format_file = format_file),
    "format_file should be an Excel file"
  )
})

# =============================================================================
# Tests for read_raw_data_with_formats() - Parameter validation
# =============================================================================

test_that("read_raw_data_with_formats validates data_dir parameter", {
  expect_error(
    read_raw_data_with_formats(123, "catalog.sas7bcat"),
    "'data_dir' must be a single character string"
  )

  expect_error(
    read_raw_data_with_formats(c("path1", "path2"), "catalog.sas7bcat"),
    "'data_dir' must be a single character string"
  )
})

test_that("read_raw_data_with_formats validates catalog_file parameter", {
  temp_dir <- withr::local_tempdir()
  expect_error(
    read_raw_data_with_formats(temp_dir, 123),
    "'catalog_file' must be a single character string"
  )

  expect_error(
    read_raw_data_with_formats(temp_dir, c("a.sas7bcat", "b.sas7bcat")),
    "'catalog_file' must be a single character string"
  )
})

test_that("read_raw_data_with_formats validates iwrs_file parameter", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  expect_error(
    read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = 123),
    "'iwrs_file' must be NULL or a single character string"
  )

  unlink(catalog_file)
})

test_that("read_raw_data_with_formats validates encoding parameter", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  expect_error(
    read_raw_data_with_formats(temp_dir, catalog_file, encoding = 123),
    "'encoding' must be a single character string"
  )

  unlink(catalog_file)
})

test_that("read_raw_data_with_formats errors when data_dir does not exist", {
  expect_error(
    read_raw_data_with_formats("nonexistent_dir", "catalog.sas7bcat"),
    "Data directory does not exist"
  )
})

test_that("read_raw_data_with_formats errors when catalog_file does not exist", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    read_raw_data_with_formats(temp_dir, "nonexistent.sas7bcat"),
    "Format catalog file does not exist"
  )
})

test_that("read_raw_data_with_formats errors when catalog_file is not .sas7bcat", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.txt")
  writeLines("dummy", catalog_file)

  expect_error(
    read_raw_data_with_formats(temp_dir, catalog_file),
    "Format catalog file must be a .sas7bcat file"
  )

  unlink(catalog_file)
})

# =============================================================================
# Tests for read_raw_data_with_formats() - Empty folder
# =============================================================================

test_that("read_raw_data_with_formats returns empty list when no SAS files", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  expect_warning(
    result <- read_raw_data_with_formats(temp_dir, catalog_file),
    "No SAS data files found"
  )

  expect_type(result, "list")
  expect_length(result, 0)

  unlink(catalog_file)
})

test_that("read_raw_data_with_formats with iwrs only when no SAS files", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  iwrs_data <- data.frame(
    SUBJID = c("001", "002"),
    TRT = c("A", "B"),
    stringsAsFactors = FALSE
  )
  iwrs_file <- create_temp_iwrs_csv(iwrs_data, "iwrs.csv", dir = temp_dir)

  expect_warning(
    result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file),
    "No SAS data files found"
  )

  expect_length(result, 1)
  expect_true("IWRS" %in% names(result))
  expect_equal(nrow(result$IWRS), 2)

  unlink(catalog_file)
})

test_that("read_raw_data_with_formats messages when iwrs_file is NULL", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  expect_message(
    expect_warning(
      read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = NULL),
      "No SAS data files found"
    ),
    "No iwrs file specified"
  )

  unlink(catalog_file)
})

test_that("read_raw_data_with_formats warns when iwrs_file does not exist", {
  temp_dir <- withr::local_tempdir()
  catalog_file <- file.path(temp_dir, "formats.sas7bcat")
  writeLines("dummy", catalog_file)

  expect_warning(
    result <- read_raw_data_with_formats(
      temp_dir, catalog_file,
      iwrs_file = "nonexistent_iwrs.csv"
    ),
    "iwrs file does not exist"
  )

  expect_length(result, 0)

  unlink(catalog_file)
})
