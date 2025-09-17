library(testthat)
library(haven)
library(dplyr)
library(readr)

# Mock data and helper functions for testing
create_temp_sas_file <- function(data, filename) {
  temp_file <- file.path(tempdir(), filename)
  # Since we can't easily create SAS files in tests, we'll mock the haven::read_sas function
  temp_file
}

create_temp_csv_file <- function(data, filename) {
  temp_file <- file.path(tempdir(), filename)
  write_csv(data, temp_file)
  temp_file
}

test_that("read_raw_data validates directory existence", {
  expect_error(
    read_raw_data("nonexistent_directory"),
    "Specified directory does not exist: nonexistent_directory"
  )
})

test_that("read_raw_data handles empty directory", {
  temp_dir <- tempdir()
  # Clean up any existing SAS files in temp directory
  existing_sas <- list.files(temp_dir, pattern = "\\.sas7bdat$", full.names = TRUE)
  file.remove(existing_sas)
  
  # Create a temporary IWRS file
  iwrs_data <- data.frame(
    SUBJID = c("001", "002"),
    RANDDT = c("2023-01-01", "2023-01-02")
  )
  iwrs_file <- create_temp_csv_file(iwrs_data, "iwrs.csv")
  
  # Mock the function to avoid actual file operations
  with_mock(
    `haven::read_sas` = function(...) stop("No format file"),
    expect_warning(
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
      "No SAS files found in the specified directory"
    )
  )
})

test_that("read_raw_data processes IWRS file correctly", {
  temp_dir <- tempdir()
  
  # Create test IWRS data with header rows
  iwrs_content <- c(
    "Header line 1",
    "Header line 2", 
    "SUBJID,RANDDT,TREATMENT",
    "001,2023-01-01,Active",
    "002,2023-01-02,Placebo"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)
  
  # Mock SAS file reading to avoid actual SAS files
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(character(0))  # No SAS files
      }
      return(character(0))
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "No SAS files found"
      )
      
      expect_true("IWRS" %in% names(result))
      expect_equal(nrow(result$IWRS), 2)
      expect_equal(result$IWRS$SUBJID, c("001", "002"))
    }
  )
  
  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles file reading errors gracefully", {
  temp_dir <- tempdir()
  
  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")
  
  # Mock to simulate SAS files and reading errors
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("test1.sas7bdat", "formats.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      stop("Simulated read error")
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "Failed to read file"
      )
      
      # Should still return IWRS data
      expect_true("IWRS" %in% names(result))
    }
  )
  
  # Clean up
  unlink(iwrs_file)
})

# Tests for read_raw_data_with_formats
test_that("read_raw_data_with_formats validates inputs", {
  expect_error(
    read_raw_data_with_formats("nonexistent_dir", "catalog.sas7bcat"),
    "Data directory does not exist: nonexistent_dir"
  )
  
  temp_dir <- tempdir()
  expect_error(
    read_raw_data_with_formats(temp_dir, "nonexistent_catalog.sas7bcat"),
    "Format catalog file does not exist: nonexistent_catalog.sas7bcat"
  )
  
  # Create a temporary file with wrong extension
  temp_file <- file.path(temp_dir, "wrong_ext.txt")
  file.create(temp_file)
  expect_error(
    read_raw_data_with_formats(temp_dir, temp_file),
    "Format catalog file must be a .sas7bcat file"
  )
  unlink(temp_file)
})

test_that("read_raw_data_with_formats handles no SAS files", {
  temp_dir <- tempdir()
  
  # Create a mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)
  
  # Mock to return no SAS files
  with_mock(
    `list.files` = function(path, pattern, ...) character(0),
    {
      expect_error(
        read_raw_data_with_formats(temp_dir, catalog_file),
        "No SAS data files found in directory"
      )
    }
  )
  
  # Clean up
  unlink(catalog_file)
})

test_that("read_raw_data_with_formats processes files with catalog", {
  temp_dir <- tempdir()
  
  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)
  
  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)
  
  # Mock SAS file operations
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("test_data.sas7bdat")
      }
      return(character(0))
    },
    `haven::read_sas` = function(data_file, catalog_file, ...) {
      # Return mock data with labelled columns
      df <- data.frame(
        subjid = c("001", "002"),
        status = structure(c(1, 2), 
                          labels = c("Active" = 1, "Inactive" = 2),
                          class = c("haven_labelled", "numeric"))
      )
      return(df)
    },
    `haven::is.labelled` = function(x) {
      inherits(x, "haven_labelled")
    },
    `haven::as_factor` = function(x) {
      if (inherits(x, "haven_labelled")) {
        labels <- attr(x, "labels")
        factor(names(labels)[match(x, labels)], levels = names(labels))
      } else {
        x
      }
    },
    {
      result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file)
      
      expect_true("IWRS" %in% names(result))
      expect_true("TEST_DATA" %in% names(result))
      expect_equal(nrow(result$IWRS), 1)
      expect_equal(result$IWRS$SUBJID, "001")
    }
  )
  
  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})

test_that("read_raw_data_with_formats handles reading errors", {
  temp_dir <- tempdir()
  
  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)
  
  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")
  
  # Mock to simulate reading errors
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("error_file.sas7bdat")
      }
      return(character(0))
    },
    `haven::read_sas` = function(...) {
      stop("Simulated catalog read error")
    },
    {
      expect_warning(
        result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file),
        "Failed to read file"
      )
      
      # Should still return IWRS data
      expect_true("IWRS" %in% names(result))
    }
  )
  
  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})

test_that("read_raw_data_with_formats warns about empty datasets", {
  temp_dir <- tempdir()
  
  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)
  
  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = character(0))  # Empty data
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")
  
  # Mock to return empty dataset
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("empty_data.sas7bdat")
      }
      return(character(0))
    },
    `haven::read_sas` = function(...) {
      data.frame(SUBJID = character(0))  # Return empty data frame
    },
    `haven::is.labelled` = function(x) FALSE,
    {
      expect_warning(
        result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file),
        "The following datasets are empty"
      )
    }
  )
  
  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})
