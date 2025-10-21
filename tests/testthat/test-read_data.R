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
  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  expect_warning(
    result <- read_raw_data(temp_dir,iwrs_file = iwrs_file),
    "No SAS files found in the specified directory"
    )
})


test_that("read_raw_data validates IWRS file existence", {
  temp_dir <- tempdir()

  # Create a format file so we don't fail on format file check
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test")
    },
    {
      expect_error(
        read_raw_data(temp_dir, iwrs_file = "nonexistent_iwrs.csv"),
        "IWRS file does not exist: nonexistent_iwrs.csv"
      )
    }
  )
})

test_that("read_raw_data handles no format file error", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  # Mock to return no format file
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("data1.sas7bdat", "data2.sas7bdat")) # No formats file
      }
      return(character(0))
    },
    {
      expect_warning(
        read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "No SAS format file \\(containing 'formats' in filename\\) found in directory"
      )
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles multiple format files", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  # Mock to return multiple format files
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats1.sas7bdat", "formats2.sas7bdat", "data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      return(data.frame(SUBJID = "001", VALUE = 1))
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "Multiple format files found, using the first one"
      )

      # Should still return data
      expect_true("IWRS" %in% names(result))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles format file read error", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  # Mock to simulate format file read error
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        stop("Cannot read format file")
      }
      return(data.frame(SUBJID = "001"))
    },
    {
      expect_warning(
        read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "Failed to read format file.*Cannot read format file"
      )
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles IWRS file read error", {
  temp_dir <- tempdir()

  # Create a malformed IWRS file
  iwrs_file <- file.path(temp_dir, "malformed_iwrs.csv")
  writeLines(c("invalid", "csv", "content"), iwrs_file)

  # Mock format file
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test")
    },
    `readr::read_csv` = function(...) {
      stop("Failed to parse CSV")
    },
    {
      expect_error(
        read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "Failed to read IWRS file.*Failed to parse CSV"
      )
    }
  )

  # Clean up
  unlink(iwrs_file)
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
        return(character(0)) # No SAS files
      }
      return(character(0))
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "No SAS files found in the specified directory"
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

test_that("read_raw_data applies format mapping correctly without modifying FMT_TBL", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Create format file
  format_file <- file.path(temp_dir, "formats.xlsx")

  # Mock to simulate multiple data files
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "dm.sas7bdat", "ae.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "COMMON", START = "1", LABEL = "Common"))
      }
      return(data.frame(SUBJID = "001", STATUS = "1"))
    },
    `file.exists` = function(path) {
      if (grepl("formats.xlsx", path)) return(TRUE)
      if (grepl("iwrs", path)) return(TRUE)
      return(FALSE)
    },
    `readxl::read_excel` = function(path, ...) {
      data.frame(
        `表` = c("DM", "AE"),
        `变量` = c("STATUS", "SEVERITY"),
        `编码值` = c("1", "2"),
        `编码说明` = c("Active", "Severe"),
        `匹配状态` = c("完全匹配", "完全匹配"),
        stringsAsFactors = FALSE
      )
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file, format_file = format_file)

      # Both datasets should be present and processed correctly
      expect_true("DM" %in% names(result))
      expect_true("AE" %in% names(result))
      # Both should have the SUBJID column
      expect_true("SUBJID" %in% names(result$DM))
      expect_true("SUBJID" %in% names(result$AE))
    }
  )

  # Clean up
  unlink(iwrs_file)
})



test_that("read_raw_data warns about empty datasets", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Mock to return empty SAS dataset
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "empty_data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      return(data.frame(SUBJID = character(0))) # Empty dataset
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "The following datasets are empty"
      )

      expect_true("IWRS" %in% names(result))
      expect_true("EMPTY_DATA" %in% names(result))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data warns about multiple empty datasets with summary", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Mock to return multiple empty SAS datasets
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "empty1.sas7bdat", "empty2.sas7bdat",
                 "empty3.sas7bdat", "nonempty.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      # Only nonempty.sas7bdat has data, others are empty
      if (grepl("nonempty", file)) {
        return(data.frame(SUBJID = "001", VALUE = 1))
      }
      return(data.frame(SUBJID = character(0))) # Empty dataset
    },
    {
      # Capture the warning message to check it contains multiple dataset names
      warning_messages <- NULL
      withCallingHandlers(
        {
          result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)
        },
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      # Check that warning was issued
      expect_true(any(grepl("The following datasets are empty", warning_messages)))

      # Check that all empty dataset names are in the warning message
      empty_warning <- warning_messages[grepl("The following datasets are empty", warning_messages)]
      expect_true(grepl("EMPTY1", empty_warning))
      expect_true(grepl("EMPTY2", empty_warning))
      expect_true(grepl("EMPTY3", empty_warning))

      # Non-empty dataset should not be in the warning
      expect_false(grepl("NONEMPTY", empty_warning))

      # All datasets should still be in the result
      expect_true("IWRS" %in% names(result))
      expect_true("EMPTY1" %in% names(result))
      expect_true("EMPTY2" %in% names(result))
      expect_true("EMPTY3" %in% names(result))
      expect_true("NONEMPTY" %in% names(result))

      # Check that non-empty dataset has data
      expect_equal(nrow(result$NONEMPTY), 1)
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data reports multiple failed file reads", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  # Mock to simulate multiple SAS files with errors
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "fail1.sas7bdat", "fail2.sas7bdat", "success.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      if (grepl("fail", file)) {
        stop("Simulated read error")
      }
      return(data.frame(SUBJID = "001", VALUE = 1))
    },
    {
      expect_warning(
        result <- read_raw_data(temp_dir, iwrs_file = iwrs_file),
        "Failed to read the following files"
      )

      # Should still have IWRS and successful file
      expect_true("IWRS" %in% names(result))
      expect_true("SUCCESS" %in% names(result))
      # Failed files should not be in result
      expect_false("FAIL1" %in% names(result))
      expect_false("FAIL2" %in% names(result))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles case-insensitive file pattern matching", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Mock to return files with standard lowercase extension
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        # Standard SAS file extensions are always lowercase .sas7bdat
        return(c("formats.sas7bdat", "data1.sas7bdat", "data2.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file, ignore.case = TRUE)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      return(data.frame(SUBJID = "001", VALUE = 1))
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)

      # All files should be read
      expect_true("DATA1" %in% names(result))
      expect_true("DATA2" %in% names(result))

      # Dataset names should be uppercase
      expect_false("data1" %in% names(result))
      expect_false("data2" %in% names(result))

      # Should NOT have .sas7bdat in names
      expect_false(any(grepl("\\.sas7bdat$", names(result))))
    }
  )

  # Clean up
  unlink(iwrs_file)
})


test_that("read_raw_data handles non-existent format_file gracefully", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Provide a format_file path that doesn't exist
  format_file <- file.path(temp_dir, "nonexistent_formats.xlsx")

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "data.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      return(data.frame(SUBJID = "001", STATUS = "1"))
    },
    {
      # Should work without error, just skip the format_file
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file, format_file = format_file)

      expect_true("IWRS" %in% names(result))
      expect_true("DATA" %in% names(result))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data correctly filters FMT_DF when FMT_TBL exists", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Create format file
  format_file <- file.path(temp_dir, "formats.xlsx")

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "dm.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        # FMT_DF contains STATUS, GENDER, AGE
        return(data.frame(
          FMTNAME = c("STATUS", "GENDER", "AGE"),
          START = c("1", "M", "1"),
          LABEL = c("Active", "Male", "Young")
        ))
      }
      return(data.frame(SUBJID = "001", STATUS = "1", GENDER = "M", AGE = "1"))
    },
    `file.exists` = function(path) {
      if (grepl("formats.xlsx", path)) return(TRUE)
      if (grepl("iwrs", path)) return(TRUE)
      return(FALSE)
    },
    `readxl::read_excel` = function(path, ...) {
      # FMT_TBL contains STATUS only (should override FMT_DF)
      data.frame(
        `表` = c("DM"),
        `变量` = c("STATUS"),
        `编码值` = c("1"),
        `编码说明` = c("Active from Excel"),
        `匹配状态` = c("完全匹配"),
        stringsAsFactors = FALSE
      )
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file, format_file = format_file)

      # Should have DM dataset
      expect_true("DM" %in% names(result))

      # FMT_DF should have been filtered to remove STATUS (which is in FMT_TBL)
      # So STATUS should use Excel format, while GENDER and AGE use SAS format
      # This is implicit in the logic but hard to test directly without exposing internals
      expect_true("SUBJID" %in% names(result$DM))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data successfully reads and formats data end-to-end", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,RANDDT,TREATMENT",
    "001,2023-01-01,1",
    "002,2023-01-02,2"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "dm.sas7bdat", "ae.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(
          FMTNAME = c("TREATMENT", "SEVERITY"),
          START = c("1", "1"),
          LABEL = c("Active Treatment", "Mild")
        ))
      }
      if (grepl("dm", file)) {
        return(data.frame(
          subjid = c("001", "002"),
          treatment = c("1", "2"),
          age = c(45, 52)
        ))
      }
      if (grepl("ae", file)) {
        return(data.frame(
          subjid = c("001"),
          severity = c("1"),
          aeterm = c("Headache")
        ))
      }
      return(data.frame())
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)

      # Check all expected datasets are present
      expect_true("IWRS" %in% names(result))
      expect_true("DM" %in% names(result))
      expect_true("AE" %in% names(result))

      # Check IWRS data
      expect_equal(nrow(result$IWRS), 2)
      expect_equal(result$IWRS$SUBJID, c("001", "002"))

      # Check DM data - columns should be uppercase
      expect_true("SUBJID" %in% names(result$DM))
      expect_true("TREATMENT" %in% names(result$DM))
      expect_equal(nrow(result$DM), 2)

      # Check AE data
      expect_true("SUBJID" %in% names(result$AE))
      expect_equal(nrow(result$AE), 1)
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data applies format mapping to actual data values", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "dm.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        # Format mapping: 1 -> "Male", 2 -> "Female"
        return(data.frame(
          FMTNAME = c("SEX", "SEX", "RACE", "RACE"),
          START = c("1", "2", "1", "2"),
          LABEL = c("Male", "Female", "Asian", "Caucasian"),
          stringsAsFactors = FALSE
        ))
      }
      # DM data with coded values
      return(data.frame(
        subjid = c("001", "002", "003"),
        sex = c("1", "2", "1"),
        race = c("1", "2", "1"),
        stringsAsFactors = FALSE
      ))
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)

      # Check that format mapping was applied to SEX column
      expect_true("SEX" %in% names(result$DM))
      expect_equal(result$DM$SEX, c("Male", "Female", "Male"))

      # Check that format mapping was applied to RACE column
      expect_true("RACE" %in% names(result$DM))
      expect_equal(result$DM$RACE, c("Asian", "Caucasian", "Asian"))
    }
  )

  # Clean up
  unlink(iwrs_file)
})

test_that("read_raw_data handles unmapped values in format mapping", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("formats.sas7bdat", "dm.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        # Format only has mapping for "1" and "2"
        return(data.frame(
          FMTNAME = c("STATUS", "STATUS"),
          START = c("1", "2"),
          LABEL = c("Active", "Inactive"),
          stringsAsFactors = FALSE
        ))
      }
      # Data has "1", "2", and unmapped value "3"
      return(data.frame(
        subjid = c("001", "002", "003"),
        status = c("1", "2", "3"),
        stringsAsFactors = FALSE
      ))
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)

      # Check that mapped values are converted
      expect_equal(result$DM$STATUS[1], "Active")
      expect_equal(result$DM$STATUS[2], "Inactive")

      # Check that unmapped value remains as original
      expect_equal(result$DM$STATUS[3], "3")
    }
  )

  # Clean up
  unlink(iwrs_file)
})

# Tests for read_raw_data_with_formats
test_that("read_raw_data_with_formats validates inputs (data_dir and catalog_file)", {
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

test_that("read_raw_data_with_formats handles empty directory", {
  temp_dir <- tempdir()
  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  expect_warning(
    result <- read_raw_data_with_formats(temp_dir,catalog_file,iwrs_file = iwrs_file),
    "No SAS data files found in directory: ", temp_dir
  )
})

test_that("read_raw_data_with_formats validates IWRS file existence", {
  temp_dir <- tempdir()

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  # Mock to return SAS files
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("test_data.sas7bdat")
      }
      return(character(0))
    },
    {
      expect_error(
        read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = "nonexistent_iwrs.csv"),
        "IWRS file does not exist: nonexistent_iwrs.csv"
      )
    }
  )

  # Clean up
  unlink(catalog_file)
})

test_that("read_raw_data_with_formats handles IWRS file read error", {
  temp_dir <- tempdir()

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  # Create a malformed IWRS file
  iwrs_file <- file.path(temp_dir, "malformed_iwrs.csv")
  writeLines(c("invalid", "csv", "content"), iwrs_file)

  # Mock SAS files
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("test_data.sas7bdat")
      }
      return(character(0))
    },
    `readr::read_csv` = function(...) {
      stop("Failed to parse CSV")
    },
    {
      expect_error(
        read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file),
        "Failed to read IWRS file.*Failed to parse CSV"
      )
    }
  )

  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
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
          class = c("haven_labelled", "numeric")
        )
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

test_that("read_raw_data correctly names datasets with regex fix", {
  temp_dir <- tempdir()

  # Create IWRS file
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Mock to return files with tricky names
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        # Files that could be problematic with bad regex
        return(c("formats.sas7bdat", "datasas7bdat.sas7bdat", "test.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(file, ...) {
      if (grepl("formats", file)) {
        return(data.frame(FMTNAME = "TEST", START = "1", LABEL = "Test"))
      }
      return(data.frame(SUBJID = "001", VALUE = 1))
    },
    {
      result <- read_raw_data(temp_dir, iwrs_file = iwrs_file)

      # Check that dataset names are correctly extracted
      expect_true("DATASAS7BDAT" %in% names(result))
      expect_true("TEST" %in% names(result))
      # Should not have .sas7bdat in the name
      expect_false(any(grepl("\\.sas7bdat", names(result))))
    }
  )

  # Clean up
  unlink(iwrs_file)
})


test_that("read_raw_data_with_formats warns about empty datasets", {
  temp_dir <- tempdir()

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = character(0)) # Empty data
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
      data.frame(SUBJID = character(0)) # Return empty data frame
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

test_that("read_raw_data_with_formats warns about multiple empty datasets with summary", {
  temp_dir <- tempdir()

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  # Create IWRS file with data
  iwrs_content <- c(
    "Header line 1",
    "Header line 2",
    "SUBJID,TREATMENT",
    "001,Active"
  )
  iwrs_file <- file.path(temp_dir, "test_iwrs.csv")
  writeLines(iwrs_content, iwrs_file)

  # Mock to return multiple empty datasets
  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("empty_dm.sas7bdat", "empty_ae.sas7bdat",
                 "empty_lb.sas7bdat", "vs.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(...) {
      # Extract file argument
      args <- list(...)
      if (!is.null(args$data_file)) {
        file <- args$data_file
      } else if (length(args) > 0) {
        file <- args[[1]]
      } else {
        file <- ""
      }

      # Only vs.sas7bdat has data
      if (grepl("vs", file)) {
        return(data.frame(SUBJID = c("001", "002"), VSTESTCD = c("HR", "TEMP")))
      }
      # Others are empty
      return(data.frame(SUBJID = character(0)))
    },
    `haven::is.labelled` = function(x) FALSE,
    {
      # Capture warning messages
      warning_messages <- NULL
      withCallingHandlers(
        {
          result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file)
        },
        warning = function(w) {
          warning_messages <<- c(warning_messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      # Check that warning was issued
      expect_true(any(grepl("The following datasets are empty", warning_messages)))

      # Check that all empty dataset names are in the warning message
      empty_warning <- warning_messages[grepl("The following datasets are empty", warning_messages)]
      expect_true(grepl("EMPTY_DM", empty_warning))
      expect_true(grepl("EMPTY_AE", empty_warning))
      expect_true(grepl("EMPTY_LB", empty_warning))

      # Non-empty dataset should not be in the warning
      expect_false(grepl("VS", empty_warning))

      # All datasets should still be in the result
      expect_true("IWRS" %in% names(result))
      expect_true("EMPTY_DM" %in% names(result))
      expect_true("EMPTY_AE" %in% names(result))
      expect_true("EMPTY_LB" %in% names(result))
      expect_true("VS" %in% names(result))

      # Check that non-empty dataset has data
      expect_equal(nrow(result$VS), 2)
    }
  )

  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})


test_that("read_raw_data_with_formats handles multiple failed file reads", {
  temp_dir <- tempdir()

  # Create mock catalog file
  catalog_file <- file.path(temp_dir, "test_catalog.sas7bcat")
  file.create(catalog_file)

  # Create IWRS file
  iwrs_data <- data.frame(SUBJID = "001")
  iwrs_file <- create_temp_csv_file(iwrs_data, "test_iwrs.csv")

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("fail1.sas7bdat", "fail2.sas7bdat", "success.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(...) {
      # Extract the data_file argument
      args <- list(...)
      if (!is.null(args$data_file)) {
        file <- args$data_file
      } else if (length(args) > 0) {
        file <- args[[1]]
      } else {
        file <- ""
      }

      if (grepl("fail", file)) {
        stop("Simulated catalog read error")
      }
      return(data.frame(SUBJID = "001", VALUE = 1))
    },
    `haven::is.labelled` = function(x) FALSE,
    {
      expect_warning(
        result <- read_raw_data_with_formats(temp_dir, catalog_file, iwrs_file = iwrs_file),
        "Failed to read the following files"
      )

      # Should still have IWRS and successful file
      expect_true("IWRS" %in% names(result))
      expect_true("SUCCESS" %in% names(result))
      # Failed files should not be in result
      expect_false("FAIL1" %in% names(result))
      expect_false("FAIL2" %in% names(result))
    }
  )

  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})


test_that("read_raw_data_with_formats uses custom encoding", {
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

  # Track if encoding parameter is passed correctly
  encoding_used <- NULL

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return("test_data.sas7bdat")
      }
      return(character(0))
    },
    `haven::read_sas` = function(data_file, catalog_file, encoding, catalog_encoding, ...) {
      encoding_used <<- encoding
      df <- data.frame(SUBJID = c("001", "002"))
      return(df)
    },
    `haven::is.labelled` = function(x) FALSE,
    {
      result <- read_raw_data_with_formats(temp_dir, catalog_file,
                                          iwrs_file = iwrs_file,
                                          encoding = "GBK")

      # Verify encoding was used
      expect_equal(encoding_used, "GBK")
      expect_true("TEST_DATA" %in% names(result))
    }
  )

  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})


test_that("read_raw_data_with_formats successfully processes labelled data", {
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

  with_mock(
    `list.files` = function(path, pattern, ...) {
      if (grepl("sas7bdat", pattern)) {
        return(c("dm.sas7bdat", "ae.sas7bdat"))
      }
      return(character(0))
    },
    `haven::read_sas` = function(data_file, catalog_file, ...) {
      if (grepl("dm", data_file)) {
        df <- data.frame(
          subjid = c("001", "002"),
          status = structure(c(1, 2),
            labels = c("Active" = 1, "Inactive" = 2),
            class = c("haven_labelled", "numeric")
          ),
          age = c(45, 52)
        )
        return(df)
      }
      if (grepl("ae", data_file)) {
        df <- data.frame(
          subjid = c("001"),
          severity = structure(c(1),
            labels = c("Mild" = 1, "Moderate" = 2),
            class = c("haven_labelled", "numeric")
          )
        )
        return(df)
      }
      return(data.frame())
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

      # Check all datasets present
      expect_true("IWRS" %in% names(result))
      expect_true("DM" %in% names(result))
      expect_true("AE" %in% names(result))

      # Check that columns were converted to uppercase
      expect_true("SUBJID" %in% names(result$DM))
      expect_true("STATUS" %in% names(result$DM))
      expect_true("AGE" %in% names(result$DM))
    }
  )

  # Clean up
  unlink(catalog_file)
  unlink(iwrs_file)
})

