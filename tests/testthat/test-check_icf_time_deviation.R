test_that("check_icf_time_deviation works with valid deviations", {
  # Create test data with deviations
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-10", "2024-01-15", "2024-01-20"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002", "003"),
      VSDAT = c("2024-01-05", "2024-01-20", "2024-01-25"), # 001 has deviation
      stringsAsFactors = FALSE
    ),
    AE = data.frame(
      SUBJID = c("001", "002"),
      AEDAT = c("2024-01-08", "2024-01-16"), # 001 has deviation
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  expect_true(result$has_deviation)
  expect_equal(length(result$messages), 1)
  expect_true(nrow(result$details) >= 1)
  expect_true(all(c("SUBJID", "action", "event_datetime", "icf_datetime", "diff_date") %in% names(result$details)))
  expect_true(all(result$details$diff_date < 0)) # All differences should be negative
  expect_true("001" %in% result$details$SUBJID) # Subject 001 should have deviation
})

test_that("check_icf_time_deviation returns no deviations when all dates are valid", {
  # Create test data without deviations
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-10", "2024-01-15", "2024-01-20"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002", "003"),
      VSDAT = c("2024-01-15", "2024-01-20", "2024-01-25"), # All after IC
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  expect_false(result$has_deviation)
  expect_equal(length(result$messages), 0)
  expect_equal(nrow(result$details), 0)
})

test_that("check_icf_time_deviation handles custom IC dataset and variable", {
  # Create test data with custom names
  test_data <- list(
    CONSENT = data.frame(
      SUBJID = c("001", "002"),
      CONSENTDT = c("2024-01-10", "2024-01-15"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002"),
      VSDAT = c("2024-01-05", "2024-01-20"), # 001 has deviation
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data,
    ic_dataset = "CONSENT",
    ic_date_var = "CONSENTDT"
  )

  expect_s3_class(result, "icf_time_deviation")
  expect_true(result$has_deviation)
  expect_true("001" %in% result$details$SUBJID)
})

test_that("check_icf_time_deviation respects ignore_vars parameter", {
  # Create test data
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002"),
      ICDAT = c("2024-01-10", "2024-01-15"),
      stringsAsFactors = FALSE
    ),
    DM = data.frame(
      SUBJID = c("001", "002"),
      BRTHDAT = c("1990-01-01", "1985-01-01"), # Should be ignored by default
      SCREENDAT = c("2024-01-05", "2024-01-12"), # Should be checked
      stringsAsFactors = FALSE
    )
  )

  # Test with default ignore_vars (BRTHDAT)
  result1 <- check_icf_time_deviation(test_data)
  expect_true(result1$has_deviation)
  expect_false(any(grepl("BRTHDAT", result1$details$action)))

  # Test with custom ignore_vars
  result2 <- check_icf_time_deviation(test_data,
    ignore_vars = c("BRTHDAT", "SCREENDAT")
  )
  expect_false(result2$has_deviation)
})

test_that("check_icf_time_deviation respects exclude_datasets parameter", {
  # Create test data
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002"),
      ICDAT = c("2024-01-10", "2024-01-15"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002"),
      VSDAT = c("2024-01-05", "2024-01-20"), # 001 has deviation
      stringsAsFactors = FALSE
    ),
    AE = data.frame(
      SUBJID = c("001", "002"),
      AEDAT = c("2024-01-08", "2024-01-12"), # Both have deviation
      stringsAsFactors = FALSE
    )
  )

  # Exclude VS dataset
  result <- check_icf_time_deviation(test_data, exclude_datasets = "VS")

  expect_s3_class(result, "icf_time_deviation")
  expect_false(any(grepl("^VS\\.", result$details$action)))
  expect_true(any(grepl("^AE\\.", result$details$action)))
})

test_that("check_icf_time_deviation handles missing IC dataset", {
  test_data <- list(
    VS = data.frame(
      SUBJID = c("001", "002"),
      VSDAT = c("2024-01-05", "2024-01-20"),
      stringsAsFactors = FALSE
    )
  )

  expect_error(check_icf_time_deviation(test_data), "Missing IC dataset")
})

test_that("check_icf_time_deviation handles invalid date formats gracefully", {
  # Create test data with some invalid dates
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-10", "2024-01-15", "2024-01-20"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002", "003"),
      VSDAT = c("2024-01-05", "UNKNOWN", "2024-01-25"), # Invalid date for 002
      stringsAsFactors = FALSE
    )
  )

  # Should not error, invalid dates should be converted to NA and filtered out
  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  expect_true(result$has_deviation)
  # Subject 002 should not appear in results due to invalid date
  expect_false("002" %in% result$details$SUBJID)
})

test_that("check_icf_time_deviation handles NA dates", {
  # Create test data with NA dates
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-10", "2024-01-15", NA),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "002", "003"),
      VSDAT = c("2024-01-05", NA, "2024-01-25"),
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  # Only valid date comparisons should be included
  expect_true(all(!is.na(result$details$event_datetime)))
  expect_true(all(!is.na(result$details$icf_datetime)))
})

test_that("check_icf_time_deviation removes duplicates correctly", {
  # Create test data with multiple events on same day
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001", "002"),
      ICDAT = c("2024-01-10", "2024-01-11"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001", "001", "001"),
      VSDAT = c("2024-01-05", "2024-01-05", "2024-01-06"),
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  # Should keep only the earliest event per subject-action combination
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$event_datetime[1], as.Date("2024-01-05"))
})

test_that("check_icf_time_deviation handles empty datasets", {
  # Create test data with empty datasets
  test_data <- list(
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = character(0),
      VSDAT = character(0),
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("check_icf_time_deviation identifies variables ending with DAT", {
  # Create test data with various date column names
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-10"),
      stringsAsFactors = FALSE
    ),
    TEST = data.frame(
      SUBJID = c("001"),
      VSDAT = c("2024-01-05"), # Should be checked (ends with DAT)
      AEDAT = c("2024-01-06"), # Should be checked (ends with DAT)
      CMSTDT = c("2024-01-07"), # Should NOT be checked (doesn't end with DAT)
      VSDATE = c("2024-01-08"), # Should NOT be checked (ends with DATE not DAT)
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  expect_s3_class(result, "icf_time_deviation")
  expect_true(result$has_deviation)
  # Should only have VSDAT and AEDAT
  expect_equal(nrow(result$details), 2)
  expect_true(all(grepl("DAT$", result$details$action)))
})

test_that("print.icf_time_deviation works correctly", {
  test_data <- list(
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-10"),
      stringsAsFactors = FALSE
    ),
    VS = data.frame(
      SUBJID = c("001"),
      VSDAT = c("2024-01-05"),
      stringsAsFactors = FALSE
    )
  )

  result <- check_icf_time_deviation(test_data)

  # Test that print method doesn't error
  expect_output(print(result), "2.1.1 获得ICF前进行了试验相关操作")
  expect_output(print(result), "Has deviation: YES")
})
