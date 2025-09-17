library(testthat)
library(dplyr)

test_that("No screened subjects without informed consent", {
  data <- list(
    SCR = data.frame(SUBJID = c("001", "002")),
    IC = data.frame(SUBJID = c("001", "002")),
    RAND = data.frame()
  )
  result <- check_screen_without_ic(data)
  expect_false(result$has_deviation)
  expect_equal(length(result$messages), 0)
  expect_equal(nrow(result$details), 0)
})

test_that("Screened subjects without informed consent detected", {
  data <- list(
    SCR = data.frame(SUBJID = c("001", "002", "003")),
    IC = data.frame(SUBJID = c("001", "002")),
    RAND = data.frame()
  )
  result <- check_screen_without_ic(data)
  expect_true(result$has_deviation)
  expect_equal(result$messages, "未签署知情同意书")
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "003")
})

test_that("Missing required datasets throw errors", {
  data_missing_rand <- list(
    SCR = data.frame(SUBJID = c("001")),
    IC = data.frame(SUBJID = c("001"))
  )
  expect_error(check_screen_without_ic(data_missing_rand), "Missing required datasets: RAND")

  data_missing_ic <- list(
    SCR = data.frame(SUBJID = c("001")),
    RAND = data.frame()
  )
  expect_error(check_screen_without_ic(data_missing_ic), "Missing required datasets: IC")

  data_missing_scr <- list(
    IC = data.frame(SUBJID = c("001")),
    RAND = data.frame()
  )
  expect_error(check_screen_without_ic(data_missing_scr), "Missing required datasets: SCR")
})
