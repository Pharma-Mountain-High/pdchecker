library(testthat)

create_temp_testwp_excel <- function(data_matrix, filename, dir = NULL) {
  if (is.null(dir)) dir <- withr::local_tempdir(.local_envir = parent.frame())
  temp_file <- file.path(dir, filename)
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(df) <- NULL
  writexl::write_xlsx(list(Sheet1 = df), temp_file)
  temp_file
}

test_that("parse_testwp_cell parses RD, SV, EX, FD rules", {
  rd <- pdchecker:::parse_testwp_cell("RD(-7d)")
  expect_equal(rd$ref, "RD")
  expect_equal(rd$wp, "-7d")
  expect_equal(rd$type, "-")
  expect_equal(rd$wpvalue, 7)

  sv <- pdchecker:::parse_testwp_cell("SV(±3d)")
  expect_equal(sv$ref, "SV")
  expect_equal(sv$type, "±")
  expect_equal(sv$wpvalue, 3)

  ex <- pdchecker:::parse_testwp_cell("EX(≤24h)")
  expect_equal(ex$ref, "EX")
  expect_equal(ex$type, "≤")
  expect_equal(ex$wpvalue, 24)
  expect_equal(ex$wp_unit, "h")

  ex_H <- pdchecker:::parse_testwp_cell("EX(≤24H)")
  expect_equal(ex_H$wpvalue, 24)
  expect_equal(ex_H$wp_unit, "h")

  fd <- pdchecker:::parse_testwp_cell("FD(±3d)")
  expect_equal(fd$ref, "FD")
  expect_equal(fd$type, "±")
  expect_equal(fd$wpvalue, 3)
  expect_equal(fd$wp_unit, "d")

  zero <- pdchecker:::parse_testwp_cell("EX(0)")
  expect_equal(zero$ref, "EX")
  expect_equal(zero$wp, "0")
  expect_equal(zero$type, "0")
  expect_equal(zero$wpvalue, 0)
  expect_equal(zero$wp_unit, "d")

  fd_zero <- pdchecker:::parse_testwp_cell("FD(0)")
  expect_equal(fd_zero$ref, "FD")
  expect_equal(fd_zero$type, "0")
  expect_equal(fd_zero$wpvalue, 0)
})

test_that("parse_testwp_cell returns NULL for empty cells", {
  expect_null(pdchecker:::parse_testwp_cell(NA))
  expect_null(pdchecker:::parse_testwp_cell(""))
  expect_null(pdchecker:::parse_testwp_cell("   "))
})

test_that("parse_testwp_cell errors on invalid format", {
  expect_error(
    pdchecker:::parse_testwp_cell("INVALID"),
    "Invalid test window rule"
  )
})

test_that("read_testwp_file expands matrix with non-empty cells only", {
  mat <- rbind(
    c("VISIT", "VISITNUM", "血常规", "血生化"),
    c("筛选期", "1", NA, NA),
    c("C1D1", "6", "EX(≤24h)", "EX(0)"),
    c("C2D1", "10", "SV(±3d)", NA)
  )

  temp_file <- create_temp_testwp_excel(mat, "test_wp.xlsx")

  result <- read_testwp_file(temp_file)

  expect_equal(nrow(result), 3)
  expect_true(all(c(
    "TESTCAT", "VISITNUM", "VISIT", "wp_rule", "ref", "wp", "type", "wpvalue", "wp_unit"
  ) %in% names(result)))

  ex_row <- result[result$TESTCAT == "血常规" & result$VISITNUM == "6", ]
  expect_equal(ex_row$ref, "EX")
  expect_equal(ex_row$type, "≤")
  expect_equal(ex_row$wpvalue, 24)
  expect_equal(ex_row$wp_unit, "h")

  zero_row <- result[result$TESTCAT == "血生化" & result$VISITNUM == "6", ]
  expect_equal(zero_row$ref, "EX")
  expect_equal(zero_row$type, "0")

  sv_row <- result[result$TESTCAT == "血常规" & result$VISITNUM == "10", ]
  expect_equal(sv_row$ref, "SV")
  expect_equal(sv_row$type, "±")

  unlink(temp_file)
})

test_that("read_testwp_file reads package example file", {
  example_path <- system.file("extdata", "example_test_wp.xlsx", package = "pdchecker")
  skip_if_not(file.exists(example_path), "example_test_wp.xlsx not installed")

  result <- read_testwp_file(example_path, sheet_name = "QL0911-302")

  expect_gt(nrow(result), 0)
  expect_true(all(result$ref %in% c("RD", "SV", "EX", "FD")))
  expect_true(any(result$ref %in% c("RD", "FD")))
  expect_true(any(result$ref == "SV"))
  expect_true(any(result$ref == "EX"))
  expect_true(any(result$type == "0"))
  expect_true(any(result$wp_unit == "h"))
  expect_true("血常规" %in% result$TESTCAT)
})

test_that("read_testwp_file validates header row", {
  mat <- rbind(
    c("WRONG", "VISITNUM", "血常规"),
    c("V1", "1", "RD(0)")
  )
  temp_file <- create_temp_testwp_excel(mat, "bad_header.xlsx")

  expect_error(
    read_testwp_file(temp_file),
    "first row must start with 'VISIT' and 'VISITNUM'"
  )

  unlink(temp_file)
})

test_that("parse_testwp_cell always returns numeric wpvalue", {
  zero <- pdchecker:::parse_testwp_cell("EX(0)")
  expect_type(zero$wpvalue, "double")

  std <- pdchecker:::parse_testwp_cell("SV(±3d)")
  expect_type(std$wpvalue, "double")

  # Unrecognized WP falls through to type "其他"; wpvalue must still be numeric NA
  other <- pdchecker:::parse_testwp_cell("SV(abc)")
  expect_equal(other$type, "其他")
  expect_type(other$wpvalue, "double")
  expect_true(is.na(other$wpvalue))
})

test_that("read_testwp_file binds rows when wpvalue types would differ", {
  mat <- rbind(
    c("VISIT", "VISITNUM", "血常规", "血生化"),
    c("筛选期", "1", "RD(0)", "RD(-7d)"),
    c("C1D1", "6", "EX(≤24h)", "SV(abc)"),
    c("C2D1", "10", "SV(±3d)", NA)
  )

  temp_file <- create_temp_testwp_excel(mat, "mixed_wpvalue.xlsx")

  result <- read_testwp_file(temp_file)

  expect_equal(nrow(result), 5)
  expect_type(result$wpvalue, "double")
  expect_true(is.na(result$wpvalue[result$wp == "abc"]))

  unlink(temp_file)
})

test_that("read_testwp_file joins visitcode when provided", {
  mat <- rbind(
    c("VISIT", "VISITNUM", "血常规"),
    c("C1D1", "6", "EX(0)")
  )
  temp_file <- create_temp_testwp_excel(mat, "join.xlsx")

  visitcode <- data.frame(
    VISIT = "C1D1_visitcode",
    VISITNUM = "6",
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- read_testwp_file(temp_file, visitcode = visitcode),
    "VISIT names that differ"
  )
  expect_equal(result$VISITNUM, "6")

  unlink(temp_file)
})
