# Test generate_report.R functions

# =============================================================================
# Helper function to create test data
# =============================================================================

create_test_checks_df <- function() {
  data.frame(
    check_name = c("age_check", "age_check", "ldl_check", "ldl_check"),
    has_deviation = c(TRUE, TRUE, FALSE, FALSE),
    message = c("年龄不符合要求", "年龄不符合要求", "LDL正常", "LDL正常"),
    details = c("详情1", "详情2", NA, NA),
    PDNO = c("1.1", "1.1", "2.1", "2.1"),
    SUBJID = c("001", "002", "003", "004"),
    SITEID = c("01", "01", "02", "02"),
    TBNAME = c("DM", "DM", "LB", "LB"),
    DESCRIPTION = c("年龄不符合要求", "年龄不符合要求", "LDL正常", "LDL正常"),
    stringsAsFactors = FALSE
  )
}

create_empty_checks_df <- function() {
  data.frame(
    check_name = character(),
    has_deviation = logical(),
    message = character(),
    details = character(),
    PDNO = character(),
    SUBJID = character(),
    SITEID = character(),
    TBNAME = character(),
    DESCRIPTION = character(),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Test generate_markdown_report() - Parameter Validation
# =============================================================================

test_that("generate_markdown_report() validates checks_df parameter", {
  skip_if_not_installed("knitr")

  # 非数据框应该报错

  expect_error(
    generate_markdown_report(checks_df = "not a dataframe"),
    "'checks_df' must be a data frame"
  )

  expect_error(
    generate_markdown_report(checks_df = list(a = 1)),
    "'checks_df' must be a data frame"
  )

  expect_error(
    generate_markdown_report(checks_df = NULL),
    "'checks_df' must be a data frame"
  )
})

test_that("generate_markdown_report() validates output_file parameter", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  expect_error(
    generate_markdown_report(checks_df = test_df, output_file = 123),
    "'output_file' must be a character string or NULL"
  )

  expect_error(
    generate_markdown_report(checks_df = test_df, output_file = c("a", "b")),
    "'output_file' must be a character string or NULL"
  )
})

test_that("generate_markdown_report() validates include_no_deviation parameter", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  expect_error(
    generate_markdown_report(checks_df = test_df, include_no_deviation = "yes"),
    "'include_no_deviation' must be a single logical value"
  )

  expect_error(
    generate_markdown_report(checks_df = test_df, include_no_deviation = c(TRUE, FALSE)),
    "'include_no_deviation' must be a single logical value"
  )
})

test_that("generate_markdown_report() validates title parameter", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  expect_error(
    generate_markdown_report(checks_df = test_df, title = 123),
    "'title' must be a single character string"
  )

  expect_error(
    generate_markdown_report(checks_df = test_df, title = c("a", "b")),
    "'title' must be a single character string"
  )
})

# =============================================================================
# Test generate_markdown_report() - Functionality
# =============================================================================

test_that("generate_markdown_report() generates correct report content", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  result <- generate_markdown_report(
    checks_df = test_df,
    title = "Test Report"
  )

  # 验证返回值是字符串
  expect_type(result, "character")

  # 验证包含标题
  expect_match(result, "# Test Report")

  # 验证包含生成时间
  expect_match(result, "Report generated on:")

  # 验证包含Summary部分
  expect_match(result, "## Summary")

  # 验证包含有偏差的检查
  expect_match(result, "age_check")
})

test_that("generate_markdown_report() handles empty dataframe with warning", {
  skip_if_not_installed("knitr")

  empty_df <- create_empty_checks_df()

  expect_warning(
    generate_markdown_report(checks_df = empty_df),
    "'checks_df' is empty"
  )
})

test_that("generate_markdown_report() respects include_no_deviation parameter", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  # 不包含无偏差的检查
  result_no <- generate_markdown_report(
    checks_df = test_df,
    include_no_deviation = FALSE
  )
  # ldl_check 没有偏差，不应该出现在详细结果中
  expect_false(grepl("### ldl_check", result_no))

  # 包含无偏差的检查
  result_yes <- generate_markdown_report(
    checks_df = test_df,
    include_no_deviation = TRUE
  )
  # ldl_check 应该出现
  expect_true(grepl("### ldl_check", result_yes))
})

test_that("generate_markdown_report() writes to file when output_file is provided", {
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()
  temp_file <- tempfile(fileext = ".md")
  on.exit(unlink(temp_file), add = TRUE)

  expect_message(
    generate_markdown_report(checks_df = test_df, output_file = temp_file),
    "Report written to:"
  )

  expect_true(file.exists(temp_file))

  content <- readLines(temp_file)
  expect_true(length(content) > 0)
})

# =============================================================================
# Test generate_html_report() - Parameter Validation
# =============================================================================

test_that("generate_html_report() validates parameters", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  test_df <- create_test_checks_df()

  # checks_df 验证
  expect_error(
    generate_html_report(checks_df = "not a df", output_file = "test.html"),
    "'checks_df' must be a data frame"
  )

  # output_file 验证
  expect_error(
    generate_html_report(checks_df = test_df, output_file = 123),
    "'output_file' must be a single character string"
  )

  # include_no_deviation 验证
  expect_error(
    generate_html_report(
      checks_df = test_df,
      output_file = "test.html",
      include_no_deviation = "yes"
    ),
    "'include_no_deviation' must be a single logical value"
  )

  # title 验证
  expect_error(
    generate_html_report(
      checks_df = test_df,
      output_file = "test.html",
      title = 123
    ),
    "'title' must be a single character string"
  )

  # css_file 验证
  expect_error(
    generate_html_report(
      checks_df = test_df,
      output_file = "test.html",
      css_file = 123
    ),
    "'css_file' must be a single character string or NULL"
  )
})

# =============================================================================
# Test generate_excel_report() - Parameter Validation
# =============================================================================

test_that("generate_excel_report() validates parameters", {
  skip_if_not_installed("openxlsx")

  test_df <- create_test_checks_df()

  # checks_df 验证
  expect_error(
    generate_excel_report(checks_df = "not a df", output_file = "test.xlsx"),
    "'checks_df' must be a data frame"
  )

  # output_file 验证
  expect_error(
    generate_excel_report(checks_df = test_df, output_file = 123),
    "'output_file' must be a single character string"
  )

  # title 验证
  expect_error(
    generate_excel_report(
      checks_df = test_df,
      output_file = "test.xlsx",
      title = 123
    ),
    "'title' must be a single character string"
  )
})

# =============================================================================
# Test generate_excel_report() - Functionality
# =============================================================================

test_that("generate_excel_report() creates Excel file", {
  skip_if_not_installed("openxlsx")

  test_df <- create_test_checks_df()
  temp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_file), add = TRUE)

  result <- generate_excel_report(
    checks_df = test_df,
    output_file = temp_file,
    title = "Test Excel Report"
  )

  # 验证文件创建
  expect_true(file.exists(temp_file))

  # 验证返回值
  expect_equal(result, temp_file)

  # 验证文件可以被读取
  wb <- openxlsx::loadWorkbook(temp_file)
  sheet_names <- names(wb)
  expect_true("Summary" %in% sheet_names)
  expect_true("All Deviations" %in% sheet_names)

  # 验证 All Deviations 工作表只包含指定列
  data <- openxlsx::read.xlsx(temp_file, sheet = "All Deviations")
  expect_equal(names(data), c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION"))
})

test_that("generate_excel_report() Summary lists all checks; All Deviations only deviation rows", {
  skip_if_not_installed("openxlsx")

  test_df <- create_test_checks_df()
  temp_file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(temp_file), add = TRUE)

  generate_excel_report(
    checks_df = test_df,
    output_file = temp_file
  )

  # All Deviations：仅 has_deviation == TRUE 的行
  data_dev <- openxlsx::read.xlsx(temp_file, sheet = "All Deviations")
  expect_equal(nrow(data_dev), 2)
  expect_equal(sort(data_dev$SUBJID), c("001", "002"))

  # Summary：含无偏离的检查（ldl_check），deviation_count 为 0
  sum_tbl <- openxlsx::read.xlsx(temp_file, sheet = "Summary", startRow = 3)
  expect_setequal(sum_tbl$check_name, c("age_check", "ldl_check"))
  expect_equal(sum_tbl$deviation_count[sum_tbl$check_name == "ldl_check"], 0)
  expect_equal(sum_tbl$deviation_count[sum_tbl$check_name == "age_check"], 2)
})

# =============================================================================
# Test package availability checks
# =============================================================================

test_that("generate_markdown_report() checks for knitr availability", {
  # 此测试仅验证错误消息格式
  # 实际测试需要在没有 knitr 的环境中运行

  skip("Skip package availability test - requires clean environment")

  expect_error(
    generate_markdown_report(create_test_checks_df()),
    "Package 'knitr' is required"
  )
})

test_that("generate_html_report() checks for rmarkdown availability", {
  skip("Skip package availability test - requires clean environment")

  expect_error(
    generate_html_report(create_test_checks_df(), "test.html"),
    "Package 'rmarkdown' is required"
  )
})

test_that("generate_excel_report() checks for openxlsx availability", {
  skip("Skip package availability test - requires clean environment")

  expect_error(
    generate_excel_report(create_test_checks_df(), "test.xlsx"),
    "Package 'openxlsx' is required"
  )
})
