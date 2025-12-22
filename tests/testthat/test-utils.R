# Test utility functions in utils.R

# =============================================================================
# Test is_sas_na() function
# =============================================================================

test_that("is_sas_na() correctly identifies SAS missing values", {
  # 测试 NA 值
  expect_true(all(is_sas_na(c(NA, NA))))

  # 测试字符串 "NA"
  expect_true(is_sas_na("NA"))

  # 测试点号 "."
  expect_true(is_sas_na("."))

  # 测试空字符串
  expect_true(is_sas_na(""))

  # 测试带空格的情况
  expect_true(is_sas_na("  NA  "))
  expect_true(is_sas_na("  .  "))
  expect_true(is_sas_na("    "))

  # 测试非缺失值
  expect_false(is_sas_na("a"))
  expect_false(is_sas_na("1"))
  expect_false(is_sas_na("text"))

  # 测试混合向量
  result <- unname(is_sas_na(c("a", NA, "NA", "", ".", "b")))
  expect_equal(result, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
})


# =============================================================================
# Test match_visit_type() function (internal function)
# =============================================================================

test_that("match_visit_type() correctly classifies visit types", {
  # Note: match_visit_type is an internal function, access with :::
  match_visit_type <- pdchecker:::match_visit_type
  # 筛选期
  expect_equal(match_visit_type("筛选"), "screening")
  expect_equal(match_visit_type("筛选访视"), "screening")
  expect_equal(match_visit_type("SCREENING筛选"), "screening")

  # 治疗期
  expect_equal(match_visit_type("治疗周期1"), "treatment")
  expect_equal(match_visit_type("治疗访视"), "treatment")
  expect_equal(match_visit_type("治疗第1天"), "treatment")

  # 治疗结束
  expect_equal(match_visit_type("治疗结束访视"), "end_of_treatment")
  expect_equal(match_visit_type("退出访视"), "end_of_treatment")
  expect_equal(match_visit_type("治疗结束"), "end_of_treatment")

  # 随访
  expect_equal(match_visit_type("随访1"), "follow_up")
  expect_equal(match_visit_type("安全性随访"), "follow_up")

  # 未知类型
  expect_equal(match_visit_type("其他访视"), "unknown")
  expect_equal(match_visit_type(NA), "unknown")

  # 大小写不敏感
  expect_equal(match_visit_type("筛选"), "screening")
  expect_equal(match_visit_type("筛选"), "screening")
})


# =============================================================================
# 测试 as_check_df() 函数
# =============================================================================

test_that("as_check_df() converts check results to data frame", {
  # 创建模拟的检查结果
  check_result <- list(
    has_deviation = TRUE,
    messages = c("发现1个问题", "发现2个问题"),
    details = data.frame(
      SUBJID = c("001", "002"),
      issue = c("问题1", "问题2"),
      stringsAsFactors = FALSE
    )
  )
  class(check_result) <- c("test_check", "list")

  # 转换为数据框
  result_df <- as_check_df(check_result)

  # 验证结果
  expect_s3_class(result_df, "data.frame")
  expect_true("check_name" %in% names(result_df))
  expect_true("has_deviation" %in% names(result_df))
  expect_equal(nrow(result_df), 2) # 应该有2行详情
  expect_equal(result_df$check_name[1], "test_check")
  expect_true(all(result_df$has_deviation))
})

test_that("as_check_df() handles results without details", {
  check_result <- list(
    has_deviation = FALSE,
    messages = "无偏差",
    details = data.frame() # 空的 details
  )

  result_df <- as_check_df(check_result, check_name = "simple_check")

  expect_s3_class(result_df, "data.frame")
  expect_equal(nrow(result_df), 1)
  expect_equal(result_df$check_name[1], "simple_check")
  expect_false(result_df$has_deviation[1])
})


# =============================================================================
# 测试 combine_check_results() 函数
# =============================================================================

test_that("combine_check_results() combines multiple check results", {
  # 创建两个检查结果
  result1 <- list(
    has_deviation = TRUE,
    messages = "问题1",
    details = data.frame(SUBJID = "001", stringsAsFactors = FALSE)
  )
  class(result1) <- c("check1", "list")

  result2 <- list(
    has_deviation = FALSE,
    messages = "无问题",
    details = data.frame(SUBJID = "002", stringsAsFactors = FALSE)
  )
  class(result2) <- c("check2", "list")

  # 合并结果
  combined <- combine_check_results(result1, result2)

  # 验证
  expect_s3_class(combined, "data.frame")
  expect_equal(nrow(combined), 2)
  expect_true("check_name" %in% names(combined))
})

test_that("combine_check_results() handles NULL results", {
  result1 <- list(
    has_deviation = TRUE,
    messages = "问题",
    details = data.frame()
  )

  combined <- combine_check_results(result1, NULL)

  expect_s3_class(combined, "data.frame")
  expect_gte(nrow(combined), 1)
})


# =============================================================================
# 测试 parse_check_output() 函数 - subjid 提取
# =============================================================================

test_that("parse_check_output() correctly extracts subjid from various formats", {
  # 测试不同格式的输出
  header <- "8.4 访视超窗检查\n====================================\n"
  body <- "Has deviation: YES\n\nFindings:\n- 发现问题\n\nDeviation Details:\n"
  test_cases <- list(
    list(
      text = paste0(header, body, "受试者12345访视超窗"),
      expected_subjid = "12345"
    ),
    list(
      text = paste0(header, body, "受试者 00123 访视超窗"),
      expected_subjid = "00123"
    ),
    list(
      text = paste0(header, body, "受试者：001访视超窗"),
      expected_subjid = "001"
    ),
    list(
      text = paste0(header, body, "受试者 0000567 访视超窗"),
      expected_subjid = "0000567"
    )
  )

  for (test_case in test_cases) {
    result <- parse_check_output(text = test_case$text)
    expect_equal(result$SUBJID[1], test_case$expected_subjid,
      info = paste("Failed for text:", substr(test_case$text, 1, 50))
    )
  }
})

test_that("parse_check_output() extracts check name correctly", {
  text <- "8.4 访视超窗检查\n====================================\nHas deviation: NO\n\nFindings:\n- 无偏差"

  result <- parse_check_output(text = text)

  expect_equal(result$check_name[1], "8.4 访视超窗检查")
  expect_false(result$has_deviation[1])
})

test_that("parse_check_output() handles deviation status", {
  # YES 情况
  text_yes <- "8.4 检查\n====================================\nHas deviation: YES"
  result_yes <- parse_check_output(text = text_yes)
  expect_true(result_yes$has_deviation[1])

  # NO 情况
  text_no <- "8.4 检查\n====================================\nHas deviation: NO"
  result_no <- parse_check_output(text = text_no)
  expect_false(result_no$has_deviation[1])
})

test_that("parse_check_output() handles empty or invalid input", {
  # 空输入应该返回默认值
  result <- parse_check_output(text = "")

  expect_s3_class(result, "data.frame")
  expect_equal(result$check_name[1], "unnamed_check")
  expect_false(result$has_deviation[1])
})

test_that("parse_check_output() extracts multiple subjects", {
  text <- paste0(
    "8.4 访视超窗检查\n====================================\n",
    "Has deviation: YES\n\nFindings:\n- 发现多个问题\n\nDeviation Details:\n",
    "受试者001访视超窗\n受试者002访视超窗\n受试者003访视超窗"
  )

  result <- parse_check_output(text = text)

  expect_equal(nrow(result), 3)
  expect_equal(result$SUBJID, c("001", "002", "003"))
  expect_true(all(result$has_deviation))
})


# =============================================================================
# 测试 apply_format_mapping() 函数
# =============================================================================

test_that("apply_format_mapping() correctly maps values", {
  # 创建测试数据
  data <- data.frame(
    SEX = c("1", "2", "1"),
    AGE = c(25, 30, 35),
    stringsAsFactors = FALSE
  )

  # 创建格式定义
  format_df <- data.frame(
    FMTNAME = c("SEX", "SEX"),
    START = c("1", "2"),
    LABEL = c("男", "女"),
    stringsAsFactors = FALSE
  )

  # 应用映射
  result <- apply_format_mapping(data, format_df)

  # 验证
  expect_equal(result$SEX, c("男", "女", "男"))
  expect_equal(result$AGE, c(25, 30, 35)) # AGE 应该保持不变
})

test_that("apply_format_mapping() handles unmapped values", {
  data <- data.frame(
    SEX = c("1", "2", "3"), # "3" 没有映射
    stringsAsFactors = FALSE
  )

  format_df <- data.frame(
    FMTNAME = c("SEX", "SEX"),
    START = c("1", "2"),
    LABEL = c("男", "女"),
    stringsAsFactors = FALSE
  )

  result <- apply_format_mapping(data, format_df)

  # "3" 应该保持原值
  expect_equal(result$SEX, c("男", "女", "3"))
})

test_that("apply_format_mapping() handles columns without format", {
  data <- data.frame(
    NAME = c("张三", "李四"),
    AGE = c(25, 30),
    stringsAsFactors = FALSE
  )

  format_df <- data.frame(
    FMTNAME = character(),
    START = character(),
    LABEL = character(),
    stringsAsFactors = FALSE
  )

  result <- apply_format_mapping(data, format_df)

  # 数据应该保持不变
  expect_equal(result$NAME, data$NAME)
  expect_equal(result$AGE, data$AGE)
})


# =============================================================================
# 测试 capture_check_results() 函数
# =============================================================================

test_that("capture_check_results() captures and parses output", {
  skip_if_not_installed("dplyr")

  # 创建一个简单的测试检查函数
  test_check <- function(data) {
    cat("8.4 测试检查\n")
    cat("====================================\n")
    cat("Has deviation: NO\n")
    cat("\nFindings:\n")
    cat("- 无偏差\n")

    return(list(
      has_deviation = FALSE,
      messages = "无偏差",
      details = data.frame()
    ))
  }

  # 捕获结果
  result <- capture_check_results(
    test_check = function(data) test_check(data),
    data = NULL
  )

  # 验证
  expect_s3_class(result, "data.frame")
  expect_true("check_name" %in% names(result))
})
