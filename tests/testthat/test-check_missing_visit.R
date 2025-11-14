# 测试 check_missing_visit 函数

# ===== 测试辅助函数：创建模拟数据 =====

create_test_planned_dates <- function() {
  # 创建一个标准的 planned_dates 数据框
  data.frame(
    SUBJID = rep(c("001", "002", "003"), each = 5),
    VISIT = rep(c("筛选访视", "C1D1", "C1D8", "C2D1", "EOT"), 3),
    VISITNUM = rep(c(0, 1, 2, 3, 99), 3),
    visittype = rep(c("筛选", "治疗周期1", "治疗周期1", "治疗周期2", "治疗结束"), 3),
    visitday = rep(c(NA, "1", "8", "1", "EOT"), 3),
    planned_date = c(
      # 001: 首次用药 2024-01-01
      as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-08"),
      as.Date("2024-01-29"), as.Date("2024-03-15"),
      # 002: 首次用药 2024-02-01
      as.Date("2024-02-01"), as.Date("2024-02-01"), as.Date("2024-02-08"),
      as.Date("2024-02-29"), as.Date("2024-04-15"),
      # 003: 首次用药 2024-03-01
      as.Date("2024-03-01"), as.Date("2024-03-01"), as.Date("2024-03-08"),
      as.Date("2024-03-29"), as.Date("2024-05-15")
    ),
    wp_start = as.Date(NA),
    wp_end = as.Date(NA),
    wp_type = NA_character_,
    wp_value = NA_real_,
    actual_date = c(
      # 001: 完成了筛选、C1D1、C1D8，缺失 C2D1、EOT
      as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-08"),
      as.Date(NA), as.Date(NA),
      # 002: 完成了所有访视
      as.Date("2024-02-01"), as.Date("2024-02-01"), as.Date("2024-02-08"),
      as.Date("2024-02-29"), as.Date("2024-04-15"),
      # 003: 只完成了筛选，缺失 C1D1、C1D8、C2D1、EOT
      as.Date("2024-03-01"), as.Date(NA), as.Date(NA),
      as.Date(NA), as.Date(NA)
    ),
    status = c(
      # 001
      "completed", "completed", "completed", "missing", "missing",
      # 002
      "completed", "completed", "completed", "completed", "completed",
      # 003
      "completed", "missing", "missing", "missing", "missing"
    ),
    first_ex_date = rep(c(as.Date("2024-01-01"), as.Date("2024-02-01"), as.Date("2024-03-01")), each = 5),
    eot_date = c(
      rep(as.Date("2024-03-15"), 5),
      rep(as.Date("2024-04-15"), 5),
      rep(as.Date("2024-05-15"), 5)
    ),
    eos_date = c(
      rep(as.Date("2024-06-30"), 5),
      rep(as.Date("2024-07-31"), 5),
      rep(as.Date("2024-08-31"), 5)
    ),
    stringsAsFactors = FALSE
  )
}

create_empty_planned_dates <- function() {
  data.frame(
    SUBJID = character(),
    VISIT = character(),
    VISITNUM = numeric(),
    visittype = character(),
    visitday = character(),
    planned_date = as.Date(character()),
    wp_start = as.Date(character()),
    wp_end = as.Date(character()),
    wp_type = character(),
    wp_value = numeric(),
    actual_date = as.Date(character()),
    status = character(),
    first_ex_date = as.Date(character()),
    eot_date = as.Date(character()),
    eos_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

# ===== 基本功能测试 =====

test_that("check_missing_visit 返回正确的类和结构", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 检查返回类型
  expect_s3_class(result, "missing_visit_check")
  expect_type(result, "list")

  # 检查必需的组件
  expect_true("has_deviation" %in% names(result))
  expect_true("messages" %in% names(result))
  expect_true("details" %in% names(result))
  expect_true("planned_visits" %in% names(result))

  # 检查数据类型
  expect_type(result$has_deviation, "logical")
  expect_type(result$messages, "character")
  expect_s3_class(result$details, "data.frame")
  expect_s3_class(result$planned_visits, "data.frame")
})

test_that("check_missing_visit 正确识别缺失访视", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 应该有偏差（001和003有缺失访视）
  expect_true(result$has_deviation)
  expect_true(length(result$messages) > 0)

  # details 中应该有2个受试者（001和003）
  expect_equal(nrow(result$details), 2)
  expect_true("001" %in% result$details$SUBJID)
  expect_true("003" %in% result$details$SUBJID)

  # 002 不应该在 details 中（所有访视都完成了）
  expect_false("002" %in% result$details$SUBJID)
})

test_that("check_missing_visit details 包含所有必需的列", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  expected_cols <- c(
    "SUBJID", "first_ex_date", "missing_visits", "eot_date",
    "eos_date", "cutoffdt", "valid_visits_count", "completed_visits_count"
  )

  expect_true(all(expected_cols %in% names(result$details)))
})

test_that("check_missing_visit 缺失访视文本格式正确", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 检查 001 的缺失访视文本
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_true(grepl("C2D1", subj_001$missing_visits))
  expect_true(grepl("EOT", subj_001$missing_visits))
  # 应该包含计划日期
  expect_true(grepl("\\(", subj_001$missing_visits)) # 包含括号
  expect_true(grepl("2024-", subj_001$missing_visits)) # 包含日期
})

# ===== 边界情况测试 =====

test_that("check_missing_visit 处理所有访视都完成的情况", {
  planned_dates <- create_test_planned_dates()
  # 只保留002的数据（所有访视都完成）
  planned_dates <- planned_dates[planned_dates$SUBJID == "002", ]

  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("check_missing_visit 处理所有访视都缺失的情况", {
  planned_dates <- create_test_planned_dates()
  # 创建一个所有访视都缺失的受试者
  planned_dates$status <- "missing"

  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 3) # 三个受试者都有缺失
})

test_that("check_missing_visit 处理空数据框抛出错误", {
  empty_df <- data.frame()

  expect_error(
    check_missing_visit(empty_df),
    "planned_dates 缺少必要的列"
  )
})

test_that("check_missing_visit 处理缺少必需列的情况", {
  planned_dates <- create_test_planned_dates()
  # 删除一个必需列
  planned_dates$status <- NULL

  expect_error(
    check_missing_visit(planned_dates),
    "planned_dates 缺少必要的列.*status"
  )
})

# ===== 输入验证测试 =====

test_that("check_missing_visit 验证 planned_dates 是 data frame", {
  expect_error(
    check_missing_visit("not a data frame"),
    "planned_dates 必须是 data frame 类型"
  )

  expect_error(
    check_missing_visit(list(a = 1, b = 2)),
    "planned_dates 必须是 data frame 类型"
  )
})

test_that("check_missing_visit 自动转换 cutoffdt 为 Date 类型", {
  planned_dates <- create_test_planned_dates()

  # 使用字符串日期
  result1 <- check_missing_visit(planned_dates, cutoffdt = "2024-12-31")
  expect_s3_class(result1, "missing_visit_check")

  # 使用 Date 对象
  result2 <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))
  expect_s3_class(result2, "missing_visit_check")
})

# ===== 截止日期逻辑测试 =====

test_that("check_missing_visit 截止日期影响缺失访视判断 - 筛选期和治疗期", {
  planned_dates <- create_test_planned_dates()

  # 使用早期截止日期（2024-01-15），只有部分访视应该完成
  result_early <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-01-15"))

  # 使用晚期截止日期（2024-12-31），更多访视应该完成
  result_late <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 晚期截止日期应该识别更多缺失访视
  expect_true(result_late$has_deviation)
})

test_that("check_missing_visit 治疗期访视截止日期逻辑：min(eot, eos, cutoffdt)", {
  planned_dates <- create_test_planned_dates()

  # 001: eot = 2024-03-15, eos = 2024-06-30
  # C2D1 计划日期 = 2024-01-29
  # 使用 cutoffdt = 2024-03-01（早于 eot），C2D1 应该被检查
  result1 <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-03-01"))
  subj_001_1 <- result1$details[result1$details$SUBJID == "001", ]
  expect_true(grepl("C2D1", subj_001_1$missing_visits))

  # 使用 cutoffdt = 2024-01-20（早于 C2D1 计划日期），C2D1 不应该被检查
  result2 <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-01-20"))
  # 001 可能仍在结果中（因为其他原因），但 C2D1 不应该在缺失列表中
  if ("001" %in% result2$details$SUBJID) {
    subj_001_2 <- result2$details[result2$details$SUBJID == "001", ]
    # C2D1 的计划日期是 2024-01-29，晚于截止日期，所以不会被算作应完成的访视
    expect_true(is.na(subj_001_2$missing_visits) || !grepl("C2D1", subj_001_2$missing_visits))
  }
})

test_that("check_missing_visit 治疗结束访视截止日期逻辑：min(eos, cutoffdt)", {
  planned_dates <- create_test_planned_dates()

  # 001: EOT 计划日期 = 2024-03-15, eos = 2024-06-30
  # 使用 cutoffdt = 2024-03-20（晚于 EOT 计划日期，早于 eos），EOT 应该被检查
  result1 <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-03-20"))
  subj_001_1 <- result1$details[result1$details$SUBJID == "001", ]
  expect_true(grepl("EOT", subj_001_1$missing_visits))

  # 使用 cutoffdt = 2024-03-10（早于 EOT 计划日期），EOT 不应该被检查
  result2 <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-03-10"))
  if ("001" %in% result2$details$SUBJID) {
    subj_001_2 <- result2$details[result2$details$SUBJID == "001", ]
    # EOT 不在缺失列表中（因为计划日期晚于截止日期）
    expect_false(grepl("EOT", subj_001_2$missing_visits))
  }
})


# ===== 统计计算测试 =====

test_that("check_missing_visit 正确计算访视统计数据", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 检查 001 的统计
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_true(subj_001$valid_visits_count > 0)
  expect_true(subj_001$completed_visits_count > 0)
  expect_true(subj_001$completed_visits_count < subj_001$valid_visits_count)

  # 缺失数 = 有效访视数 - 完成数
  missing_count <- subj_001$valid_visits_count - subj_001$completed_visits_count
  # 检查缺失访视文本中的访视数量
  missing_visits_count <- length(strsplit(subj_001$missing_visits, "、")[[1]])
  expect_equal(missing_count, missing_visits_count)
})

test_that("check_missing_visit 正确记录各种日期", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  subj_001 <- result$details[result$details$SUBJID == "001", ]

  # 检查日期列存在且为 Date 类型
  expect_true("first_ex_date" %in% names(subj_001))
  expect_true("eot_date" %in% names(subj_001))
  expect_true("eos_date" %in% names(subj_001))
  expect_true("cutoffdt" %in% names(subj_001))

  # 检查日期值
  expect_equal(subj_001$first_ex_date, as.Date("2024-01-01"))
  expect_equal(subj_001$eot_date, as.Date("2024-03-15"))
  expect_equal(subj_001$eos_date, as.Date("2024-06-30"))
  expect_equal(subj_001$cutoffdt, as.Date("2024-12-31"))
})

# ===== 打印方法测试 =====

test_that("print.missing_visit_check 正常工作", {
  planned_dates <- create_test_planned_dates()
  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  # 捕获打印输出
  output <- capture.output(print.missing_visit_check(result))

  # 检查输出内容
  expect_true(any(grepl("8.2 按计划进行现场访视", output)))
  expect_true(any(grepl("Has deviation", output)))
  expect_true(any(grepl("Findings", output)))
  expect_true(any(grepl("Deviation Details", output)))
  expect_true(any(grepl("受试者编号", output)))
  expect_true(any(grepl("首次用药时间", output)))
})

test_that("print.missing_visit_check 在无偏差时正常工作", {
  planned_dates <- create_test_planned_dates()
  # 只保留 002（无缺失访视）
  planned_dates <- planned_dates[planned_dates$SUBJID == "002", ]

  result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))

  output <- capture.output(print.missing_visit_check(result))

  expect_true(any(grepl("Has deviation: NO", output)))
})

# ===== NA 值处理测试 =====

test_that("check_missing_visit 处理 NA 日期", {
  planned_dates <- create_test_planned_dates()
  # 将某些日期设为 NA
  planned_dates$eot_date[planned_dates$SUBJID == "003"] <- as.Date(NA)
  planned_dates$eos_date[planned_dates$SUBJID == "003"] <- as.Date(NA)

  # 应该不会出错
  expect_no_error({
    result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))
  })

  # 003 应该仍然在结果中
  expect_true("003" %in% result$details$SUBJID)
})

test_that("check_missing_visit 处理 NA planned_date", {
  planned_dates <- create_test_planned_dates()
  # 将某个访视的计划日期设为 NA
  planned_dates$planned_date[1] <- as.Date(NA)

  # 应该不会出错
  expect_no_error({
    result <- check_missing_visit(planned_dates, cutoffdt = as.Date("2024-12-31"))
  })
})
