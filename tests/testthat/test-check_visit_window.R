# 测试 check_visit_window 函数

# ===== 测试辅助函数：创建模拟数据 =====

create_test_visit_window_data <- function() {
  # 创建一个标准的 planned_dates 数据框，包含窗口期信息
  data.frame(
    SUBJID = rep(c("001", "002", "003"), each = 4),
    VISIT = rep(c("筛选访视", "C1D1", "C1D8", "C2D1"), 3),
    VISITNUM = rep(c(0, 1, 2, 3), 3),
    planned_date = c(
      # 001: 首次用药 2024-01-01
      as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-08"),
      as.Date("2024-01-29"),
      # 002: 首次用药 2024-02-01
      as.Date("2024-02-01"), as.Date("2024-02-01"), as.Date("2024-02-08"),
      as.Date("2024-02-29"),
      # 003: 首次用药 2024-03-01
      as.Date("2024-03-01"), as.Date("2024-03-01"), as.Date("2024-03-08"),
      as.Date("2024-03-29")
    ),
    wp_start = c(
      # 001: 窗口期开始
      as.Date("2023-12-29"), as.Date("2023-12-29"), as.Date("2024-01-05"),
      as.Date("2024-01-26"),
      # 002
      as.Date("2024-01-29"), as.Date("2024-01-29"), as.Date("2024-02-05"),
      as.Date("2024-02-26"),
      # 003
      as.Date("2024-02-27"), as.Date("2024-02-27"), as.Date("2024-03-05"),
      as.Date("2024-03-26")
    ),
    wp_end = c(
      # 001: 窗口期结束
      as.Date("2024-01-04"), as.Date("2024-01-04"), as.Date("2024-01-11"),
      as.Date("2024-02-01"),
      # 002
      as.Date("2024-02-04"), as.Date("2024-02-04"), as.Date("2024-02-11"),
      as.Date("2024-03-03"),
      # 003
      as.Date("2024-03-04"), as.Date("2024-03-04"), as.Date("2024-03-11"),
      as.Date("2024-04-01")
    ),
    wp_type = rep(c("±", "±", "±", "±"), 3),
    wp_value = rep(c(3, 3, 3, 3), 3),
    actual_date = c(
      # 001: 筛选和C1D1在窗口期内，C1D8超窗，C2D1超窗
      as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-15"),
      as.Date("2024-02-05"),
      # 002: 所有访视都在窗口期内
      as.Date("2024-02-01"), as.Date("2024-02-01"), as.Date("2024-02-08"),
      as.Date("2024-02-29"),
      # 003: 筛选在窗口期内，C1D1提前超窗，C1D8在窗口期内，C2D1延后超窗
      as.Date("2024-03-01"), as.Date("2024-02-20"), as.Date("2024-03-08"),
      as.Date("2024-04-10")
    ),
    status = rep("completed", 12),
    first_dose_date = rep(c(as.Date("2024-01-01"), as.Date("2024-02-01"), as.Date("2024-03-01")), each = 4),
    stringsAsFactors = FALSE
  )
}

create_empty_visit_window_data <- function() {
  data.frame(
    SUBJID = character(),
    VISIT = character(),
    VISITNUM = numeric(),
    planned_date = as.Date(character()),
    wp_start = as.Date(character()),
    wp_end = as.Date(character()),
    wp_type = character(),
    wp_value = numeric(),
    actual_date = as.Date(character()),
    status = character(),
    first_dose_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

# ===== 基本功能测试 =====

test_that("check_visit_window 返回正确的类和结构", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 检查返回类型
  expect_s3_class(result, "visit_window_check")
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

test_that("check_visit_window 正确识别超窗访视", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 应该有偏差（001和003有超窗访视）
  expect_true(result$has_deviation)
  expect_true(length(result$messages) > 0)

  # details 中应该有4条超窗记录（001有2个，003有2个）
  expect_equal(nrow(result$details), 4)
  expect_true("001" %in% result$details$SUBJID)
  expect_true("003" %in% result$details$SUBJID)

  # 002 不应该在 details 中（所有访视都在窗口期内）
  expect_false("002" %in% result$details$SUBJID)

  # 检查每个超窗访视都是独立的记录
  subj_001_records <- result$details[result$details$SUBJID == "001", ]
  expect_equal(nrow(subj_001_records), 2) # 001有2个超窗访视
  expect_true("C1D8" %in% subj_001_records$VISIT)
  expect_true("C2D1" %in% subj_001_records$VISIT)

  subj_003_records <- result$details[result$details$SUBJID == "003", ]
  expect_equal(nrow(subj_003_records), 2) # 003有2个超窗访视
  expect_true("C1D1" %in% subj_003_records$VISIT)
  expect_true("C2D1" %in% subj_003_records$VISIT)
})

test_that("check_visit_window details 包含所有必需的列", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  expected_cols <- c(
    "SUBJID", "first_dose_date", "VISIT", "VISITNUM", "planned_date",
    "actual_date", "wp_start", "wp_end", "wp_type", "wp_value",
    "deviation_days", "total_completed_visits", "out_of_window_count"
  )

  expect_true(all(expected_cols %in% names(result$details)))
})

test_that("check_visit_window 正确计算偏差天数", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 检查 001 的 C1D8 访视（计划日期：2024-01-08，实际日期：2024-01-15）
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  c1d8_record <- subj_001[subj_001$VISIT == "C1D8", ]

  expect_equal(nrow(c1d8_record), 1)
  # 偏差天数：2024-01-15 - 2024-01-08 = 7天
  expect_equal(c1d8_record$deviation_days, 7)

  # 检查 003 的 C1D1 访视（计划日期：2024-03-01，实际日期：2024-02-20）
  subj_003 <- result$details[result$details$SUBJID == "003", ]
  c1d1_record <- subj_003[subj_003$VISIT == "C1D1", ]

  expect_equal(nrow(c1d1_record), 1)
  # 偏差天数：2024-02-20 - 2024-03-01 = -10天（提前）
  expect_equal(c1d1_record$deviation_days, -10)
})

# ===== 边界情况测试 =====

test_that("check_visit_window 处理所有访视都在窗口期内的情况", {
  planned_dates <- create_test_visit_window_data()
  # 只保留002的数据（所有访视都在窗口期内）
  planned_dates <- planned_dates[planned_dates$SUBJID == "002", ]

  result <- check_visit_window(planned_dates)

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("check_visit_window 处理所有访视都超窗的情况", {
  planned_dates <- create_test_visit_window_data()
  # 将所有访视的实际日期设为超窗
  planned_dates$actual_date <- planned_dates$wp_end + 10

  result <- check_visit_window(planned_dates)

  expect_true(result$has_deviation)
  # 3个受试者 × 4个访视 = 12条超窗记录
  expect_equal(nrow(result$details), 12)
})

test_that("check_visit_window 处理没有已完成访视的情况", {
  planned_dates <- create_test_visit_window_data()
  # 将所有访视标记为未完成
  planned_dates$status <- "missing"

  result <- check_visit_window(planned_dates)

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})


test_that("check_visit_window 处理缺少必需列的情况", {
  planned_dates <- create_test_visit_window_data()
  # 删除一个必需列
  planned_dates$wp_start <- NULL

  expect_error(
    check_visit_window(planned_dates),
    "'planned_dates' is missing required columns: wp_start"
  )
})

# ===== 输入验证测试 =====

test_that("check_visit_window 验证 planned_dates 是 data frame", {
  expect_error(
    check_visit_window("not a data frame"),
    "'planned_dates' must be a data frame"
  )

  expect_error(
    check_visit_window(list(a = 1, b = 2)),
    "'planned_dates' must be a data frame"
  )
})

# ===== 窗口期逻辑测试 =====

test_that("check_visit_window 正确判断访视是否在窗口期内", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 002的所有访视都应该在窗口期内
  expect_false("002" %in% result$details$SUBJID)

  # 001的筛选和C1D1应该在窗口期内，不出现在details中
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_false("筛选访视" %in% subj_001$VISIT)
  expect_false("C1D1" %in% subj_001$VISIT)

  # 001的C1D8和C2D1应该超窗
  expect_true("C1D8" %in% subj_001$VISIT)
  expect_true("C2D1" %in% subj_001$VISIT)
})

test_that("check_visit_window 处理窗口期边界值", {
  planned_dates <- create_test_visit_window_data()

  # 设置一个访视正好在窗口期开始日期
  planned_dates$actual_date[1] <- planned_dates$wp_start[1]

  result <- check_visit_window(planned_dates)

  # 正好在窗口期开始日期应该被认为在窗口期内
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_false("筛选访视" %in% subj_001$VISIT)

  # 设置一个访视正好在窗口期结束日期
  planned_dates$actual_date[2] <- planned_dates$wp_end[2]

  result2 <- check_visit_window(planned_dates)

  # 正好在窗口期结束日期应该被认为在窗口期内
  subj_001_2 <- result2$details[result2$details$SUBJID == "001", ]
  expect_false("C1D1" %in% subj_001_2$VISIT)
})

# ===== 统计计算测试 =====

test_that("check_visit_window 正确计算访视统计数据", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 检查 001 的统计
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_true(nrow(subj_001) > 0)

  # 所有记录应该有相同的统计值
  expect_true(all(subj_001$total_completed_visits == 4)) # 001完成了4个访视
  expect_true(all(subj_001$out_of_window_count == 2)) # 其中2个超窗
})

# ===== NA 值处理测试 =====

test_that("check_visit_window 处理 NA 窗口期日期", {
  planned_dates <- create_test_visit_window_data()
  # 将某些窗口期日期设为 NA
  planned_dates$wp_start[1] <- as.Date(NA)
  planned_dates$wp_end[1] <- as.Date(NA)

  # 应该不会出错
  expect_no_error({
    result <- check_visit_window(planned_dates)
  })

  # 有NA窗口期的访视不会被检查超窗
  subj_001 <- result$details[result$details$SUBJID == "001", ]
  expect_false("筛选访视" %in% subj_001$VISIT)
})

test_that("check_visit_window 处理 NA 实际日期", {
  planned_dates <- create_test_visit_window_data()
  # 将某个访视的实际日期设为 NA
  planned_dates$actual_date[1] <- as.Date(NA)
  planned_dates$status[1] <- "missing"

  # 应该不会出错
  expect_no_error({
    result <- check_visit_window(planned_dates)
  })

  # 实际日期为NA的访视不会被检查
  # 因为它们的status是"missing"，不是"completed"
})

# ===== 打印方法测试 =====

test_that("print.visit_window_check 正常工作", {
  planned_dates <- create_test_visit_window_data()
  result <- check_visit_window(planned_dates)

  # 捕获打印输出
  output <- capture.output(print.visit_window_check(result))

  # 检查输出内容
  expect_true(any(grepl("8.4 访视超窗检查", output)))
  expect_true(any(grepl("Has deviation", output)))
  expect_true(any(grepl("Findings", output)))
  expect_true(any(grepl("Deviation Details", output)))
  expect_true(any(grepl("受试者编号", output)))
  expect_true(any(grepl("首次用药时间", output)))
})

test_that("print.visit_window_check 在无偏差时正常工作", {
  planned_dates <- create_test_visit_window_data()
  # 只保留 002（无超窗访视）
  planned_dates <- planned_dates[planned_dates$SUBJID == "002", ]

  result <- check_visit_window(planned_dates)

  output <- capture.output(print.visit_window_check(result))

  expect_true(any(grepl("Has deviation: NO", output)))
})

