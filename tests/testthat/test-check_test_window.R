# 测试 check_test_window 函数
# =============================================================================

# 构造 generate_test_window_dates 输出风格的测试数据
# 001 全部合规；002 的 V1 血生化、V2 血常规超窗
create_test_window_dates <- function() {
  data.frame(
    SUBJID = c(
      "001", "001", "001", "001",
      "002", "002", "002", "002"
    ),
    VISIT = c(
      "V1", "V1", "V2", "V2",
      "V1", "V1", "V2", "V2"
    ),
    VISITNUM = c(1, 1, 2, 2, 1, 1, 2, 2),
    SVDAT = as.Date(c(
      "2024-01-10", "2024-01-10", "2024-02-01", "2024-02-01",
      "2024-01-12", "2024-01-12", "2024-02-03", "2024-02-03"
    )),
    TBNAME = "LB",
    TESTCAT = c(
      "血常规", "血生化", "血常规", "血生化",
      "血常规", "血生化", "血常规", "血生化"
    ),
    TESTDAT = as.Date(c(
      "2024-01-10", "2024-01-09", "2024-01-31", "2024-02-01",
      "2024-01-11", "2024-01-20", "2024-01-25", "2024-02-03"
    )),
    wp_rule = c(
      "RD(-7d)", "SV(±3d)", "EX(≤24h)", "EX(0)",
      "RD(-7d)", "SV(±3d)", "EX(≤24h)", "EX(0)"
    ),
    ref = c(
      "RD", "SV", "EX", "EX",
      "RD", "SV", "EX", "EX"
    ),
    wp = c(
      "-7d", "±3d", "≤24h", "0",
      "-7d", "±3d", "≤24h", "0"
    ),
    type = c(
      "-", "±", "≤", "0",
      "-", "±", "≤", "0"
    ),
    wpvalue = c(7, 3, 1, 0, 7, 3, 1, 0),
    # Fixture uses precomputed Date windows for EX(≤24h) (~1 day);
    # keep wp_unit = "d" so datetime columns are not required here.
    wp_unit = c("d", "d", "d", "d", "d", "d", "d", "d"),
    anchor_date = as.Date(c(
      "2024-01-15", "2024-01-10", "2024-02-01", "2024-02-01",
      "2024-01-15", "2024-01-12", "2024-02-03", "2024-02-03"
    )),
    window_start = as.Date(c(
      "2024-01-08", "2024-01-07", "2024-01-31", "2024-02-01",
      "2024-01-08", "2024-01-09", "2024-02-02", "2024-02-03"
    )),
    window_end = as.Date(c(
      "2024-01-15", "2024-01-13", "2024-02-01", "2024-02-01",
      "2024-01-15", "2024-01-15", "2024-02-03", "2024-02-03"
    )),
    window_status = "ok",
    cyc_dose_date = as.Date(c(
      NA, NA, "2024-02-01", "2024-02-01",
      NA, NA, "2024-02-03", "2024-02-03"
    )),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 基本结构
# =============================================================================

test_that("check_test_window 返回正确的类和结构", {
  result <- check_test_window(create_test_window_dates())

  expect_s3_class(result, "test_window_check")
  expect_type(result$has_deviation, "logical")
  expect_true(is.character(result$messages))
  expect_true(is.data.frame(result$details))
})

test_that("check_test_window details 包含所有必需的列", {
  result <- check_test_window(create_test_window_dates())

  expected_cols <- c(
    "PDNO", "SUBJID", "VISIT", "VISITNUM", "TESTCAT",
    "wp_rule", "ref", "wp_unit", "anchor_date", "test_date",
    "window_start", "window_end", "deviation_days", "deviation_hours",
    "TBNAME", "DESCRIPTION"
  )
  expect_true(all(expected_cols %in% names(result$details)))
})

# =============================================================================
# 超窗判定
# =============================================================================

test_that("check_test_window 正确识别超窗检查项", {
  result <- check_test_window(create_test_window_dates())

  expect_true(result$has_deviation)
  # 002 的 V1 血生化（SV±3d 超窗）和 V2 血常规（EX≤24h 超窗）
  expect_equal(nrow(result$details), 2)
  expect_true(all(result$details$SUBJID == "002"))

  sv_dev <- result$details[result$details$ref == "SV", ]
  expect_equal(sv_dev$TESTCAT, "血生化")
  expect_equal(sv_dev$deviation_days, 8)

  ex_dev <- result$details[result$details$ref == "EX", ]
  expect_equal(ex_dev$TESTCAT, "血常规")
  expect_equal(ex_dev$window_start, as.Date("2024-02-02"))
})

test_that("check_test_window 全部合规时无偏差", {
  data <- create_test_window_dates()
  result <- check_test_window(data[data$SUBJID == "001", ])

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("check_test_window 窗口边界值在窗内", {
  data <- create_test_window_dates()
  data <- data[data$SUBJID == "001" & data$TESTCAT == "血生化" & data$VISITNUM == 1, ]

  # 恰在窗口起点
  data$TESTDAT <- data$window_start
  expect_false(check_test_window(data)$has_deviation)

  # 恰在窗口终点
  data$TESTDAT <- data$window_end
  expect_false(check_test_window(data)$has_deviation)

  # 超出终点1天
  data$TESTDAT <- data$window_end + 1
  expect_true(check_test_window(data)$has_deviation)
})

test_that("check_test_window 同一检查多条记录时任一在窗内即合规", {
  data <- create_test_window_dates()
  data <- data[data$SUBJID == "001" & data$TESTCAT == "血生化" & data$VISITNUM == 1, ]
  # 同一访视两条记录：一条超窗、一条在窗内
  extra <- data
  extra$TESTDAT <- as.Date("2024-01-30")
  combined <- rbind(data, extra)

  result <- check_test_window(combined)

  expect_false(result$has_deviation)
})

test_that("check_test_window 跳过 TESTDAT 缺失和窗口为 NA 的行", {
  data <- create_test_window_dates()
  # 002 的检查全部缺失
  data$TESTDAT[data$SUBJID == "002"] <- NA
  result <- check_test_window(data)
  expect_false(result$has_deviation)

  # 002 的窗口全部无法推导（如 no_anchor_data）
  data <- create_test_window_dates()
  data$window_start[data$SUBJID == "002"] <- NA
  data$window_end[data$SUBJID == "002"] <- NA
  result <- check_test_window(data)
  expect_false(result$has_deviation)
})

# =============================================================================
# 参数校验
# =============================================================================

test_that("check_test_window 校验输入参数", {
  expect_error(
    check_test_window("not a data frame"),
    "must be a data frame"
  )

  # prepare_test_data 的输出（没有窗口日期列）应报错，强制走新流程
  data <- create_test_window_dates()
  data$anchor_date <- NULL
  data$window_start <- NULL
  data$window_end <- NULL
  expect_error(
    check_test_window(data),
    "generate_test_window_dates"
  )

  expect_error(
    check_test_window(create_test_window_dates(), pdno = c("a", "b")),
    "'pdno' must be a single character string"
  )
})

# =============================================================================
# PDNO 与 print 方法
# =============================================================================

test_that("check_test_window details 包含正确的 PDNO 和 DESCRIPTION", {
  result <- check_test_window(create_test_window_dates(), pdno = "8.4.9")

  expect_true(all(result$details$PDNO == "8.4.9"))
  expect_true(all(grepl("受试者编号002", result$details$DESCRIPTION)))
  # SV/RD -> 不在窗口期；EX/FD -> 计划在…进行
  expect_true(any(grepl("不在窗口期", result$details$DESCRIPTION)))
  expect_true(any(grepl("计划在", result$details$DESCRIPTION)))
})

test_that("print.test_window_check 正常工作", {
  result <- check_test_window(create_test_window_dates())

  output <- capture.output(print(result))
  expect_true(any(grepl("8.4.2 检查项超窗检查", output)))
  expect_true(any(grepl("Has deviation: YES", output)))
})

test_that("print.test_window_check 在无偏差时正常工作", {
  data <- create_test_window_dates()
  result <- check_test_window(data[data$SUBJID == "001", ])

  output <- capture.output(print(result))
  expect_true(any(grepl("Has deviation: NO", output)))
})

test_that("check_test_window 结果可用于 as_check_df", {
  result <- check_test_window(create_test_window_dates())

  df <- as_check_df(result)
  expect_true(all(c("PDNO", "SUBJID", "TBNAME", "DESCRIPTION", "check_name", "has_deviation") %in% names(df)))
  expect_equal(nrow(df), 2)
  expect_equal(df$check_name[1], "test_window_check")
})

# =============================================================================
# 端到端：generate_test_window_dates -> check_test_window
# =============================================================================

test_that("完整流程 generate_test_window_dates 到 check_test_window", {
  prepared <- data.frame(
    SUBJID = c("001", "002"),
    VISIT = "V2",
    VISITNUM = 2,
    SVDAT = as.Date(c("2024-02-01", "2024-02-03")),
    TBNAME = "LB",
    TESTCAT = "血常规",
    TESTDAT = as.Date(c("2024-01-31", "2024-01-25")),
    TESTTIM = c("00:00", "00:00"),
    rd_date = as.Date(c(NA, NA)),
    cyc_dose_date = as.Date(c("2024-02-01", "2024-02-03")),
    cyc_dose_time = c("00:00", "00:00"),
    wp_rule = "EX(≤24h)",
    ref = "EX",
    wp = "≤24h",
    type = "≤",
    wpvalue = 24,
    wp_unit = "h",
    stringsAsFactors = FALSE
  )

  testwp_dates <- generate_test_window_dates(prepared)
  result <- check_test_window(testwp_dates)

  expect_true(result$has_deviation)
  expect_equal(result$details$SUBJID, "002")
  expect_equal(result$details$wp_unit, "h")
  expect_equal(result$details$deviation_hours, -216)
  expect_true(is.na(result$details$deviation_days))
})

test_that("小时级窗口结合给药/检查时间精确比较", {
  prepared <- data.frame(
    SUBJID = c("001", "002"),
    VISIT = "V2",
    VISITNUM = 2,
    SVDAT = as.Date(c("2024-02-01", "2024-02-01")),
    TBNAME = "LB",
    TESTCAT = "血常规",
    # 给药 10:00；001 检查 09:00（窗内），002 检查 前一日 09:00（超窗 >24h）
    TESTDAT = as.Date(c("2024-02-01", "2024-01-31")),
    TESTTIM = c("09:00", "09:00"),
    cyc_dose_date = as.Date(c("2024-02-01", "2024-02-01")),
    cyc_dose_time = c("10:00", "10:00"),
    wp_rule = "EX(≤24h)",
    ref = "EX",
    wp = "≤24h",
    type = "≤",
    wpvalue = 24,
    wp_unit = "h",
    stringsAsFactors = FALSE
  )

  testwp_dates <- generate_test_window_dates(prepared)
  # 窗口：2024-01-31 10:00 ～ 2024-02-01 10:00
  expect_equal(
    format(testwp_dates$window_start_dt[1], tz = "UTC"),
    "2024-01-31 10:00:00"
  )
  expect_equal(
    format(testwp_dates$window_end_dt[1], tz = "UTC"),
    "2024-02-01 10:00:00"
  )

  result <- check_test_window(testwp_dates)
  expect_true(result$has_deviation)
  expect_equal(result$details$SUBJID, "002")
  expect_equal(result$details$deviation_hours, -25)
})
