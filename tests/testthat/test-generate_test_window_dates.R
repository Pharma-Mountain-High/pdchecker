# 测试 generate_test_window_dates 函数
# =============================================================================

# 构造 prepare_test_data 输出风格的测试数据（含窗口规则列 + 衍生日期列）
create_testwp_prepared_data <- function() {
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
    rd_date = as.Date(c(
      "2024-01-15", "2024-01-15", "2024-01-15", "2024-01-15",
      "2024-01-15", "2024-01-15", "2024-01-15", "2024-01-15"
    )),
    first_dose_date = as.Date(c(
      "2024-02-01", "2024-02-01", "2024-02-01", "2024-02-01",
      "2024-02-03", "2024-02-03", "2024-02-03", "2024-02-03"
    )),
    cyc_dose_date = as.Date(c(
      NA, NA, "2024-02-01", "2024-02-01",
      NA, NA, "2024-02-03", "2024-02-03"
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
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 基本结构
# =============================================================================

test_that("generate_test_window_dates 保留所有行并新增窗口列", {
  data <- create_testwp_prepared_data()
  result <- generate_test_window_dates(data)

  expect_equal(nrow(result), nrow(data))
  expect_true(all(c(
    "anchor_date", "window_start", "window_end", "window_status"
  ) %in% names(result)))
  # 原始列保留
  expect_true(all(names(data) %in% names(result)))
  # VISITNUM 类型不变
  expect_equal(result$VISITNUM, data$VISITNUM)
})

# =============================================================================
# 锚点解析
# =============================================================================

test_that("generate_test_window_dates 正确解析三种锚点", {
  data <- create_testwp_prepared_data()
  result <- generate_test_window_dates(data)

  expect_true(all(result$window_status == "ok"))

  # RD：随机日期
  rd_rows <- result[result$ref == "RD", ]
  expect_true(all(rd_rows$anchor_date == as.Date("2024-01-15")))

  # SV：实际访视日期
  sv_001 <- result[result$ref == "SV" & result$SUBJID == "001", ]
  expect_equal(sv_001$anchor_date, as.Date("2024-01-10"))

  # EX：该访视给药日期
  ex_002 <- result[result$ref == "EX" & result$SUBJID == "002", ]
  expect_true(all(ex_002$anchor_date == as.Date("2024-02-03")))
})

test_that("generate_test_window_dates 正确推导窗口起止", {
  data <- create_testwp_prepared_data()
  result <- generate_test_window_dates(data)

  # RD(-7d)：随机日前7天
  rd_row <- result[result$ref == "RD" & result$SUBJID == "001", ]
  expect_equal(rd_row$window_start, as.Date("2024-01-08"))
  expect_equal(rd_row$window_end, as.Date("2024-01-15"))

  # SV(±3d)
  sv_row <- result[result$ref == "SV" & result$SUBJID == "001", ]
  expect_equal(sv_row$window_start, as.Date("2024-01-07"))
  expect_equal(sv_row$window_end, as.Date("2024-01-13"))

  # EX(0)：给药当天
  zero_row <- result[result$wp_rule == "EX(0)" & result$SUBJID == "001", ]
  expect_equal(zero_row$window_start, as.Date("2024-02-01"))
  expect_equal(zero_row$window_end, as.Date("2024-02-01"))
})

# =============================================================================
# window_status 状态
# =============================================================================

test_that("缺少 rd_date 列时 RD 行标记 no_anchor_data 并警告", {
  data <- create_testwp_prepared_data()
  data$rd_date <- NULL

  expect_warning(
    result <- generate_test_window_dates(data),
    "'rd_date' column missing"
  )

  rd_rows <- result[result$ref == "RD", ]
  expect_true(all(rd_rows$window_status == "no_anchor_data"))
  expect_true(all(is.na(rd_rows$window_start)))
  # SV 行仍可用
  expect_true(all(result$window_status[result$ref == "SV"] == "ok"))
})

test_that("缺少 cyc_dose_date 列时 EX 行标记 no_anchor_data 并警告", {
  data <- create_testwp_prepared_data()
  data$cyc_dose_date <- NULL

  expect_warning(
    result <- generate_test_window_dates(data),
    "'cyc_dose_date' column missing"
  )

  ex_rows <- result[result$ref == "EX", ]
  expect_true(all(ex_rows$window_status == "no_anchor_data"))
})

test_that("generate_test_window_dates 正确解析 FD 锚点", {
  data <- create_testwp_prepared_data()
  data <- data[data$SUBJID == "001" & data$VISITNUM == 1, ][1, ]
  data$wp_rule <- "FD(±3d)"
  data$ref <- "FD"
  data$wp <- "±3d"
  data$type <- "±"
  data$wpvalue <- 3
  data$TESTDAT <- as.Date("2024-02-02")

  result <- generate_test_window_dates(data)

  expect_equal(result$window_status, "ok")
  expect_equal(result$anchor_date, as.Date("2024-02-01"))
  expect_equal(result$window_start, as.Date("2024-01-29"))
  expect_equal(result$window_end, as.Date("2024-02-04"))
})

test_that("缺少 first_dose_date 列时 FD 行标记 no_anchor_data 并警告", {
  data <- create_testwp_prepared_data()
  data <- data[1, ]
  data$wp_rule <- "FD(0)"
  data$ref <- "FD"
  data$wp <- "0"
  data$type <- "0"
  data$wpvalue <- 0
  data$first_dose_date <- NULL

  expect_warning(
    result <- generate_test_window_dates(data),
    "'first_dose_date' column missing"
  )
  expect_equal(result$window_status, "no_anchor_data")
})

test_that("锚点日期缺失的行标记 missing_anchor_date", {
  data <- create_testwp_prepared_data()
  # 002 没有随机日期
  data$rd_date[data$SUBJID == "002"] <- as.Date(NA)

  result <- generate_test_window_dates(data)

  rd_002 <- result[result$ref == "RD" & result$SUBJID == "002", ]
  expect_equal(rd_002$window_status, "missing_anchor_date")
  expect_true(is.na(rd_002$window_start))

  rd_001 <- result[result$ref == "RD" & result$SUBJID == "001", ]
  expect_equal(rd_001$window_status, "ok")
})

test_that("无法推导窗口的规则标记 unsupported_rule", {
  data <- create_testwp_prepared_data()
  # 把一行改成范围型规则（wpvalue 为 NA）
  data$type[1] <- "范围"
  data$wpvalue[1] <- NA

  result <- generate_test_window_dates(data)

  expect_equal(result$window_status[1], "unsupported_rule")
  expect_true(is.na(result$window_start[1]))
})

test_that("ref 为 NA 的行标记 no_rule", {
  data <- create_testwp_prepared_data()
  data$ref[1] <- NA

  result <- generate_test_window_dates(data)

  expect_equal(result$window_status[1], "no_rule")
  expect_true(is.na(result$window_start[1]))
})

# =============================================================================
# 参数
# =============================================================================

test_that("generate_test_window_dates 校验输入参数", {
  expect_error(
    generate_test_window_dates("not a data frame"),
    "must be a data frame"
  )

  data <- create_testwp_prepared_data()
  data$wpvalue <- NULL
  expect_error(
    generate_test_window_dates(data),
    "missing required columns"
  )
})

test_that("小时级规则推导 POSIXct 窗口", {
  data <- create_testwp_prepared_data()
  # 仅保留 EX(≤24h) 行并补充给药时间
  data <- data[data$wp_rule == "EX(≤24h)" & data$SUBJID == "001", ]
  data$cyc_dose_time <- "10:00"
  data$wp_unit <- "h"
  data$wpvalue <- 24

  result <- generate_test_window_dates(data)

  expect_equal(result$wp_unit, "h")
  expect_equal(result$window_status, "ok")
  expect_equal(
    format(result$anchor_datetime, tz = "UTC"),
    "2024-02-01 10:00:00"
  )
  expect_equal(
    format(result$window_start_dt, tz = "UTC"),
    "2024-01-31 10:00:00"
  )
  expect_equal(
    format(result$window_end_dt, tz = "UTC"),
    "2024-02-01 10:00:00"
  )
  # 日期列保留日历边界
  expect_equal(result$window_start, as.Date("2024-01-31"))
  expect_equal(result$window_end, as.Date("2024-02-01"))
})
