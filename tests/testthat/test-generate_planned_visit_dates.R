# 测试 generate_planned_visit_dates 函数

# 创建测试数据
setup_test_data <- function() {
  # 访视编码数据
  visit_schedule <- data.frame(
    VISIT = c("筛选访视", "C1D1", "C1D8", "C1D15", "C2D1", "C2D15", "治疗结束访视", "随访访视"),
    VISITNUM = c(0, 1, 2, 3, 4, 5, 99, 100),
    CYCLE = c("筛选", "治疗周期1", "治疗周期1", "治疗周期1", "治疗周期2", "治疗周期2", "治疗结束", "随访"),
    VISITDAY = c("0", "1", "8", "15", "1", "15", "EOT", "EOT+30"),
    WP = c("±3d", "±3d", "±2d", "±2d", "±3d", "±2d", "+7d", "±7d"),
    type = c("±", "±", "±", "±", "±", "±", "+", "±"),
    wpvalue = c("3", "3", "2", "2", "3", "2", "7", "7"),
    stringsAsFactors = FALSE
  )

  # 用药数据
  ex_data <- data.frame(
    SUBJID = c("001", "001", "002", "002", "003"),
    EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05", "2024-02-02", "2024-01-10"),
    stringsAsFactors = FALSE
  )

  # 访视数据
  sv_data <- data.frame(
    SUBJID = c("001", "001", "001", "002", "002", "003", "003"),
    VISIT = c("筛选访视", "C1D1", "C1D8", "筛选访视", "C1D1", "筛选访视", "C1D1"),
    VISITNUM = c(0, 1, 2, 0, 1, 0, 1),
    SVDAT = c("2024-01-01", "2024-01-01", "2024-01-08", "2024-01-05", "2024-01-05", "2024-01-10", "2024-01-10"),
    stringsAsFactors = FALSE
  )

  # 治疗结束数据
  eot_data <- data.frame(
    SUBJID = c("001", "002"),
    EOTDAT = c("2024-03-15", "2024-03-20"),
    stringsAsFactors = FALSE
  )

  # 研究结束数据
  ds_data <- data.frame(
    SUBJID = c("001", "002"),
    DSDAT = c("2024-04-15", "2024-04-20"),
    stringsAsFactors = FALSE
  )

  # 组合为 list
  data_list <- list(
    EX = ex_data,
    SV = sv_data,
    EOT = eot_data,
    DS = ds_data
  )

  list(
    data = data_list,
    visit_schedule = visit_schedule
  )
}

# ===== 基本功能测试 =====

test_that("基本功能：正常输入返回数据框", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 检查返回类型
  expect_s3_class(result, "data.frame")

  # 检查必要的列是否存在
  expected_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "planned_date",
    "wp_start", "wp_end", "actual_date", "status",
    "first_dose_date", "eot_date", "eos_date"
  )
  expect_true(all(expected_cols %in% names(result)))

  # 检查数据行数（3个受试者 × 8个访视 = 24行）
  expect_equal(nrow(result), 24)

  # 检查受试者数量
  expect_equal(length(unique(result$SUBJID)), 3)
})

test_that("基本功能：计算首次给药日期", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 检查受试者001的首次给药日期（应为2024-01-01）
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(unique(subj_001$first_dose_date)), "2024-01-01")

  # 检查受试者002的首次给药日期（应为2024-01-05）
  subj_002 <- result[result$SUBJID == "002", ]
  expect_equal(as.character(unique(subj_002$first_dose_date)), "2024-01-05")
})

# ===== 访视计划日期计算测试 =====

test_that("筛选期访视计划日期等于首次给药日期", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 筛选访视的计划日期应等于首次给药日期
  screening_visits <- result[result$VISIT == "筛选访视", ]

  expect_true(all(screening_visits$planned_date == screening_visits$first_dose_date,
    na.rm = TRUE
  ))
})

test_that("治疗期D1访视计划日期计算正确", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule,
    cycle_days = 28
  )

  # C1D1应等于首次给药日期
  c1d1_visits <- result[result$VISIT == "C1D1", ]
  expect_true(all(c1d1_visits$planned_date == c1d1_visits$first_dose_date,
    na.rm = TRUE
  ))

  # C2D1应等于首次给药日期 + 28天
  c2d1_visits <- result[result$VISIT == "C2D1" & result$SUBJID == "001", ]
  if (nrow(c2d1_visits) > 0) {
    expected_c2d1 <- as.Date("2024-01-01") + 28
    expect_equal(c2d1_visits$planned_date[1], expected_c2d1)
  }
})

test_that("治疗期非D1访视计划日期计算正确", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # C1D8 应等于 C1D1 + 7天
  subj_001 <- result[result$SUBJID == "001", ]
  c1d1_date <- subj_001$planned_date[subj_001$VISIT == "C1D1"][1]
  c1d8_date <- subj_001$planned_date[subj_001$VISIT == "C1D8"][1]

  expect_equal(as.numeric(c1d8_date - c1d1_date), 7)

  # C1D15 应等于 C1D1 + 14天
  c1d15_date <- subj_001$planned_date[subj_001$VISIT == "C1D15"][1]
  expect_equal(as.numeric(c1d15_date - c1d1_date), 14)
})

test_that("治疗结束访视计划日期计算正确", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 检查EOT访视日期
  eot_visits <- result[result$VISIT == "治疗结束访视" & result$SUBJID == "001", ]
  expect_equal(as.character(eot_visits$planned_date[1]), "2024-03-15")
})

# ===== 窗口期计算测试 =====

test_that("窗口期计算：± 类型", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 筛选访视窗口期为 ±3d
  screening <- result[result$VISIT == "筛选访视" & result$SUBJID == "001", ]
  planned <- screening$planned_date[1]

  expect_equal(screening$wp_start[1], planned - 3)
  expect_equal(screening$wp_end[1], planned + 3)
  expect_equal(screening$wp_type[1], "±")
  expect_equal(screening$wp_value[1], 3)
})

test_that("窗口期计算：+ 类型", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 治疗结束访视窗口期为 +7d
  eot <- result[result$VISIT == "治疗结束访视" & result$SUBJID == "001", ]
  planned <- eot$planned_date[1]

  expect_equal(eot$wp_start[1], planned)
  expect_equal(eot$wp_end[1], planned + 7)
  expect_equal(eot$wp_type[1], "+")
})

# ===== 访视状态测试 =====

test_that("访视状态：已完成访视标记为completed", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # C1D1有实际访视记录，应标记为completed
  c1d1_001 <- result[result$VISIT == "C1D1" & result$SUBJID == "001", ]
  expect_equal(c1d1_001$status[1], "completed")
  expect_false(is.na(c1d1_001$actual_date[1]))
})

test_that("访视状态：未完成访视标记为missing", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # C1D15没有实际访视记录，应标记为missing
  c1d15_001 <- result[result$VISIT == "C1D15" & result$SUBJID == "001", ]
  expect_equal(c1d15_001$status[1], "missing")
  expect_true(is.na(c1d15_001$actual_date[1]))
})

# ===== 多数据集支持测试 =====

test_that("多数据集支持：多个EX数据集使用相同日期列", {
  test_data <- setup_test_data()

  # 创建第二个用药数据集
  test_data$data$EX1 <- data.frame(
    SUBJID = "001",
    EXSTDAT = "2023-12-28", # 比EX中更早
    stringsAsFactors = FALSE
  )

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule,
    ex_datasets = c("EX", "EX1"),
    ex_date_var = "EXSTDAT" # 单个值，应用于所有数据集
  )

  # 应使用最早的日期（2023-12-28）
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(unique(subj_001$first_dose_date)), "2023-12-28")
})

test_that("多数据集支持：多个EX数据集使用不同日期列", {
  test_data <- setup_test_data()

  # 创建第二个用药数据集，使用不同的日期列名
  test_data$data$EX1 <- data.frame(
    SUBJID = "001",
    STDAT = "2023-12-25", # 更早的日期
    stringsAsFactors = FALSE
  )

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule,
    ex_datasets = c("EX", "EX1"),
    ex_date_var = c("EXSTDAT", "STDAT") # 一一对应
  )

  # 应使用最早的日期（2023-12-25）
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(unique(subj_001$first_dose_date)), "2023-12-25")
})

# ===== 参数验证测试 =====

test_that("参数验证：visit_schedule_data缺少必要列时报错", {
  test_data <- setup_test_data()

  # 删除必要的列
  incomplete_schedule <- test_data$visit_schedule[, -1]

  expect_error(
    generate_planned_visit_dates(
      data = test_data$data,
      visit_schedule_data = incomplete_schedule
    ),
    "visit_schedule_data is missing required columns"
  )
})

test_that("参数验证：ex_date_var长度不匹配时报错", {
  test_data <- setup_test_data()

  expect_error(
    generate_planned_visit_dates(
      data = test_data$data,
      visit_schedule_data = test_data$visit_schedule,
      ex_datasets = c("EX", "EX1"),
      ex_date_var = c("EXSTDAT", "STDAT", "EXTRA") # 长度不匹配
    ),
    "'ex_date_var' length must be 1 or equal to 'ex_datasets' length"
  )
})

test_that("参数验证：访视数据集不存在时报错", {
  test_data <- setup_test_data()

  expect_error(
    generate_planned_visit_dates(
      data = test_data$data,
      visit_schedule_data = test_data$visit_schedule,
      sv_dataset = "NONEXISTENT"
    ),
    "Missing visit dataset"
  )
})

test_that("参数验证：访视数据集缺少必要列时报错", {
  test_data <- setup_test_data()

  # 删除VISIT列
  test_data$data$SV$VISIT <- NULL

  expect_error(
    generate_planned_visit_dates(
      data = test_data$data,
      visit_schedule_data = test_data$visit_schedule
    ),
    "is missing required columns"
  )
})

# ===== 边界情况测试 =====

test_that("边界情况：受试者没有用药记录", {
  test_data <- setup_test_data()

  # 添加没有用药记录的受试者
  test_data$data$SV <- rbind(
    test_data$data$SV,
    data.frame(
      SUBJID = "999",
      VISIT = "筛选访视",
      VISITNUM = 0,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 该受试者的first_dose_date应为NA
  subj_999 <- result[result$SUBJID == "999", ]
  expect_true(all(is.na(subj_999$first_dose_date)))

  # 计划日期也应为NA
  expect_true(all(is.na(subj_999$planned_date)))
})

test_that("边界情况：自定义周期天数", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule,
    cycle_days = 21 # 21天周期
  )

  # C2D1应等于C1D1 + 21天
  subj_001 <- result[result$SUBJID == "001", ]
  c1d1_date <- subj_001$planned_date[subj_001$VISIT == "C1D1"][1]
  c2d1_date <- subj_001$planned_date[subj_001$VISIT == "C2D1"][1]

  expect_equal(as.numeric(c2d1_date - c1d1_date), 21)
})

test_that("边界情况：自定义访视数据集列名", {
  test_data <- setup_test_data()

  # 修改列名
  names(test_data$data$SV)[names(test_data$data$SV) == "VISIT"] <- "VISITNAME"
  names(test_data$data$SV)[names(test_data$data$SV) == "VISITNUM"] <- "VISITCODE"
  names(test_data$data$SV)[names(test_data$data$SV) == "SVDAT"] <- "VISITDATE"

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule,
    sv_visit_var = "VISITNAME",
    sv_visitnum_var = "VISITCODE",
    sv_date_var = "VISITDATE"
  )

  # 应正常返回结果
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

test_that("边界情况：缺少EOT数据集", {
  test_data <- setup_test_data()

  # 删除EOT数据集
  test_data$data$EOT <- NULL

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # EOT相关访视的计划日期应为NA
  eot_visits <- result[result$VISIT == "治疗结束访视", ]
  expect_true(all(is.na(eot_visits$planned_date)))
  expect_true(all(is.na(eot_visits$eot_date)))
})

test_that("边界情况：日期列包含SAS缺失值", {
  test_data <- setup_test_data()

  # 添加SAS缺失值
  test_data$data$EX <- rbind(
    test_data$data$EX,
    data.frame(
      SUBJID = "004",
      EXSTDAT = "", # SAS缺失值
      stringsAsFactors = FALSE
    )
  )

  test_data$data$SV <- rbind(
    test_data$data$SV,
    data.frame(
      SUBJID = "004",
      VISIT = "筛选访视",
      VISITNUM = 0,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  # 应该正常运行，不报错
  expect_no_error({
    result <- generate_planned_visit_dates(
      data = test_data$data,
      visit_schedule_data = test_data$visit_schedule
    )
  })

  # 受试者004的first_dose_date应为NA
  subj_004 <- result[result$SUBJID == "004", ]
  expect_true(all(is.na(subj_004$first_dose_date)))
})

# ===== 返回结果完整性测试 =====

test_that("返回结果包含所有访视", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 每个受试者应该有完整的访视记录
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(nrow(subj_001), nrow(test_data$visit_schedule))

  # 检查访视名称是否完整
  expect_setequal(subj_001$VISIT, test_data$visit_schedule$VISIT)
})

test_that("返回结果按受试者和日期排序", {
  test_data <- setup_test_data()

  result <- generate_planned_visit_dates(
    data = test_data$data,
    visit_schedule_data = test_data$visit_schedule
  )

  # 检查是否按SUBJID排序
  expect_true(all(result$SUBJID == sort(result$SUBJID)))

  # 检查每个受试者内部是否按actual_date排序（非NA值）
  subj_001 <- result[result$SUBJID == "001", ]
  subj_001_with_date <- subj_001[!is.na(subj_001$actual_date), ]

  if (nrow(subj_001_with_date) > 1) {
    expect_true(all(diff(subj_001_with_date$actual_date) >= 0))
  }
})


# =============================================================================
# 补充测试：D1 计划日期迭代计算
# =============================================================================

test_that("D1计划日期：C2D1基于C1D1实际日期计算", {
  # 当C1D1有实际访视记录且与首次给药日期不同时
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "C2D1"),
    VISITNUM = c(1, 2),
    CYCLE = c("治疗周期1", "治疗周期2"),
    VISITDAY = c("1", "1"),
    WP = c("±3d", "±3d"),
    type = c("±", "±"),
    wpvalue = c("3", "3"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = c("001", "001"),
      VISIT = c("C1D1", "C2D1"),
      VISITNUM = c(1, 2),
      SVDAT = c("2024-01-03", "2024-02-02"), # C1D1实际延迟2天
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule,
    cycle_days = 28
  )

  # C1D1计划日期应为实际日期（有实际访视）
  c1d1 <- result[result$VISIT == "C1D1", ]
  expect_equal(as.character(c1d1$planned_date), "2024-01-03")

  # C2D1计划日期应基于C1D1实际日期 + 28天
  c2d1 <- result[result$VISIT == "C2D1", ]
  expect_equal(as.character(c2d1$planned_date), "2024-01-31")
})


# =============================================================================
# 补充测试：非D1访视基于D1实际日期计算
# =============================================================================

test_that("非D1访视：基于当前周期D1实际日期计算", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "C1D8", "C1D15"),
    VISITNUM = c(1, 2, 3),
    CYCLE = c("治疗周期1", "治疗周期1", "治疗周期1"),
    VISITDAY = c("1", "8", "15"),
    WP = c("±3d", "±2d", "±2d"),
    type = c("±", "±", "±"),
    wpvalue = c("3", "2", "2"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = c("001"),
      VISIT = c("C1D1"),
      VISITNUM = c(1),
      SVDAT = c("2024-01-05"), # C1D1实际延迟4天
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  # C1D8应基于C1D1实际日期(2024-01-05) + 7天
  c1d8 <- result[result$VISIT == "C1D8", ]
  expect_equal(as.character(c1d8$planned_date), "2024-01-12")

  # C1D15应基于C1D1实际日期(2024-01-05) + 14天
  c1d15 <- result[result$VISIT == "C1D15", ]
  expect_equal(as.character(c1d15$planned_date), "2024-01-19")
})


# =============================================================================
# 补充测试：随访访视计划日期
# =============================================================================

test_that("随访访视：基于EOT日期计算", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "随访1", "随访2"),
    VISITNUM = c(1, 100, 101),
    CYCLE = c("治疗周期1", "随访", "随访"),
    VISITDAY = c("1", "EOT+30", "EOT+60"),
    WP = c("±3d", "±7d", "±7d"),
    type = c("±", "±", "±"),
    wpvalue = c("3", "7", "7"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    EOT = data.frame(
      SUBJID = "001",
      EOTDAT = "2024-03-15",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  # 随访1应为EOT + 30天
  fu1 <- result[result$VISIT == "随访1", ]
  expect_equal(as.character(fu1$planned_date), "2024-04-14")

  # 随访2应为EOT + 60天
  fu2 <- result[result$VISIT == "随访2", ]
  expect_equal(as.character(fu2$planned_date), "2024-05-14")
})

test_that("随访访视：基于末次给药日期计算（LD+N格式）", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "安全性随访"),
    VISITNUM = c(1, 100),
    CYCLE = c("治疗周期1", "随访"),
    VISITDAY = c("1", "LD+30"),
    WP = c("±3d", "±7d"),
    type = c("±", "±"),
    wpvalue = c("3", "7"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = c("001", "001"),
      EXSTDAT = c("2024-01-01", "2024-02-01"),
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  # 安全性随访应为末次给药日期(2024-02-01) + 30天
  fu <- result[result$VISIT == "安全性随访", ]
  expect_equal(as.character(fu$planned_date), "2024-03-02")
})


# =============================================================================
# 补充测试：EOT访视计划日期
# =============================================================================

test_that("EOT访视：使用EOT日期", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "治疗结束"),
    VISITNUM = c(1, 99),
    CYCLE = c("治疗周期1", "治疗结束"),
    VISITDAY = c("1", "EOT"),
    WP = c("±3d", "+7d"),
    type = c("±", "+"),
    wpvalue = c("3", "7"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    EOT = data.frame(
      SUBJID = "001",
      EOTDAT = "2024-03-15",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  # 治疗结束访视计划日期应为EOT日期
  eot <- result[result$VISIT == "治疗结束", ]
  expect_equal(as.character(eot$planned_date), "2024-03-15")
})

test_that("EOT访视：使用EOS日期", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "研究结束"),
    VISITNUM = c(1, 99),
    CYCLE = c("治疗周期1", "治疗结束"),
    VISITDAY = c("1", "EOS"),
    WP = c("±3d", "+7d"),
    type = c("±", "+"),
    wpvalue = c("3", "7"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    DS = data.frame(
      SUBJID = "001",
      DSDAT = "2024-09-01",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  # 研究结束访视计划日期应为EOS日期
  eos <- result[result$VISIT == "研究结束", ]
  expect_equal(as.character(eos$planned_date), "2024-09-01")
})


# =============================================================================
# 补充测试：窗口类型变体
# =============================================================================

test_that("窗口期计算：- 类型（负向窗口）", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1"),
    VISITNUM = c(1),
    CYCLE = c("治疗周期1"),
    VISITDAY = c("1"),
    WP = c("-3d"),
    type = c("-"),
    wpvalue = c("3"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-15",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-15",
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule
  )

  c1d1 <- result[result$VISIT == "C1D1", ]
  # -3d 表示 [planned - 3, planned]
  expect_equal(c1d1$wp_start[1], as.Date("2024-01-12"))
  expect_equal(c1d1$wp_end[1], as.Date("2024-01-15"))
})


# =============================================================================
# 补充测试：处理非数字visitday不产生警告
# =============================================================================

test_that("处理非数字visitday不产生警告", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "治疗结束", "随访"),
    VISITNUM = c(1, 99, 100),
    CYCLE = c("治疗周期1", "治疗结束", "随访"),
    VISITDAY = c("1", "EOT", "EOT+30"), # EOT和EOT+30是非数字
    WP = c("±3d", "+7d", "±7d"),
    type = c("±", "+", "±"),
    wpvalue = c("3", "7", "7"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = "001",
      VISIT = "C1D1",
      VISITNUM = 1,
      SVDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    EOT = data.frame(
      SUBJID = "001",
      EOTDAT = "2024-03-15",
      stringsAsFactors = FALSE
    )
  )

  # 不应产生警告
  expect_no_warning({
    result <- generate_planned_visit_dates(
      data = data_list,
      visit_schedule_data = visit_schedule
    )
  })

  expect_s3_class(result, "data.frame")
})


# =============================================================================
# 补充测试：多周期场景
# =============================================================================

test_that("多周期场景：C3D1基于C2D1实际日期计算", {
  visit_schedule <- data.frame(
    VISIT = c("C1D1", "C2D1", "C3D1"),
    VISITNUM = c(1, 2, 3),
    CYCLE = c("治疗周期1", "治疗周期2", "治疗周期3"),
    VISITDAY = c("1", "1", "1"),
    WP = c("±3d", "±3d", "±3d"),
    type = c("±", "±", "±"),
    wpvalue = c("3", "3", "3"),
    stringsAsFactors = FALSE
  )

  data_list <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    ),
    SV = data.frame(
      SUBJID = c("001", "001"),
      VISIT = c("C1D1", "C2D1"),
      VISITNUM = c(1, 2),
      SVDAT = c("2024-01-01", "2024-02-05"), # C2D1延迟7天
      stringsAsFactors = FALSE
    )
  )

  result <- generate_planned_visit_dates(
    data = data_list,
    visit_schedule_data = visit_schedule,
    cycle_days = 28
  )

  # C1D1：实际日期 = 2024-01-01
  c1d1 <- result[result$VISIT == "C1D1", ]
  expect_equal(as.character(c1d1$planned_date), "2024-01-01")

  # C2D1：基于C1D1 + 28天 = 2024-01-29，但有实际访视延迟到2024-02-05
  c2d1 <- result[result$VISIT == "C2D1", ]
  expect_equal(as.character(c2d1$planned_date), "2024-01-29")

  # C3D1：基于C2D1实际日期(2024-02-05) + 28天 = 2024-03-04
  c3d1 <- result[result$VISIT == "C3D1", ]
  expect_equal(as.character(c3d1$planned_date), "2024-03-04")
})
