# Test date extraction functions in get_date.R

# =============================================================================
# 测试辅助函数
# =============================================================================

setup_dose_data <- function() {
  list(
    EX = data.frame(
      SUBJID = c("001", "001", "002", "003"),
      EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05", "2024-01-10"),
      EXENDAT = c("2024-01-28", "2024-02-25", "2024-02-01", "2024-02-07"),
      stringsAsFactors = FALSE
    )
  )
}

# =============================================================================
# 测试 get_first_dose_date() 函数
# =============================================================================

test_that("get_first_dose_date() 返回正确的首次给药日期", {
  data <- setup_dose_data()

  result <- get_first_dose_date(data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("SUBJID", "first_dose_date") %in% names(result)))


  # 受试者001有两条记录，应返回最早的日期
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$first_dose_date), "2024-01-01")

  # 受试者002只有一条记录
  subj_002 <- result[result$SUBJID == "002", ]
  expect_equal(as.character(subj_002$first_dose_date), "2024-01-05")
})

test_that("get_first_dose_date() 支持多个数据集", {
  data <- list(
    EX1 = data.frame(
      SUBJID = c("001", "001"),
      EXSTDAT = c("2024-01-15", "2024-02-01"),
      stringsAsFactors = FALSE
    ),
    EX2 = data.frame(
      SUBJID = c("001", "002"),
      EXSTDAT = c("2024-01-10", "2024-01-20"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(
    data,
    ex_datasets = c("EX1", "EX2"),
    ex_date_var = "EXSTDAT"
  )

  # 受试者001应返回跨数据集的最早日期
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$first_dose_date), "2024-01-10")

  # 受试者002只在EX2中
  subj_002 <- result[result$SUBJID == "002", ]
  expect_equal(as.character(subj_002$first_dose_date), "2024-01-20")
})

test_that("get_first_dose_date() 支持不同数据集使用不同的日期列", {
  data <- list(
    EX1 = data.frame(
      SUBJID = "001",
      STDAT1 = "2024-01-15",
      stringsAsFactors = FALSE
    ),
    EX2 = data.frame(
      SUBJID = "001",
      STDAT2 = "2024-01-10",
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(
    data,
    ex_datasets = c("EX1", "EX2"),
    ex_date_var = c("STDAT1", "STDAT2")
  )

  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$first_dose_date), "2024-01-10")
})

test_that("get_first_dose_date() 正确处理SAS缺失值", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "001", "002"),
      EXSTDAT = c("2024-01-01", ".", ""),
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  # 受试者001应忽略缺失值
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$first_dose_date), "2024-01-01")

  # 受试者002没有有效日期，不应出现在结果中
  expect_false("002" %in% result$SUBJID)
})

test_that("get_first_dose_date() 缺少数据集时静默跳过", {
  data <- list(
    EX1 = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-15",
      stringsAsFactors = FALSE
    )
  )

  # EX2 不存在，应该静默跳过
  result <- get_first_dose_date(
    data,
    ex_datasets = c("EX1", "EX2"),
    ex_date_var = "EXSTDAT"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("get_first_dose_date() 返回空数据框当无有效数据时", {
  data <- list(
    EX = data.frame(
      SUBJID = character(),
      EXSTDAT = character(),
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("SUBJID", "first_dose_date", "first_dose_time") %in% names(result)))
})

test_that("get_first_dose_date() 支持提取给药时间", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "001", "002"),
      EXSTDAT = c("2024-01-01", "2024-01-01", "2024-01-05"),
      EXSTTIM = c("10:00", "08:30", "14:00"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data, ex_time_var = "EXSTTIM")

  # 同一首剂日期取较早时间
  expect_equal(result$first_dose_time[result$SUBJID == "001"], "08:30")
  expect_equal(result$first_dose_time[result$SUBJID == "002"], "14:00")
})

test_that("get_first_dose_date() 未指定时间变量时时间为 NA", {
  data <- list(
    EX = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  expect_true("first_dose_time" %in% names(result))
  expect_true(is.na(result$first_dose_time[1]))
})

# =============================================================================
# 测试 get_first_dose_date() 参数验证
# =============================================================================

test_that("get_first_dose_date() 参数验证：data 必须是列表", {
  expect_error(
    get_first_dose_date(c(1, 2, 3)),
    "'data' must be a list"
  )

  expect_error(
    get_first_dose_date("not a list"),
    "'data' must be a list"
  )

  expect_error(
    get_first_dose_date(NULL),
    "'data' must be a list"
  )
})

test_that("get_first_dose_date() 参数验证：ex_datasets 必须是非空字符向量", {
  data <- setup_dose_data()

  expect_error(
    get_first_dose_date(data, ex_datasets = NULL),
    "'ex_datasets' must be a non-empty character vector"
  )

  expect_error(
    get_first_dose_date(data, ex_datasets = character()),
    "'ex_datasets' must be a non-empty character vector"
  )

  expect_error(
    get_first_dose_date(data, ex_datasets = 123),
    "'ex_datasets' must be a non-empty character vector"
  )
})

test_that("get_first_dose_date() 参数验证：ex_date_var 长度必须匹配", {
  data <- setup_dose_data()

  expect_error(
    get_first_dose_date(
      data,
      ex_datasets = c("EX1", "EX2"),
      ex_date_var = c("A", "B", "C")
    ),
    "ex_date_var length must be 1 or equal to ex_datasets length"
  )
})

# =============================================================================
# 测试 get_last_dose_date() 函数
# =============================================================================

test_that("get_last_dose_date() 默认使用开始日期作为末次给药日期", {
  data <- setup_dose_data()

  result <- get_last_dose_date(data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("SUBJID", "last_dose_date") %in% names(result)))

  # 受试者001应返回最晚的开始日期
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$last_dose_date), "2024-01-29")
})

test_that("get_last_dose_date() 使用结束日期当指定时", {
  data <- setup_dose_data()

  result <- get_last_dose_date(
    data,
    ex_end_date_var = "EXENDAT"
  )

  # 受试者001应返回最晚的结束日期
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$last_dose_date), "2024-02-25")
})

test_that("get_last_dose_date() 支持多个数据集", {
  data <- list(
    EX1 = data.frame(
      SUBJID = c("001", "001"),
      EXSTDAT = c("2024-01-01", "2024-02-01"),
      stringsAsFactors = FALSE
    ),
    EX2 = data.frame(
      SUBJID = c("001", "002"),
      EXSTDAT = c("2024-03-01", "2024-01-15"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_last_dose_date(
    data,
    ex_datasets = c("EX1", "EX2"),
    ex_date_var = "EXSTDAT"
  )

  # 受试者001应返回跨数据集的最晚日期
  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$last_dose_date), "2024-03-01")
})

test_that("get_last_dose_date() 多数据集使用不同的结束日期列", {
  data <- list(
    EX1 = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-01-01",
      ENDAT1 = "2024-01-28",
      stringsAsFactors = FALSE
    ),
    EX2 = data.frame(
      SUBJID = "001",
      EXSTDAT = "2024-02-01",
      ENDAT2 = "2024-03-15",
      stringsAsFactors = FALSE
    )
  )

  result <- get_last_dose_date(
    data,
    ex_datasets = c("EX1", "EX2"),
    ex_date_var = c("EXSTDAT", "EXSTDAT"),
    ex_end_date_var = c("ENDAT1", "ENDAT2")
  )

  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$last_dose_date), "2024-03-15")
})

# =============================================================================
# 测试 get_eot_date() 函数
# =============================================================================

test_that("get_eot_date() 返回正确的治疗结束日期", {
  data <- list(
    EOT = data.frame(
      SUBJID = c("001", "002"),
      EOTDAT = c("2024-06-01", "2024-06-15"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("SUBJID", "eot_date") %in% names(result)))

  expect_equal(as.character(result$eot_date[result$SUBJID == "001"]), "2024-06-01")
  expect_equal(as.character(result$eot_date[result$SUBJID == "002"]), "2024-06-15")
})

test_that("get_eot_date() 支持自定义数据集和列名", {
  data <- list(
    END_TREATMENT = data.frame(
      SUBJID = "001",
      END_DATE = "2024-06-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(
    data,
    eot_dataset = "END_TREATMENT",
    eot_date_var = "END_DATE"
  )

  expect_equal(as.character(result$eot_date[1]), "2024-06-01")
})

test_that("get_eot_date() 数据集不存在时返回空数据框", {
  data <- list(
    EX = data.frame(SUBJID = "001", stringsAsFactors = FALSE)
  )

  result <- get_eot_date(data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_eot_date() 处理同一受试者多条记录取最大日期", {
  data <- list(
    EOT = data.frame(
      SUBJID = c("001", "001"),
      EOTDAT = c("2024-06-01", "2024-07-15"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(data)

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$eot_date[1]), "2024-07-15")
})

test_that("get_eot_date() 支持多个数据集取最大日期", {
  data <- list(
    EOT1 = data.frame(
      SUBJID = c("001", "002"),
      EOTDAT = c("2024-06-01", "2024-06-10"),
      stringsAsFactors = FALSE
    ),
    EOT2 = data.frame(
      SUBJID = c("001", "002"),
      EOTDAT = c("2024-07-15", "2024-06-05"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(
    data,
    eot_dataset = c("EOT1", "EOT2"),
    eot_date_var = "EOTDAT"
  )

  subj_001 <- result[result$SUBJID == "001", ]
  expect_equal(as.character(subj_001$eot_date), "2024-07-15")

  subj_002 <- result[result$SUBJID == "002", ]
  expect_equal(as.character(subj_002$eot_date), "2024-06-10")
})

test_that("get_eot_date() 支持不同数据集使用不同的日期列", {
  data <- list(
    EOT1 = data.frame(
      SUBJID = "001",
      EOTDAT1 = "2024-06-01",
      stringsAsFactors = FALSE
    ),
    EOT2 = data.frame(
      SUBJID = "001",
      EOTDAT2 = "2024-08-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(
    data,
    eot_dataset = c("EOT1", "EOT2"),
    eot_date_var = c("EOTDAT1", "EOTDAT2")
  )

  expect_equal(as.character(result$eot_date[1]), "2024-08-01")
})

test_that("get_eot_date() 正确处理SAS缺失值", {
  data <- list(
    EOT = data.frame(
      SUBJID = c("001", "002", "003"),
      EOTDAT = c("2024-06-01", ".", ""),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eot_date(data)

  # 只有受试者001有有效日期
  expect_equal(nrow(result), 1)
  expect_equal(result$SUBJID[1], "001")
})

# =============================================================================
# 测试 get_eot_date() 参数验证
# =============================================================================

test_that("get_eot_date() 参数验证：eot_dataset 必须是非空字符向量", {
  data <- list()

  expect_error(
    get_eot_date(data, eot_dataset = character()),
    "'eot_dataset' must be a non-empty character vector"
  )

  expect_error(
    get_eot_date(data, eot_dataset = 123),
    "'eot_dataset' must be a non-empty character vector"
  )
})

test_that("get_eot_date() 参数验证：eot_date_var 长度必须与数据集匹配", {
  data <- list()

  expect_error(
    get_eot_date(data, eot_dataset = c("EOT1", "EOT2"), eot_date_var = c("A", "B", "C")),
    "eot_date_var length must be 1 or equal to eot_dataset length"
  )
})

# =============================================================================
# 测试 get_eos_date() 函数
# =============================================================================

test_that("get_eos_date() 返回正确的研究结束日期", {
  data <- list(
    DS = data.frame(
      SUBJID = c("001", "002"),
      DSDAT = c("2024-09-01", "2024-09-15"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eos_date(data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("SUBJID", "eos_date") %in% names(result)))

  expect_equal(as.character(result$eos_date[result$SUBJID == "001"]), "2024-09-01")
  expect_equal(as.character(result$eos_date[result$SUBJID == "002"]), "2024-09-15")
})

test_that("get_eos_date() 支持自定义数据集和列名", {
  data <- list(
    DISPOSITION = data.frame(
      SUBJID = "001",
      STUDY_END_DATE = "2024-09-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_eos_date(
    data,
    ds_dataset = "DISPOSITION",
    ds_date_var = "STUDY_END_DATE"
  )

  expect_equal(as.character(result$eos_date[1]), "2024-09-01")
})

test_that("get_eos_date() 数据集不存在时返回空数据框", {
  data <- list(
    EX = data.frame(SUBJID = "001", stringsAsFactors = FALSE)
  )

  result <- get_eos_date(data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("get_eos_date() 正确处理SAS缺失值", {
  data <- list(
    DS = data.frame(
      SUBJID = c("001", "002", "003"),
      DSDAT = c("2024-09-01", "NA", "."),
      stringsAsFactors = FALSE
    )
  )

  result <- get_eos_date(data)

  # 只有受试者001有有效日期
  expect_equal(nrow(result), 1)
  expect_equal(result$SUBJID[1], "001")
})

# =============================================================================
# 测试 get_eos_date() 参数验证
# =============================================================================

test_that("get_eos_date() 参数验证：ds_dataset 必须是单个字符串", {
  data <- list()

  expect_error(
    get_eos_date(data, ds_dataset = c("A", "B")),
    "'ds_dataset' must be a single character string"
  )
})

test_that("get_eos_date() 参数验证：ds_date_var 必须是单个字符串", {
  data <- list()

  expect_error(
    get_eos_date(data, ds_date_var = c("A", "B")),
    "'ds_date_var' must be a single character string"
  )
})

# =============================================================================
# 测试边界情况
# =============================================================================

test_that("日期函数处理NA值", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "002"),
      EXSTDAT = c("2024-01-01", NA),
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  # 受试者001有有效日期
  expect_true("001" %in% result$SUBJID)
  # 受试者002没有有效日期
  expect_false("002" %in% result$SUBJID)
})

test_that("日期函数处理无SUBJID列的情况", {
  data <- list(
    EX = data.frame(
      PATIENT_ID = "001",
      EXSTDAT = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  # 应返回空数据框
  expect_equal(nrow(result), 0)
})

test_that("日期函数处理日期列不存在的情况", {
  data <- list(
    EX = data.frame(
      SUBJID = "001",
      OTHER_DATE = "2024-01-01",
      stringsAsFactors = FALSE
    )
  )

  result <- get_first_dose_date(data)

  # 应返回空数据框
  expect_equal(nrow(result), 0)
})

# =============================================================================
# 测试 get_rand_date() 函数
# =============================================================================

test_that("get_rand_date 按受试者取最早随机日期", {
  data <- list(
    RAND = data.frame(
      SUBJID = c("001", "001", "002"),
      RANDDT = c("2024-01-20", "2024-01-15", "2024-01-18"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_rand_date(data)

  expect_equal(nrow(result), 2)
  expect_true(all(c("SUBJID", "rd_date") %in% names(result)))
  expect_equal(result$rd_date[result$SUBJID == "001"], as.Date("2024-01-15"))
  expect_equal(result$rd_date[result$SUBJID == "002"], as.Date("2024-01-18"))
})

test_that("get_rand_date 支持多个数据集合并", {
  data <- list(
    RAND1 = data.frame(
      SUBJID = "001",
      RANDDT = "2024-01-20",
      stringsAsFactors = FALSE
    ),
    RAND2 = data.frame(
      SUBJID = "001",
      RANDDT = "2024-01-10",
      stringsAsFactors = FALSE
    )
  )

  result <- get_rand_date(data, rd_datasets = c("RAND1", "RAND2"))

  expect_equal(nrow(result), 1)
  expect_equal(result$rd_date, as.Date("2024-01-10"))
})

test_that("get_rand_date 支持自定义变量名", {
  data <- list(
    RAND = data.frame(
      SUBJID = "001",
      RANDDAT = "2024-01-15",
      stringsAsFactors = FALSE
    )
  )

  result <- get_rand_date(data, rd_date_var = "RANDDAT")

  expect_equal(nrow(result), 1)
  expect_equal(result$rd_date, as.Date("2024-01-15"))
})

test_that("get_rand_date 排除SAS缺失值并处理缺失数据集", {
  data <- list(
    RAND = data.frame(
      SUBJID = c("001", "002"),
      RANDDT = c("2024-01-15", "."),
      stringsAsFactors = FALSE
    )
  )

  result <- get_rand_date(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$SUBJID, "001")

  result_empty <- get_rand_date(list(LB = data.frame(SUBJID = "001")))
  expect_equal(nrow(result_empty), 0)
  expect_true(all(c("SUBJID", "rd_date") %in% names(result_empty)))
})

test_that("get_rand_date 校验输入参数", {
  expect_error(get_rand_date("not a list"), "'data' must be a list")
  expect_error(
    get_rand_date(list(), rd_datasets = character(0)),
    "'rd_datasets' must be a non-empty character vector"
  )
})

# =============================================================================
# 测试 get_cyc_dose_date() 函数
# =============================================================================

test_that("get_cyc_dose_date 按受试者和访视取最早给药日期", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "001", "001", "002"),
      VISITNUM = c(2, 2, 3, 2),
      EXSTDAT = c("2024-01-02", "2024-01-01", "2024-01-29", "2024-01-05"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_cyc_dose_date(data)

  expect_equal(nrow(result), 3)
  expect_true(all(c("SUBJID", "VISITNUM", "cyc_dose_date", "cyc_dose_time") %in% names(result)))
  expect_type(result$VISITNUM, "character")

  # 同一访视多条给药记录取最早
  v2_001 <- result[result$SUBJID == "001" & result$VISITNUM == "2", ]
  expect_equal(v2_001$cyc_dose_date, as.Date("2024-01-01"))
  expect_true(is.na(v2_001$cyc_dose_time))

  v3_001 <- result[result$SUBJID == "001" & result$VISITNUM == "3", ]
  expect_equal(v3_001$cyc_dose_date, as.Date("2024-01-29"))
})

test_that("get_cyc_dose_date 支持提取给药时间", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "001", "001"),
      VISITNUM = c(2, 2, 3),
      EXSTDAT = c("2024-01-01", "2024-01-01", "2024-01-29"),
      EXSTTIM = c("10:00", "08:00", "09:30"),
      stringsAsFactors = FALSE
    )
  )

  result <- get_cyc_dose_date(data, ex_time_var = "EXSTTIM")

  v2 <- result[result$SUBJID == "001" & result$VISITNUM == "2", ]
  expect_equal(v2$cyc_dose_date, as.Date("2024-01-01"))
  expect_equal(v2$cyc_dose_time, "08:00")

  v3 <- result[result$SUBJID == "001" & result$VISITNUM == "3", ]
  expect_equal(v3$cyc_dose_time, "09:30")
})

test_that("get_cyc_dose_date 支持多个数据集合并", {
  data <- list(
    EX1 = data.frame(
      SUBJID = "001",
      VISITNUM = 2,
      EXSTDAT = "2024-01-10",
      stringsAsFactors = FALSE
    ),
    EX2 = data.frame(
      SUBJID = "001",
      VISITNUM = 2,
      EXSTDAT = "2024-01-05",
      stringsAsFactors = FALSE
    )
  )

  result <- get_cyc_dose_date(data, ex_datasets = c("EX1", "EX2"))

  expect_equal(nrow(result), 1)
  expect_equal(result$cyc_dose_date, as.Date("2024-01-05"))
})

test_that("get_cyc_dose_date 支持自定义变量名", {
  data <- list(
    EX = data.frame(
      SUBJID = "001",
      VISNO = 2,
      DOSEDAT = "2024-01-10",
      stringsAsFactors = FALSE
    )
  )

  result <- get_cyc_dose_date(
    data,
    ex_date_var = "DOSEDAT",
    visitnum_var = "VISNO"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$VISITNUM, "2")
})

test_that("get_cyc_dose_date 排除SAS缺失值", {
  data <- list(
    EX = data.frame(
      SUBJID = c("001", "002", "003"),
      VISITNUM = c(2, 2, 2),
      EXSTDAT = c("2024-01-01", ".", ""),
      stringsAsFactors = FALSE
    )
  )

  result <- get_cyc_dose_date(data)

  expect_equal(nrow(result), 1)
  expect_equal(result$SUBJID, "001")
})

test_that("get_cyc_dose_date 处理缺失数据集或缺失列", {
  # 数据集不存在
  result1 <- get_cyc_dose_date(list(LB = data.frame(SUBJID = "001")))
  expect_equal(nrow(result1), 0)
  expect_true(all(c("SUBJID", "VISITNUM", "cyc_dose_date", "cyc_dose_time") %in% names(result1)))

  # 缺少 VISITNUM 列
  result2 <- get_cyc_dose_date(
    list(EX = data.frame(SUBJID = "001", EXSTDAT = "2024-01-01", stringsAsFactors = FALSE))
  )
  expect_equal(nrow(result2), 0)
})

test_that("get_cyc_dose_date 校验输入参数", {
  expect_error(get_cyc_dose_date("not a list"), "'data' must be a list")
  expect_error(
    get_cyc_dose_date(list(), ex_datasets = character(0)),
    "'ex_datasets' must be a non-empty character vector"
  )
  expect_error(
    get_cyc_dose_date(list(), visitnum_var = c("A", "B")),
    "'visitnum_var' must be a single character string"
  )
})
