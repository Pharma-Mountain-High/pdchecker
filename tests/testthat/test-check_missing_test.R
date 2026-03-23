# 测试 check_missing_test() 函数
# =============================================================================

# 创建测试数据
# =============================================================================

create_prepared_test_data <- function() {
  # 模拟 prepare_test_data 返回的标准化数据（骨架 + left join）
  # TESTCAT 来自骨架（config），永远有值
  # TESTCAT_ORIG 来自原始检查数据，NA 表示骨架行（无匹配的检查记录）
  data.frame(
    SUBJID = rep(c("001", "002", "003"), each = 3),
    VISIT = rep(c("V1", "V2", "V3"), times = 3),
    VISITNUM = rep(c("1", "2", "3"), times = 3),
    SVDAT = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-01", # 001
      "2024-01-02", "2024-01-16", "2024-02-02", # 002
      "2024-01-03", "2024-01-17", "2024-02-03" # 003
    )),
    TBNAME = "LB",
    TESTCAT = c(
      "血常规", "血常规", "血常规", # 001: 都有检查分类
      "血常规", "血常规", "血常规", # 002: 骨架行 TESTCAT 也有值
      "血常规", "血生化", "血常规" # 003: 都有检查分类
    ),
    TESTCAT_ORIG = c(
      "血常规", "血常规", "血常规", # 001: 都有原始数据
      "血常规", NA, "血常规", # 002: V2 无匹配的检查记录（情况1/2）
      "血常规", "血生化", "血常规" # 003: 都有原始数据
    ),
    TESTDE = c(
      "白细胞", "白细胞", "白细胞", # 001
      "白细胞", NA, "白细胞", # 002
      "白细胞", "肝功能", "白细胞" # 003
    ),
    TESTYN = c(
      "是", "是", "是", # 001: 都做了检查
      "是", NA, "否", # 002: V3 有记录但 TESTYN="否"（情况2）
      "是", "是", "是" # 003: 都做了检查
    ),
    TESTDAT = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-01", # 001
      "2024-01-02", NA, "2024-02-02", # 002: V2 无检查日期
      "2024-01-03", "2024-01-17", "2024-02-03" # 003
    )),
    ORRES = c(
      "5.5", "6.2", "6.0", # 001: 都有结果
      "5.8", NA, "5.9", # 002: V2 无结果
      "6.1", NA, "5.7" # 003: V2 结果为空（情况3）
    ),
    stringsAsFactors = FALSE
  )
}


create_no_missing_data <- function() {
  # 创建没有缺失的完整数据
  data.frame(
    SUBJID = rep(c("001", "002"), each = 2),
    VISIT = rep(c("V1", "V2"), times = 2),
    VISITNUM = rep(c("1", "2"), times = 2),
    SVDAT = as.Date(c(
      "2024-01-01", "2024-01-15",
      "2024-01-02", "2024-01-16"
    )),
    TBNAME = "LB",
    TESTCAT = rep("血常规", 4),
    TESTCAT_ORIG = rep("血常规", 4),
    TESTDE = rep("白细胞", 4),
    TESTYN = rep("是", 4),
    TESTDAT = as.Date(c(
      "2024-01-01", "2024-01-15",
      "2024-01-02", "2024-01-16"
    )),
    ORRES = c("5.5", "6.2", "5.8", "6.0"),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# 测试基本功能
# =============================================================================

test_that("check_missing_test() 基本功能正常", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  # 检查返回的是列表

  expect_true(is.list(result))

  # 检查返回的组件
  expect_true("has_deviation" %in% names(result))
  expect_true("messages" %in% names(result))
  expect_true("details" %in% names(result))

  # 检查类型
  expect_true(is.logical(result$has_deviation))
  expect_true(is.character(result$messages))
  expect_true(is.data.frame(result$details))
})


test_that("check_missing_test() 检测到缺失时返回正确结果", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  # 应该检测到缺失
  expect_true(result$has_deviation)

  # 应该有消息
  expect_gt(length(result$messages), 0)
  expect_gt(nchar(result$messages[1]), 0)

  # 应该有详细信息
  expect_gt(nrow(result$details), 0)

  # 检查 details 的列
  expected_cols <- c(
    "PDNO", "SUBJID", "VISIT", "VISITNUM", "visit_date",
    "TBNAME", "test_name", "missing_type", "DESCRIPTION"
  )
  expect_true(all(expected_cols %in% names(result$details)))
})


test_that("check_missing_test() 没有缺失时返回正确结果", {
  test_data <- create_no_missing_data()

  result <- check_missing_test(data = test_data)

  # 不应该检测到缺失
  expect_false(result$has_deviation)

  # 消息应该为空
  expect_equal(length(result$messages), 0)

  # details 应该为空
  expect_equal(nrow(result$details), 0)
})


# =============================================================================
# 测试 pdno 参数
# =============================================================================

test_that("pdno 参数默认值为 '8.3.1'", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    expect_true(all(result$details$PDNO == "8.3.1"))
  }
})


test_that("pdno 参数可以自定义", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data, pdno = "8.3.2")

  if (nrow(result$details) > 0) {
    expect_true(all(result$details$PDNO == "8.3.2"))
  }
})


test_that("pdno 参数验证正确", {
  test_data <- create_prepared_test_data()

  # pdno 必须是单个字符串

  expect_error(
    check_missing_test(data = test_data, pdno = 123),
    "'pdno' must be a single character string"
  )

  expect_error(
    check_missing_test(data = test_data, pdno = c("8.3.1", "8.3.2")),
    "'pdno' must be a single character string"
  )

  expect_error(
    check_missing_test(data = test_data, pdno = NULL),
    "'pdno' must be a single character string"
  )
})


# =============================================================================
# 测试三种缺失情况
# =============================================================================

test_that("检测情况1：访视完全无检查记录（骨架行 TESTCAT_ORIG 全为 NA）", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  # 应该检测到访视无检查记录的情况
  testcat_empty <- result$details[result$details$missing_type == "TESTCAT_EMPTY", ]
  expect_gt(nrow(testcat_empty), 0)

  # 检查 002 的 V2（TESTCAT_ORIG 为 NA，骨架行无匹配数据）
  subj002_v2 <- testcat_empty[testcat_empty$SUBJID == "002" & testcat_empty$VISIT == "V2", ]
  expect_equal(nrow(subj002_v2), 1)

  # 检查 test_name 格式
  expect_true(grepl("全部", subj002_v2$test_name))

  # 检查 TBNAME 列
  expect_equal(subj002_v2$TBNAME, "LB")

  # 检查 DESCRIPTION 列
  expect_true(grepl("受试者编号002", subj002_v2$DESCRIPTION))
  expect_true(grepl("缺失", subj002_v2$DESCRIPTION))
})


test_that("检测情况2：有部分检查记录但特定 TESTCAT 缺失", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  # 应该检测到整个 TESTCAT 缺失的情况
  testcat_missing <- result$details[result$details$missing_type == "TESTCAT_MISSING", ]
  expect_gt(nrow(testcat_missing), 0)

  # 检查 002 的 V3（TESTYN = "否"）
  subj002_v3 <- testcat_missing[testcat_missing$SUBJID == "002" & testcat_missing$VISIT == "V3", ]
  expect_equal(nrow(subj002_v3), 1)

  # 检查 test_name 是 TESTCAT 的值
  expect_equal(subj002_v3$test_name, "血常规")

  # 检查 DESCRIPTION 列
  expect_true(grepl("受试者编号002", subj002_v3$DESCRIPTION))
  expect_true(grepl("血常规", subj002_v3$DESCRIPTION))
})


test_that("检测情况3：单个TESTDE指标缺失", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data, missing_de = TRUE)

  # 应该检测到 TESTDE 缺失的情况
  testde_missing <- result$details[result$details$missing_type == "TESTDE_MISSING", ]
  expect_gt(nrow(testde_missing), 0)

  # 检查 003 的 V2（ORRES = NA，但 TESTDAT 有值）
  subj003_v2 <- testde_missing[testde_missing$SUBJID == "003" & testde_missing$VISIT == "V2", ]
  expect_equal(nrow(subj003_v2), 1)

  # 检查 test_name 格式（TESTCAT-TESTDE）
  expect_true(grepl("-", subj003_v2$test_name))
  expect_true(grepl("血生化", subj003_v2$test_name))

  # 检查 DESCRIPTION 列
  expect_true(grepl("受试者编号003", subj003_v2$DESCRIPTION))
})


# =============================================================================
# 测试 missing_de 参数
# =============================================================================

test_that("missing_de = FALSE 时不检查单个指标缺失", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data, missing_de = FALSE)

  # 不应该有 TESTDE 缺失的记录
  testde_missing <- result$details[result$details$missing_type == "TESTDE_MISSING", ]
  expect_equal(nrow(testde_missing), 0)

  # 应该在消息中说明
  expect_true(grepl("仅检查整体检查项缺失", result$messages))
})


test_that("missing_de = TRUE 时检查单个指标缺失", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data, missing_de = TRUE)

  # 应该有 TESTDE 缺失的记录
  testde_missing <- result$details[result$details$missing_type == "TESTDE_MISSING", ]
  expect_gt(nrow(testde_missing), 0)
})


# =============================================================================
# 测试 test_var 和 test 参数
# =============================================================================

test_that("test_var 和 test 参数可以筛选特定检查项", {
  test_data <- create_prepared_test_data()

  # 只检查血常规
  result <- check_missing_test(
    data = test_data,
    test_var = "TESTCAT",
    test = "血常规"
  )

  # 结果中应该只有血常规相关的缺失
  if (nrow(result$details) > 0) {
    # 检查 TESTCAT 不为空的记录都是"血常规"
    non_empty_testcat <- result$details |>
      dplyr::filter(!is.na(missing_type), missing_type != "TESTCAT_EMPTY")
    if (nrow(non_empty_testcat) > 0) {
      # test_name 应该包含"血常规"或者是"全部LB"
      expect_true(all(grepl("血常规|全部", result$details$test_name)))
    }
  }
})


test_that("test_var 为 NULL 时检查所有检查项", {
  test_data <- create_prepared_test_data()

  result_all <- check_missing_test(
    data = test_data,
    test_var = NULL,
    test = NULL
  )

  result_filtered <- check_missing_test(
    data = test_data,
    test_var = "TESTCAT",
    test = "血常规"
  )

  # 检查所有检查项应该返回更多或相等的缺失记录
  expect_gte(nrow(result_all$details), nrow(result_filtered$details))
})


# =============================================================================
# 测试边界情况
# =============================================================================

test_that("没有实际访视记录时返回空结果", {
  # 创建 SVDAT 都为 NA 的数据
  test_data <- create_prepared_test_data()
  test_data$SVDAT <- NA

  result <- check_missing_test(data = test_data)

  # 不应该有缺失
  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})


test_that("空数据框返回空结果", {
  # 创建空的但结构正确的数据框
  test_data <- create_prepared_test_data()[0, ]

  result <- check_missing_test(data = test_data)

  # 不应该有缺失
  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})


test_that("所有检查都完整时返回空结果", {
  test_data <- create_no_missing_data()

  result <- check_missing_test(data = test_data)

  # 不应该有缺失
  expect_false(result$has_deviation)
  expect_equal(length(result$messages), 0)
  expect_equal(nrow(result$details), 0)
})


# =============================================================================
# 测试错误处理
# =============================================================================

test_that("data 不是 data frame 时报错", {
  expect_error(
    check_missing_test(data = list(a = 1, b = 2)),
    "'data' must be a data frame"
  )

  expect_error(
    check_missing_test(data = "not a dataframe"),
    "'data' must be a data frame"
  )
})


test_that("缺少必要列时报错", {
  test_data <- create_prepared_test_data()

  # 移除必要的列
  test_data_incomplete <- test_data[, setdiff(names(test_data), "TESTCAT")]

  expect_error(
    check_missing_test(data = test_data_incomplete),
    "'data' is missing required columns.*TESTCAT"
  )
})


test_that("缺少多个必要列时报错", {
  test_data <- create_prepared_test_data()

  # 移除多个必要的列
  test_data_incomplete <- test_data[, c("SUBJID", "VISIT")]

  expect_error(
    check_missing_test(data = test_data_incomplete),
    "'data' is missing required columns"
  )
})


test_that("提示使用 prepare_test_data 准备数据", {
  expect_error(
    check_missing_test(data = data.frame(x = 1:5)),
    "prepare_test_data"
  )
})


# =============================================================================
# 测试返回值结构
# =============================================================================

test_that("返回的 details 包含所有必要的列", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    expected_cols <- c(
      "PDNO", "SUBJID", "VISIT", "VISITNUM", "visit_date",
      "TBNAME", "test_name", "missing_type", "DESCRIPTION"
    )
    expect_true(all(expected_cols %in% names(result$details)))
  }
})


test_that("返回的 missing_type 只包含预期的值", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    valid_types <- c("TESTCAT_EMPTY", "TESTCAT_MISSING", "TESTDE_MISSING")
    expect_true(all(result$details$missing_type %in% valid_types))
  }
})


test_that("visit_date 是 Date 类型", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    expect_true(inherits(result$details$visit_date, "Date"))
  }
})


test_that("DESCRIPTION 列格式正确", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    # 检查 DESCRIPTION 包含必要信息
    expect_true(all(grepl("受试者", result$details$DESCRIPTION)))
    expect_true(all(grepl("访视", result$details$DESCRIPTION)))
    expect_true(all(grepl("缺失", result$details$DESCRIPTION)))
  }
})


# =============================================================================
# 测试消息生成
# =============================================================================

test_that("消息正确反映缺失数量", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data)

  if (result$has_deviation) {
    # 消息应该包含缺失数量
    expect_true(grepl("\\d+", result$messages))

    # 如果有不同类型的缺失，消息应该用分号分隔
    n_types <- length(unique(result$details$missing_type))
    if (n_types > 1) {
      expect_true(grepl("；", result$messages))
    }
  }
})


test_that("消息格式正确", {
  test_data <- create_prepared_test_data()

  result <- check_missing_test(data = test_data, missing_de = TRUE)

  if (result$has_deviation) {
    message <- result$messages[1]

    # 检查是否包含关键词
    keywords <- c("访视无检查记录|整体检查项缺失|具体指标缺失")
    expect_true(grepl(keywords, message))

    # 检查是否包含数字
    expect_true(grepl("\\d+", message))

    # 检查是否包含"条"
    expect_true(grepl("条", message))
  }
})


# =============================================================================
# 测试 print 方法
# =============================================================================

test_that("print.missing_test_check 方法正常工作", {
  test_data <- create_prepared_test_data()
  result <- check_missing_test(data = test_data)

  # 捕获输出
  output <- capture.output(print(result))

  # 应该有输出
  expect_gt(length(output), 0)

  # 应该包含 pdno 和标题
  expect_true(any(grepl("8.3.1 检查项缺失检查", output)))

  # 应该包含 has_deviation 状态
  expect_true(any(grepl("Has deviation", output)))
})


test_that("print 方法显示缺失详情", {
  test_data <- create_prepared_test_data()
  result <- check_missing_test(data = test_data)

  if (result$has_deviation) {
    output <- capture.output(print(result))

    # 应该包含 Findings
    expect_true(any(grepl("Findings", output)))

    # 应该包含详细信息
    expect_true(any(grepl("Deviation Details", output)))

    # 应该包含受试者信息（来自 DESCRIPTION）
    expect_true(any(grepl("受试者", output)))
  }
})


test_that("print 方法使用自定义 pdno", {
  test_data <- create_prepared_test_data()
  result <- check_missing_test(data = test_data, pdno = "8.3.5")

  output <- capture.output(print(result))

  # 应该显示自定义的 pdno
  expect_true(any(grepl("8.3.5", output)))
})


# =============================================================================
# 测试 SAS NA 值处理
# =============================================================================

test_that("正确处理 SAS 缺失值", {
  test_data <- create_prepared_test_data()

  # 添加 SAS 风格的缺失值
  test_data$ORRES[1] <- "." # SAS 缺失值
  test_data$TESTYN[2] <- "NA" # SAS 缺失值

  result <- check_missing_test(data = test_data, missing_de = TRUE)

  # 应该检测到这些缺失
  expect_true(result$has_deviation)
  expect_gt(nrow(result$details), 0)
})


# =============================================================================
# 测试数据一致性
# =============================================================================

test_that("同一受试者同一访视所有 TESTCAT 均无数据只输出一条记录", {
  # 创建骨架行数据：多个 TESTCAT 都没有匹配的检查记录
  test_data <- data.frame(
    SUBJID = rep("001", 6),
    VISIT = rep("V1", 6),
    VISITNUM = rep("1", 6),
    SVDAT = rep(as.Date("2024-01-01"), 6),
    TBNAME = "LB",
    TESTCAT = c("血常规", "血常规", "血常规", "血生化", "血生化", "血生化"),
    TESTCAT_ORIG = rep(NA, 6),
    TESTDE = rep(NA, 6),
    TESTYN = rep(NA, 6),
    TESTDAT = rep(NA, 6),
    ORRES = rep(NA, 6),
    stringsAsFactors = FALSE
  )

  result <- check_missing_test(data = test_data)

  # 全部无数据的情况应该只有一条 TESTCAT_EMPTY 记录
  testcat_empty <- result$details[result$details$missing_type == "TESTCAT_EMPTY", ]
  expect_equal(nrow(testcat_empty), 1)
})


test_that("同一受试者同一访视同一 TESTCAT 缺失只输出一条记录", {
  # 创建同一 TESTCAT 有多条记录的数据（有原始数据但 TESTYN="否"）
  test_data <- data.frame(
    SUBJID = rep("001", 3),
    VISIT = rep("V1", 3),
    VISITNUM = rep("1", 3),
    SVDAT = rep(as.Date("2024-01-01"), 3),
    TBNAME = "LB",
    TESTCAT = rep("血常规", 3),
    TESTCAT_ORIG = rep("血常规", 3),
    TESTDE = c("白细胞", "红细胞", "血小板"),
    TESTYN = rep("否", 3),
    TESTDAT = rep(NA, 3),
    ORRES = rep(NA, 3),
    stringsAsFactors = FALSE
  )

  result <- check_missing_test(data = test_data, missing_de = FALSE)

  # TESTCAT 缺失应该只有一条记录
  testcat_missing <- result$details[result$details$missing_type == "TESTCAT_MISSING", ]
  expect_equal(nrow(testcat_missing), 1)
})


test_that("单个 TESTDE 缺失每个指标输出一条记录", {
  # 创建多个 TESTDE 缺失的数据
  test_data <- data.frame(
    SUBJID = rep("001", 3),
    VISIT = rep("V1", 3),
    VISITNUM = rep("1", 3),
    SVDAT = rep(as.Date("2024-01-01"), 3),
    TBNAME = "LB",
    TESTCAT = rep("血常规", 3),
    TESTCAT_ORIG = rep("血常规", 3),
    TESTDE = c("白细胞", "红细胞", "血小板"),
    TESTYN = rep("是", 3),
    TESTDAT = rep(as.Date("2024-01-01"), 3),
    ORRES = rep(NA, 3),
    stringsAsFactors = FALSE
  )

  result <- check_missing_test(data = test_data, missing_de = TRUE)

  # 应该有三条 TESTDE 缺失记录
  testde_missing <- result$details[result$details$missing_type == "TESTDE_MISSING", ]
  expect_equal(nrow(testde_missing), 3)

  # 每条记录的 TESTDE 应该不同
  expect_equal(length(unique(testde_missing$test_name)), 3)
})


# =============================================================================
# 测试返回对象的类
# =============================================================================

test_that("返回对象具有正确的类", {
  test_data <- create_prepared_test_data()
  result <- check_missing_test(data = test_data)

  # 检查类
  expect_true("missing_test_check" %in% class(result))
  expect_true("list" %in% class(result))
})


# =============================================================================
# 测试 TBNAME 列
# =============================================================================

test_that("details 包含正确的 TBNAME 值", {
  test_data <- create_prepared_test_data()
  result <- check_missing_test(data = test_data)

  if (nrow(result$details) > 0) {
    # TBNAME 应该是 LB
    expect_true(all(result$details$TBNAME == "LB"))
  }
})


test_that("不同 TBNAME 正确区分", {
  # 创建包含多个 TBNAME 的骨架行数据
  test_data <- data.frame(
    SUBJID = c("001", "001"),
    VISIT = c("V1", "V1"),
    VISITNUM = c("1", "1"),
    SVDAT = as.Date(c("2024-01-01", "2024-01-01")),
    TBNAME = c("LB", "VS"),
    TESTCAT = c("血常规", "心电图"),
    TESTCAT_ORIG = c(NA, NA),
    TESTDE = c(NA, NA),
    TESTYN = c(NA, NA),
    TESTDAT = as.Date(c(NA, NA)),
    ORRES = c(NA, NA),
    stringsAsFactors = FALSE
  )

  result <- check_missing_test(data = test_data)

  # 应该有两条记录，分别对应 LB 和 VS
  testcat_empty <- result$details[result$details$missing_type == "TESTCAT_EMPTY", ]
  expect_equal(nrow(testcat_empty), 2)
  expect_true("LB" %in% testcat_empty$TBNAME)
  expect_true("VS" %in% testcat_empty$TBNAME)
})
