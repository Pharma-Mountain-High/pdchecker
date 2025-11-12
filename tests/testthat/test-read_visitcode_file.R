library(testthat)
library(tibble)
library(readxl)

# 辅助函数：创建临时Excel文件
create_temp_excel_file <- function(data, filename) {
  temp_file <- file.path(tempdir(), filename)
  writexl::write_xlsx(data, temp_file)
  temp_file
}

# 辅助函数：创建临时CSV文件
create_temp_csv_file <- function(data, filename) {
  temp_file <- file.path(tempdir(), filename)
  write.csv(data, temp_file, row.names = FALSE)
  temp_file
}

# ============================================================================
# 测试 parse_window_period() 函数
# ============================================================================

test_that("parse_window_period 处理缺失值", {
  result <- pdchecker:::parse_window_period(NA)
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))

  result <- pdchecker:::parse_window_period("")
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))

  result <- pdchecker:::parse_window_period(NULL)
  expect_true(is.na(result$type))
  expect_true(is.na(result$value))
})

test_that("parse_window_period 解析 ± 类型", {
  result <- pdchecker:::parse_window_period("±3d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("±24h")
  expect_equal(result$type, "±")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("±2周")
  expect_equal(result$type, "±")
  expect_equal(result$value, 14)

  result <- pdchecker:::parse_window_period("±1w")
  expect_equal(result$type, "±")
  expect_equal(result$value, 7)
})

test_that("parse_window_period 解析 ≤ 类型", {
  result <- pdchecker:::parse_window_period("≤24h")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("≤1d")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("<=2天")
  expect_equal(result$type, "≤")
  expect_equal(result$value, 2)
})

test_that("parse_window_period 解析 ≥ 类型", {
  result <- pdchecker:::parse_window_period("≥1d")
  expect_equal(result$type, "≥")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period(">=3天")
  expect_equal(result$type, "≥")
  expect_equal(result$value, 3)
})

test_that("parse_window_period 解析 + 类型", {
  result <- pdchecker:::parse_window_period("+2d")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)

  result <- pdchecker:::parse_window_period("+3天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("+48h")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)
})

test_that("parse_window_period 解析 - 类型", {
  result <- pdchecker:::parse_window_period("-1d")
  expect_equal(result$type, "-")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("-2天")
  expect_equal(result$type, "-")
  expect_equal(result$value, 2)
})

test_that("parse_window_period 解析范围类型", {
  result <- pdchecker:::parse_window_period("-2到+4")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-2到+4")

  result <- pdchecker:::parse_window_period("1至3天")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "1至3天")
})

test_that("parse_window_period 解析数字无前缀类型", {
  result <- pdchecker:::parse_window_period("2d")
  expect_equal(result$type, "+")
  expect_equal(result$value, 2)

  result <- pdchecker:::parse_window_period("3天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("1w")
  expect_equal(result$type, "+")
  expect_equal(result$value, 7)
})

test_that("parse_window_period 转换时间单位", {
  # 小时转天
  result <- pdchecker:::parse_window_period("±24h")
  expect_equal(result$value, 1)

  result <- pdchecker:::parse_window_period("±12小时")
  expect_equal(result$value, 0.5)

  # 周转天
  result <- pdchecker:::parse_window_period("±1w")
  expect_equal(result$value, 7)

  result <- pdchecker:::parse_window_period("±2周")
  expect_equal(result$value, 14)

  # 天数
  result <- pdchecker:::parse_window_period("±3d")
  expect_equal(result$value, 3)

  result <- pdchecker:::parse_window_period("±4天")
  expect_equal(result$value, 4)

  result <- pdchecker:::parse_window_period("±5日")
  expect_equal(result$value, 5)
})

test_that("parse_window_period 处理其他格式", {
  result <- pdchecker:::parse_window_period("固定访视")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "固定访视")
})

test_that("parse_window_period 处理带空格的输入", {
  result <- pdchecker:::parse_window_period("  ±3d  ")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)
})

test_that("parse_window_period 处理小数时间单位", {
  # 小数天数
  result <- pdchecker:::parse_window_period("±1.5d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 1.5)

  # 小数小时
  result <- pdchecker:::parse_window_period("±6h")
  expect_equal(result$type, "±")
  expect_equal(result$value, 0.25)

  # 小数周
  result <- pdchecker:::parse_window_period("+0.5w")
  expect_equal(result$type, "+")
  expect_equal(result$value, 3.5)
})

test_that("parse_window_period 处理零值", {
  result <- pdchecker:::parse_window_period("±0d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 0)

  result <- pdchecker:::parse_window_period("0天")
  expect_equal(result$type, "+")
  expect_equal(result$value, 0)
})

test_that("parse_window_period 处理大数值", {
  result <- pdchecker:::parse_window_period("±100d")
  expect_equal(result$type, "±")
  expect_equal(result$value, 100)

  result <- pdchecker:::parse_window_period("±52w")
  expect_equal(result$type, "±")
  expect_equal(result$value, 364)
})

test_that("parse_window_period 处理无效数值", {
  # 无法解析的数值会返回 NA 并产生警告
  expect_warning(
    result <- pdchecker:::parse_window_period("±abc天"),
    "NAs introduced by coercion"
  )
  expect_equal(result$type, "±")
  expect_true(is.na(result$value))

  expect_warning(
    result <- pdchecker:::parse_window_period("≤xyz小时"),
    "NAs introduced by coercion"
  )
  expect_equal(result$type, "≤")
  expect_true(is.na(result$value))
})

test_that("parse_window_period 处理不同的日期单位写法", {
  # 测试"日"作为单位
  result <- pdchecker:::parse_window_period("±3日")
  expect_equal(result$type, "±")
  expect_equal(result$value, 3)

  # 测试"小时"作为单位
  result <- pdchecker:::parse_window_period("+6小时")
  expect_equal(result$type, "+")
  expect_equal(result$value, 0.25)
})

test_that("parse_window_period 处理无单位的纯数字", {
  result <- pdchecker:::parse_window_period("7")
  expect_equal(result$type, "+")
  expect_equal(result$value, 7)

  result <- pdchecker:::parse_window_period("+5")
  expect_equal(result$type, "+")
  expect_equal(result$value, 5)
})

test_that("parse_window_period 处理负号开头但有范围关键字", {
  # 包含"到"或"至"应识别为范围，而非负号类型
  result <- pdchecker:::parse_window_period("-3到+7")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-3到+7")

  result <- pdchecker:::parse_window_period("-1至+1")
  expect_equal(result$type, "范围")
  expect_equal(result$value, "-1至+1")
})

test_that("parse_window_period 处理多种其他格式", {
  result <- pdchecker:::parse_window_period("固定")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "固定")

  result <- pdchecker:::parse_window_period("不限")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "不限")

  result <- pdchecker:::parse_window_period("NA")
  expect_equal(result$type, "其他")
  expect_equal(result$value, "NA")
})

# ============================================================================
# 测试 read_visitcode_file() 函数
# ============================================================================

test_that("read_visitcode_file 读取Excel文件", {
  # 创建测试数据
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3"),
    访视名称 = c("筛选访视", "基线访视", "第一次随访"),
    窗口期 = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_visitcode.xlsx")

  result <- read_visitcode_file(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true("type" %in% names(result))
  expect_true("wpvalue" %in% names(result))

  expect_equal(result$type, c("±", "≤", "+"))
  expect_equal(result$wpvalue, c("3", "1", "2"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 读取CSV文件", {
  # 创建测试数据
  test_data <- data.frame(
    访视编码 = c("V1", "V2"),
    访视名称 = c("筛选访视", "基线访视"),
    窗口期 = c("-1d", "±2周"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_visitcode.csv")

  result <- read_visitcode_file(temp_file)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$type, c("-", "±"))
  expect_equal(result$wpvalue, c("1", "14"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 支持多种窗口期列名", {
  # 测试不同的列名
  col_names <- c("窗口期", "访视窗口", "窗口", "window", "Window")

  for (col_name in col_names) {
    test_data <- data.frame(
      访视编码 = c("V1"),
      stringsAsFactors = FALSE
    )
    test_data[[col_name]] <- "±3d"

    temp_file <- create_temp_csv_file(test_data, paste0("test_", col_name, ".csv"))

    result <- read_visitcode_file(temp_file)

    expect_equal(result$type[1], "±")
    expect_equal(result$wpvalue[1], "3")

    # 清理临时文件
    unlink(temp_file)
  }
})

test_that("read_visitcode_file 处理包含NA值的数据", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3"),
    窗口期 = c("±3d", NA, ""),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_with_na.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type[1], "±")
  expect_true(is.na(result$type[2]))
  expect_true(is.na(result$type[3]))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理空文件", {
  test_data <- data.frame(
    访视编码 = character(0),
    窗口期 = character(0),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_empty.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 0)
  expect_true("type" %in% names(result))
  expect_true("wpvalue" %in% names(result))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 文件不存在时报错", {
  expect_error(
    read_visitcode_file("nonexistent_file.xlsx"),
    "文件未找到"
  )
})

test_that("read_visitcode_file 不支持的文件格式时报错", {
  # 创建一个txt文件
  temp_file <- file.path(tempdir(), "test.txt")
  writeLines("test", temp_file)

  expect_error(
    read_visitcode_file(temp_file),
    "不支持的文件格式"
  )

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 缺少窗口期列时报错", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2"),
    访视名称 = c("筛选访视", "基线访视"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_no_window.csv")

  expect_error(
    read_visitcode_file(temp_file),
    "文件中缺少窗口期列"
  )

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 读取指定sheet", {
  # 创建测试数据
  test_data <- data.frame(
    访视编码 = c("V1"),
    窗口期 = c("±3d"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_excel_file(test_data, "test_sheet.xlsx")

  # 默认读取Sheet1
  result <- read_visitcode_file(temp_file, sheet_name = "Sheet1")

  expect_equal(nrow(result), 1)
  expect_equal(result$type[1], "±")

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 完整场景测试", {
  # 创建包含多种窗口期格式的测试数据
  test_data <- data.frame(
    访视编码 = c("V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"),
    访视名称 = c("筛选", "基线", "第1次", "第2次", "第3次", "第4次", "第5次", "第6次", "第7次"),
    窗口期 = c("±3d", "≤24h", "+2天", "-1d", "1w", "±2周", "≥1d", "-2到+4", "固定"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_complete.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 9)
  expect_equal(result$type, c("±", "≤", "+", "-", "+", "±", "≥", "范围", "其他"))
  expect_equal(result$wpvalue, c("3", "1", "2", "1", "7", "14", "1", "-2到+4", "固定"))

  # 验证原始列保留
  expect_true("访视编码" %in% names(result))
  expect_true("访视名称" %in% names(result))
  expect_true("窗口期" %in% names(result))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理列名冲突", {
  # 创建已有 type 和 wpvalue 列的数据
  test_data <- data.frame(
    访视编码 = c("V1", "V2"),
    窗口期 = c("±3d", "≤24h"),
    type = c("旧类型1", "旧类型2"),
    wpvalue = c("旧值1", "旧值2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_conflict.csv")

  # 应该发出消息提示覆盖
  expect_message(
    result <- read_visitcode_file(temp_file),
    "已存在 'type' 列，将被覆盖"
  )

  expect_message(
    result <- read_visitcode_file(temp_file),
    "已存在 'wpvalue' 列，将被覆盖"
  )

  # 验证列已被覆盖为新解析的值
  expect_equal(result$type, c("±", "≤"))
  expect_equal(result$wpvalue, c("3", "1"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理混合窗口期类型", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3", "V4", "V5", "V6", "V7"),
    窗口期 = c("±3d", "≤24h", "≥7天", "+2w", "-1d", "1至3天", "固定"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_mixed.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type, c("±", "≤", "≥", "+", "-", "范围", "其他"))
  expect_equal(result$wpvalue, c("3", "1", "7", "14", "1", "1至3天", "固定"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理小数窗口期", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3"),
    窗口期 = c("±1.5d", "≤0.5天", "+2.5w"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_decimal.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(result$type, c("±", "≤", "+"))
  expect_equal(result$wpvalue, c("1.5", "0.5", "17.5"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 保留原始列完整性", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2"),
    访视名称 = c("筛选", "基线"),
    访视日期 = c("第1天", "第7天"),
    窗口期 = c("±3d", "≤24h"),
    备注 = c("备注1", "备注2"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_integrity.csv")

  result <- read_visitcode_file(temp_file)

  # 验证所有原始列都存在
  expect_true(all(c("访视编码", "访视名称", "访视日期", "窗口期", "备注") %in% names(result)))

  # 验证新增列存在
  expect_true(all(c("type", "wpvalue") %in% names(result)))

  # 验证原始数据未被修改
  expect_equal(result$访视编码, c("V1", "V2"))
  expect_equal(result$访视名称, c("筛选", "基线"))
  expect_equal(result$备注, c("备注1", "备注2"))

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 列名优先级测试", {
  # 当存在多个可能的窗口期列名时，应按优先级选择
  test_data <- data.frame(
    访视编码 = c("V1"),
    窗口期 = "±3d",
    window = "±5d", # 应该优先使用"窗口期"列
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_priority.csv")

  result <- read_visitcode_file(temp_file)

  # 应该使用"窗口期"列的值
  expect_equal(result$type[1], "±")
  expect_equal(result$wpvalue[1], "3")

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理包含无效值的混合数据", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3", "V4"),
    窗口期 = c("±3d", NA, "invalid", "≤24h"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_mixed_invalid.csv")

  result <- read_visitcode_file(temp_file)

  # V1 正常解析
  expect_equal(result$type[1], "±")
  expect_equal(result$wpvalue[1], "3")

  # V2 是 NA
  expect_true(is.na(result$type[2]))
  expect_true(is.na(result$wpvalue[2]))

  # V3 无法识别，归为"其他"
  expect_equal(result$type[3], "其他")
  expect_equal(result$wpvalue[3], "invalid")

  # V4 正常解析
  expect_equal(result$type[4], "≤")
  expect_equal(result$wpvalue[4], "1")

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理大数据集", {
  # 创建100行数据
  test_data <- data.frame(
    访视编码 = paste0("V", 1:100),
    窗口期 = rep(c("±3d", "≤24h", "+2天", "-1d", "1w"), 20),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_large.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 100)
  expect_true(all(c("type", "wpvalue") %in% names(result)))

  # 验证解析结果的模式
  expected_types <- rep(c("±", "≤", "+", "-", "+"), 20)
  expect_equal(result$type, expected_types)

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理只有一行数据的文件", {
  test_data <- data.frame(
    访视编码 = "V1",
    窗口期 = "±3d",
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_single_row.csv")

  result <- read_visitcode_file(temp_file)

  expect_equal(nrow(result), 1)
  expect_equal(result$type[1], "±")
  expect_equal(result$wpvalue[1], "3")

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file 处理特殊字符在访视名称中", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3"),
    访视名称 = c("第1次访视（基线）", "随访 - 第2周", "终点访视/退出"),
    窗口期 = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  temp_file <- create_temp_csv_file(test_data, "test_special_chars.csv")

  result <- read_visitcode_file(temp_file)

  # 验证特殊字符不影响解析
  expect_equal(nrow(result), 3)
  expect_equal(result$type, c("±", "≤", "+"))

  # 验证访视名称保持完整
  expect_equal(result$访视名称[1], "第1次访视（基线）")

  # 清理临时文件
  unlink(temp_file)
})

test_that("read_visitcode_file Excel和CSV结果一致性", {
  test_data <- data.frame(
    访视编码 = c("V1", "V2", "V3"),
    窗口期 = c("±3d", "≤24h", "+2天"),
    stringsAsFactors = FALSE
  )

  # 创建Excel和CSV文件
  excel_file <- create_temp_excel_file(test_data, "test_consistency.xlsx")
  csv_file <- create_temp_csv_file(test_data, "test_consistency.csv")

  result_excel <- read_visitcode_file(excel_file)
  result_csv <- read_visitcode_file(csv_file)

  # 验证两种文件格式解析结果一致
  expect_equal(result_excel$type, result_csv$type)
  expect_equal(result_excel$wpvalue, result_csv$wpvalue)

  # 清理临时文件
  unlink(excel_file)
  unlink(csv_file)
})
