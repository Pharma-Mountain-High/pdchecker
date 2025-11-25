# 测试 prepare_test_data() 函数
# =============================================================================

# 创建测试数据
# =============================================================================

create_test_data <- function() {
  # 访视数据
  sv_data <- data.frame(
    SUBJID = c("001", "001", "001", "002", "002", "002", "003", "003", "003"),
    VISIT = c("V1", "V2", "V3", "V1", "V2", "V3", "V1", "V2", "V3"),
    VISITNUM = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    SVDAT = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-01",
      "2024-01-02", "2024-01-16", "2024-02-02",
      "2024-01-03", "2024-01-17", "2024-02-03"
    )),
    stringsAsFactors = FALSE
  )

  # 实验室检查数据
  lb_data <- data.frame(
    SUBJID = c("001", "001", "002", "002", "003", "003"),
    VISIT = c("V1", "V2", "V1", "V2", "V1", "V2"),
    VISITNUM = c(1, 2, 1, 2, 1, 2),
    LBCAT = c("血常规", "血常规", "血常规", "血生化", "血常规", "血常规"),
    LBTEST = c("白细胞", "白细胞", "白细胞", "肝功能", "白细胞", "白细胞"),
    LBDAT = as.Date(c(
      "2024-01-01", "2024-01-15",
      "2024-01-02", "2024-01-16",
      "2024-01-03", "2024-01-17"
    )),
    ORRES = c("5.5", "6.2", "5.8", "正常", "6.0", "5.9"),
    YN = c("是", "是", "是", "是", "是", "是"),
    stringsAsFactors = FALSE
  )

  # 受试者信息数据
  subject_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    SEX = c("男", "女", "男"),
    AGE = c(25, 30, 45),
    stringsAsFactors = FALSE
  )

  # 人口学数据
  dm_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    AGE = c(25, 30, 45),
    BMI = c(22.5, 28.0, 25.5),
    stringsAsFactors = FALSE
  )

  # 入组数据
  enrol_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    ENRYN = c("是", "是", "否"),
    stringsAsFactors = FALSE
  )

  list(
    SV = sv_data,
    LB = lb_data,
    SUBJECT = subject_data,
    DM = dm_data,
    ENROL = enrol_data
  )
}


# =============================================================================
# 测试基本功能
# =============================================================================

test_that("prepare_test_data() 基本功能正常", {
  test_data <- create_test_data()

  # 禁用入组筛选，测试基本功能
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST" # 使用不存在的数据集名，禁用入组筛选
    )
  })

  # 检查返回的数据框不为空
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # 检查必要的列存在
  expected_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME",
    "TESTCAT", "TESTDE", "TESTYN", "TESTDAT", "ORRES"
  )
  expect_true(all(expected_cols %in% names(result)))

  # 检查 TBNAME 列被正确设置
  expect_true(all(result$TBNAME == "LB"))

  # 检查 TESTCAT 列被正确映射
  expect_true("血常规" %in% result$TESTCAT)
})


test_that("prepare_test_data() 正确合并访视和检查数据", {
  test_data <- create_test_data()

  # 禁用入组筛选，测试纯粹的合并逻辑
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST" # 使用不存在的数据集名，禁用入组筛选
    )
  })

  # 检查返回的记录数应该等于访视记录数（left_join 基于访视）
  expect_equal(nrow(result), nrow(test_data$SV))

  # 检查所有访视记录都在结果中
  expect_setequal(
    paste(result$SUBJID, result$VISIT),
    paste(test_data$SV$SUBJID, test_data$SV$VISIT)
  )
})


# =============================================================================
# 测试 filter_cond 参数（单个数据集筛选）
# =============================================================================

test_that("filter_cond 单个数据集筛选：筛选男性受试者", {
  test_data <- create_test_data()

  # 禁用入组筛选，只测试 filter_cond
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = "SUBJECT|SEX=='男'"
    )
  })

  # 检查只保留了男性受试者
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "003"))
  expect_false("002" %in% unique_subjids) # 002 是女性
})


test_that("filter_cond 单个数据集筛选：筛选年龄>30的受试者", {
  test_data <- create_test_data()

  # 禁用入组筛选，只测试 filter_cond
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = "SUBJECT|AGE>30"
    )
  })

  # 检查只保留了年龄>30的受试者
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003") # 只有 003 年龄为 45
})


test_that("filter_cond 单个数据集筛选：组合多个条件", {
  test_data <- create_test_data()

  # 禁用入组筛选，只测试 filter_cond
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = "SUBJECT|SEX=='男' & AGE>=25 & AGE<=30"
    )
  })

  # 检查只保留了男性且年龄在25-30之间的受试者
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "001") # 只有 001 符合条件
})


# =============================================================================
# 测试 filter_cond 参数（多个数据集筛选）
# =============================================================================

test_that("filter_cond 多个数据集筛选：两个条件取交集", {
  test_data <- create_test_data()

  # 禁用入组筛选，只测试 filter_cond
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = "SUBJECT|SEX=='男';DM|AGE>=30"
    )
  })

  # SUBJECT 中男性：001, 003
  # DM 中 AGE>=30：002, 003
  # 交集：003
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003")
})


test_that("filter_cond 多个数据集筛选：三个条件取交集", {
  test_data <- create_test_data()

  # 禁用入组筛选，只测试 filter_cond
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = "SUBJECT|SEX=='男';DM|AGE>=25;DM|BMI<26"
    )
  })

  # SUBJECT 中男性：001, 003
  # DM 中 AGE>=25：001, 002, 003
  # DM 中 BMI<26：001, 003
  # 交集：001, 003
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "003"))
})


test_that("filter_cond 多个数据集筛选：条件前后有空格", {
  test_data <- create_test_data()

  # 禁用入组筛选，测试格式容错：条件前后有空格
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = " SUBJECT | SEX=='男' ; DM | AGE>=30 "
    )
  })

  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003")
})


# =============================================================================
# 测试入组筛选
# =============================================================================

test_that("入组筛选：只保留入组的受试者", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    enrl_dataset = "ENROL",
    enrl_yn_var = "ENRYN"
  )

  # 只有 001 和 002 入组了（ENRYN="是"）
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002"))
  expect_false("003" %in% unique_subjids)
})


# =============================================================================
# 测试入组筛选和 filter_cond 同时使用
# =============================================================================

test_that("入组筛选和 filter_cond 同时使用：取交集", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    enrl_dataset = "ENROL",
    enrl_yn_var = "ENRYN",
    filter_cond = "SUBJECT|SEX=='男'"
  )

  # 入组的：001, 002
  # 男性：001, 003
  # 交集：001
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "001")
})


test_that("入组筛选和多个 filter_cond 同时使用", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    enrl_dataset = "ENROL",
    enrl_yn_var = "ENRYN",
    filter_cond = "SUBJECT|SEX=='男';DM|BMI<26"
  )

  # 入组的：001, 002
  # 男性：001, 003
  # BMI<26：001, 003
  # 交集：001
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "001")
})


# =============================================================================
# 测试配置文件功能
# =============================================================================

test_that("使用配置生成访视-检查项骨架", {
  test_data <- create_test_data()

  # 创建配置数据框
  config_df <- data.frame(
    TESTCAT = c("血常规", "血生化"),
    VISITNUM = c("1,2,3", "1,2"),
    stringsAsFactors = FALSE
  )

  # 禁用入组筛选，只测试配置功能
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      config = config_df
    )
  })

  # 检查返回的数据包含配置的检查分类
  expect_true("血常规" %in% result$TESTCAT)
  expect_true("血生化" %in% result$TESTCAT)

  # 检查血常规在所有3个访视都有记录
  blood_routine <- result[result$TESTCAT == "血常规" & result$SUBJID == "001", ]
  expect_equal(nrow(blood_routine), 3) # V1, V2, V3
})


test_that("config_cat 参数筛选特定检查分类", {
  test_data <- create_test_data()

  config_df <- data.frame(
    TESTCAT = c("血常规", "血生化", "尿常规"),
    VISITNUM = c("1,2", "1,2", "1,2"),
    stringsAsFactors = FALSE
  )

  # 禁用入组筛选，只测试配置功能
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      config = config_df,
      config_cat = c("血常规", "血生化") # 只使用这两个
    )
  })

  # 检查只包含指定的检查分类
  unique_testcat <- unique(result$TESTCAT)
  expect_true(all(unique_testcat %in% c("血常规", "血生化")))
  expect_false("尿常规" %in% unique_testcat)
})


# =============================================================================
# 测试错误处理
# =============================================================================

test_that("filter_cond 格式错误时报错", {
  test_data <- create_test_data()

  # 格式错误：缺少分隔符 |
  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "SUBJECT SEX=='男'" # 缺少 |
    ),
    "filter_cond 参数格式错误"
  )
})


test_that("filter_cond 中指定的数据集不存在时报错", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "NONEXIST|SEX=='男'" # 数据集不存在
    ),
    "filter_cond 中指定的数据集不存在"
  )
})


test_that("filter_cond 中的数据集缺少 SUBJID 列时报错", {
  test_data <- create_test_data()

  # 创建一个没有 SUBJID 的数据集
  test_data$NOSUBJID <- data.frame(
    ID = c("001", "002", "003"),
    VALUE = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "NOSUBJID|VALUE>1"
    ),
    "中缺少 SUBJID 列"
  )
})


test_that("filter_cond 筛选条件语法错误时报错", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "SUBJECT|NONEXIST_COL=='男'" # 列不存在
    ),
    "筛选条件.*执行失败"
  )
})


test_that("缺少必需参数时报错", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data
      # 缺少 test_dataset
    ),
    "必须指定 test_dataset 参数"
  )
})


test_that("test_dataset 不存在时报错", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "NONEXIST"
    ),
    "data 中不存在检查项数据集"
  )
})


test_that("访视数据集不存在时报错", {
  test_data <- create_test_data()
  test_data$SV <- NULL # 移除访视数据集

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB"
    ),
    "data 中不存在访视数据集"
  )
})


# =============================================================================
# 测试边界情况
# =============================================================================

test_that("filter_cond 为 NULL 时不进行筛选", {
  test_data <- create_test_data()

  # 禁用入组筛选，测试 filter_cond = NULL 的行为
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = NULL
    )
  })

  # 应该包含所有受试者
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond 为空字符串时不进行筛选", {
  test_data <- create_test_data()

  # 禁用入组筛选，测试 filter_cond = "" 的行为
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      filter_cond = ""
    )
  })

  # 应该包含所有受试者
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond 未匹配到任何受试者时给出警告", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      filter_cond = "SUBJECT|AGE>100" # 没有受试者年龄>100
    ),
    "未匹配到任何受试者"
  )
})


test_that("多个 filter_cond 的交集为空时给出警告", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      filter_cond = "SUBJECT|SEX=='男';SUBJECT|SEX=='女'" # 交集为空
    ),
    "所有筛选条件的交集为空"
  )
})


test_that("入组筛选和 filter_cond 的交集为空时给出警告", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "ENROL",
      enrl_yn_var = "ENRYN",
      filter_cond = "SUBJECT|SUBJID=='003'" # 003未入组
    ),
    "入组筛选和自定义筛选的交集为空"
  )
})


test_that("入组数据集不存在时给出警告", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST"
    ),
    "data 中不存在入组数据集"
  )
})


# =============================================================================
# 测试列名映射
# =============================================================================

test_that("TBNAME 正确映射", {
  test_data <- create_test_data()

  # 禁用入组筛选
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST", # 禁用入组筛选
      tb_name_var = NULL # 使用 test_dataset 作为 TBNAME
    )
  })

  expect_true(all(result$TBNAME == "LB"))
})


test_that("TESTCAT 正确映射", {
  test_data <- create_test_data()

  # 禁用入组筛选
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST" # 禁用入组筛选
    )
  })

  # 检查 TESTCAT 列被正确创建
  expect_true("TESTCAT" %in% names(result))
  # 检查有非NA的TESTCAT值
  expect_true(any(!is.na(result$TESTCAT)))
})


test_that("TESTDE 正确映射", {
  test_data <- create_test_data()

  # 禁用入组筛选
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST" # 禁用入组筛选
    )
  })

  # 检查 TESTDE 列被正确创建
  expect_true("TESTDE" %in% names(result))
  # 检查值被正确映射
  expect_true("白细胞" %in% result$TESTDE[!is.na(result$TESTDE)])
})


test_that("列顺序正确", {
  test_data <- create_test_data()

  # 禁用入组筛选
  suppressWarnings({
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      enrl_dataset = "NONEXIST" # 禁用入组筛选
    )
  })

  # 检查列顺序：关键列在前，衍生列次之
  first_cols <- names(result)[1:9]
  expect_equal(
    first_cols,
    c(
      "SUBJID", "VISIT", "VISITNUM", "TBNAME", "TESTCAT", "TESTDE",
      "TESTYN", "TESTDAT", "ORRES"
    )
  )
})
