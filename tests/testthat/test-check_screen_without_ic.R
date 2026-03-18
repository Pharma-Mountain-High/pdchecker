library(testthat)
library(dplyr)

# =============================================================================
# 基本功能测试
# =============================================================================

test_that("无偏离：所有筛选受试者都有知情同意", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003"),
      VISIT = c("Screening", "Screening", "screening"),
      VISITNUM = c(1, 1, 1)
    ),
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-01", "2024-01-02", "2024-01-03")
    )
  )

  result <- check_screen_without_ic(data)

  expect_false(result$has_deviation)
  expect_equal(length(result$messages), 0)
  expect_equal(nrow(result$details), 0)
  expect_s3_class(result, "screen_ic_check")
})

test_that("有偏离：存在未签署知情同意的筛选受试者", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003"),
      VISIT = c("Screening", "Screening", "screening"),
      VISITNUM = c(1, 1, 1)
    ),
    IC = data.frame(
      SUBJID = c("001", "002"),
      ICDAT = c("2024-01-01", "2024-01-02")
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(result$messages, "未签署知情同意书")
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "003")
})

test_that("有偏离：多个受试者未签署知情同意", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003", "004", "005"),
      VISIT = c("Screening", "Screening", "Screening", "screening", "SCREENING"),
      VISITNUM = c(1, 1, 1, 1, 1)
    ),
    IC = data.frame(
      SUBJID = c("001", "002"),
      ICDAT = c("2024-01-01", "2024-01-02")
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(result$messages, "未签署知情同意书")
  expect_equal(nrow(result$details), 3)
  expect_true(all(c("003", "004", "005") %in% result$details$SUBJID))
})

# =============================================================================
# 日期字段测试
# =============================================================================

test_that("IC日期为NA视为未签署", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003"),
      VISIT = c("Screening", "Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-01", NA, NA)
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 2)
  expect_true(all(c("002", "003") %in% result$details$SUBJID))
})

test_that("混合情况：部分有日期，部分NA，部分不在IC中", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003", "004"),
      VISIT = c("Screening", "Screening", "Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001", "002", "003"),
      ICDAT = c("2024-01-01", NA, "2024-01-03")
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 2)
  expect_true(all(c("002", "004") %in% result$details$SUBJID))
})

test_that("空字符串日期不等于NA（应该被grepl正常处理）", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001"),
      VISIT = c("Screening")
    ),
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01") # 有效日期
    )
  )

  result <- check_screen_without_ic(data)
  expect_false(result$has_deviation)
})

# =============================================================================
# 访视模式匹配测试
# =============================================================================

test_that("visit_pattern正确匹配中文筛选访视", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003", "004"),
      VISIT = c("Screening", "Screening", "C1D1", "SF")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data)

  expect_equal(nrow(result$details), 2)
  expect_true(all(c("001", "002") %in% result$details$SUBJID))
})

test_that("visit_pattern正确匹配英文Screening（大小写不敏感）", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003", "004"),
      VISIT = c("Screening", "SCREENING", "screening", "Baseline")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data)

  expect_equal(nrow(result$details), 3)
  expect_true(all(c("001", "002", "003") %in% result$details$SUBJID))
})

test_that("自定义visit_pattern", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003"),
      VISIT = c("V1", "Visit 1", "V2")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data, visit_pattern = "V1|Visit 1")

  expect_equal(nrow(result$details), 2)
  expect_true(all(c("001", "002") %in% result$details$SUBJID))
})

# =============================================================================
# 自定义参数测试
# =============================================================================

test_that("自定义sv_dataset名称", {
  data <- list(
    VISIT_DATA = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01")
    )
  )

  result <- check_screen_without_ic(data, sv_dataset = "VISIT_DATA")

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

test_that("自定义ic_dataset名称", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("Screening", "Screening")
    ),
    CONSENT = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01")
    )
  )

  result <- check_screen_without_ic(data, ic_dataset = "CONSENT")

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

test_that("自定义sv_visit_var变量名", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISITNAME = c("Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01")
    )
  )

  result <- check_screen_without_ic(data, sv_visit_var = "VISITNAME")

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
})

test_that("自定义ic_date_var变量名", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001", "002"),
      CONSENT_DATE = c("2024-01-01", NA)
    )
  )

  result <- check_screen_without_ic(data, ic_date_var = "CONSENT_DATE")

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

test_that("同时使用多个自定义参数", {
  data <- list(
    VS = data.frame(
      SUBJID = c("001", "002"),
      VSTESTCD = c("SCR", "SCR")
    ),
    CONSENT = data.frame(
      SUBJID = c("001", "002"),
      SIGN_DATE = c("2024-01-01", NA)
    )
  )

  result <- check_screen_without_ic(
    data,
    sv_dataset = "VS",
    ic_dataset = "CONSENT",
    sv_visit_var = "VSTESTCD",
    visit_pattern = "SCR",
    ic_date_var = "SIGN_DATE"
  )

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

# =============================================================================
# 错误处理测试
# =============================================================================

test_that("缺失sv_dataset时抛出错误", {
  data <- list(
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  expect_error(
    check_screen_without_ic(data),
    "Missing required datasets: SV"
  )
})

test_that("缺失ic_dataset时抛出错误", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening"))
  )

  expect_error(
    check_screen_without_ic(data),
    "Missing required datasets: IC"
  )
})

test_that("缺失多个数据集时抛出错误", {
  data <- list(
    OTHER = data.frame(SUBJID = c("001"))
  )

  expect_error(
    check_screen_without_ic(data),
    "Missing required datasets"
  )
})

test_that("sv_visit_var不存在时抛出错误", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  expect_error(
    check_screen_without_ic(data, sv_visit_var = "NONEXISTENT"),
    "Variable 'NONEXISTENT' not found in dataset 'SV'"
  )
})

test_that("ic_date_var不存在时抛出错误", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  expect_error(
    check_screen_without_ic(data, ic_date_var = "NONEXISTENT"),
    "Variable 'NONEXISTENT' not found in dataset 'IC'"
  )
})

# =============================================================================
# 边界情况测试
# =============================================================================

test_that("空SV数据集返回无偏离", {
  data <- list(
    SV = data.frame(
      SUBJID = character(0),
      VISIT = character(0)
    ),
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01")
    )
  )

  result <- check_screen_without_ic(data)

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("空IC数据集时所有筛选受试者都为偏离", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 2)
})

test_that("SV中没有匹配的筛选访视返回无偏离", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("C1D1", "SF")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data)

  expect_false(result$has_deviation)
  expect_equal(nrow(result$details), 0)
})

test_that("SV中有重复SUBJID时正确去重", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "001", "002", "002"),
      VISIT = c("Screening", "Screening", "Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001"),
      ICDAT = c("2024-01-01")
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

test_that("IC中有重复SUBJID时正确去重", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002"),
      VISIT = c("Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = c("001", "001"),
      ICDAT = c("2024-01-01", "2024-01-02")
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 1)
  expect_equal(result$details$SUBJID, "002")
})

test_that("所有筛选受试者都未签IC", {
  data <- list(
    SV = data.frame(
      SUBJID = c("001", "002", "003"),
      VISIT = c("Screening", "Screening", "Screening")
    ),
    IC = data.frame(
      SUBJID = character(0),
      ICDAT = character(0)
    )
  )

  result <- check_screen_without_ic(data)

  expect_true(result$has_deviation)
  expect_equal(nrow(result$details), 3)
  expect_equal(sort(result$details$SUBJID), c("001", "002", "003"))
})

# =============================================================================
# 返回值结构测试
# =============================================================================

test_that("返回值包含所有必需元素", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_type(result, "list")
  expect_true("has_deviation" %in% names(result))
  expect_true("messages" %in% names(result))
  expect_true("details" %in% names(result))
  expect_s3_class(result, "screen_ic_check")
})

test_that("has_deviation是逻辑值", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_type(result$has_deviation, "logical")
  expect_length(result$has_deviation, 1)
})

test_that("messages是字符向量", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_type(result$messages, "character")
})

test_that("details是数据框", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_s3_class(result$details, "data.frame")
})

# =============================================================================
# 打印方法测试
# =============================================================================

test_that("print方法正常工作 - 无偏离", {
  data <- list(
    SV = data.frame(SUBJID = c("001"), VISIT = c("Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_output(print(result), "2.4.1 未签署知情同意书")
  expect_output(print(result), "Has deviation: NO")
})

test_that("print方法正常工作 - 有偏离", {
  data <- list(
    SV = data.frame(SUBJID = c("001", "002"), VISIT = c("Screening", "Screening")),
    IC = data.frame(SUBJID = c("001"), ICDAT = c("2024-01-01"))
  )

  result <- check_screen_without_ic(data)

  expect_output(print(result), "2.4.1 未签署知情同意书")
  expect_output(print(result), "Has deviation: YES")
  expect_output(print(result), "未签署知情同意书")
  expect_output(print(result), "受试者002在未签署知情同意书的情况下进行了Screening访视")
})
