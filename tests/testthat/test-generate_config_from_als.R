library(testthat)

create_als_test_workbook <- function(dir) {
  visit_df <- data.frame(
    VISIT = c("筛选期- D-28~D-1", "筛选期- D-7~D-1", "纠正治疗期_D1"),
    VISITNUM = c(101, 102, 201),
    WP = c("", "±3d", "±1d"),
    CYCLE = c("筛选期", "筛选期", "纠正治疗期"),
    VISITDAY = c(-28, -7, 1),
    stringsAsFactors = FALSE
  )

  matrix_df <- data.frame(
    `eCRF/Visit` = c("访视日期(SV)", "血常规(LB)"),
    `筛选期- D-28~D-1` = c("\u2611", "\u2611"),
    `筛选期- D-7~D-1` = c("\u2611", ""),
    `纠正治疗期_D1` = c("\u2611", "\u2611"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  header_row <- data.frame(
    `eCRF/Visit` = "eCRF/Visit",
    `筛选期- D-28~D-1` = "筛选期- D-28~D-1",
    `筛选期- D-7~D-1` = "筛选期- D-7~D-1",
    `纠正治疗期_D1` = "纠正治疗期_D1",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  matrix_out <- rbind(header_row, matrix_df)
  path <- file.path(dir, "als_test.xlsx")
  writexl::write_xlsx(
    list(
      `访视` = visit_df,
      `Casebook TRIGGER` = matrix_out
    ),
    path
  )
  path
}

test_that("infer_visit_cycle maps standard types", {
  infer <- pdchecker:::infer_visit_cycle
  expect_equal(infer("筛选期- D-28~D-1"), "筛选期")
  expect_equal(infer("纠正治疗期_D1"), "预治疗")
  expect_equal(infer("维持治疗期_C1D8"), "治疗期1")
  expect_equal(infer("延伸治疗期_C3D15"), "治疗期3")
  expect_equal(infer("导入期（C2/D1）"), "治疗期2")
  expect_equal(infer("治疗结束/退出研究访视"), "治疗结束")
  expect_equal(infer("研究结束"), "研究结束")
  expect_equal(infer("安全性访视"), "随访期")
  expect_equal(infer("共同页"), "")
})

test_that("infer_visitday parses day patterns", {
  infer <- pdchecker:::infer_visitday
  expect_equal(infer("筛选期- D-28~D-1"), "-28")
  expect_equal(infer("纠正治疗期_D8"), "8")
  expect_equal(infer("维持治疗期_C1D15"), "15")
  expect_equal(infer("治疗结束/退出研究访视"), "EOT")
  expect_equal(infer("研究结束"), "EOS")
})

test_that("serialize_als_wb produces sheet blocks", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempdir()
  als_test <- create_als_test_workbook(tmp)
  text <- pdchecker:::serialize_als_wb(als_test, max_rows = 10, max_cols = 10)
  expect_true(grepl("### 工作表: 访视", text, fixed = TRUE))
  expect_true(grepl("### 工作表: Casebook TRIGGER", text, fixed = TRUE))
})

test_that("normalize_als_config builds valid tables", {
  mock <- list(
    visitcode = data.frame(
      VISIT = c("筛选期- D-28~D-1", "纠正治疗期_D1", "维持治疗期_C1D8"),
      VISITNUM = c(101, 201, 301),
      WP = c("±3d", "", ""),
      CYCLE = c("wrong", "", ""),
      VISITDAY = c("", "", ""),
      stringsAsFactors = FALSE
    ),
    testconfig = data.frame(
      TESTCAT = c("访视日期(SV)", "血常规(LB)"),
      VISITNUM = c("101,201", "101"),
      FORM = c("访视日期(SV)", "血常规(LB)"),
      TBNAME = c("SV", "LB"),
      stringsAsFactors = FALSE
    ),
    notes = "Used 访视 sheet for VISITNUM."
  )
  out <- pdchecker:::normalize_als_config(
    parsed_json = mock,
    exclude_visits = "共同页",
    exclude_forms = NULL
  )
  expect_equal(out$visitcode$VISITNUM, c(101, 201, 301))
  expect_equal(out$visitcode$VISITDAY, c("-28", "1", "8"))
  expect_equal(out$visitcode$CYCLE, c("筛选期", "预治疗", "治疗期1"))
  expect_equal(out$visitcode$WP, c("", "", ""))
  expect_equal(out$testconfig$TESTCAT, "血常规")
  expect_equal(out$testconfig$VISITNUM, "101")
  expect_false("TBNAME" %in% names(out$testconfig))
  expect_equal(names(out$testconfig), c("TESTCAT", "VISITNUM", "FORM"))
})

test_that(".ai_creds reads required env vars", {
  old <- list(
    OPENROUTER_API_KEY = Sys.getenv("OPENROUTER_API_KEY"),
    OPENROUTER_BASE_URL = Sys.getenv("OPENROUTER_BASE_URL"),
    OPENROUTER_MODEL = Sys.getenv("OPENROUTER_MODEL")
  )
  on.exit(
    Sys.setenv(
      OPENROUTER_API_KEY = old$OPENROUTER_API_KEY,
      OPENROUTER_BASE_URL = old$OPENROUTER_BASE_URL,
      OPENROUTER_MODEL = old$OPENROUTER_MODEL
    ),
    add = TRUE
  )
  Sys.setenv(
    OPENROUTER_API_KEY = "test-key",
    OPENROUTER_BASE_URL = "https://ai-api.qilu-pharma.com/v1",
    OPENROUTER_MODEL = "deepseek-v4-flash"
  )

  creds <- pdchecker:::.ai_creds()
  expect_equal(creds$api_key, "test-key")
  expect_equal(creds$base_url, "https://ai-api.qilu-pharma.com/v1")
  expect_equal(creds$model, "deepseek-v4-flash")
})

test_that("generate_config_from_als requires AI env vars", {
  skip_if_not_installed("writexl")
  tmp <- withr::local_tempdir()
  als_test <- create_als_test_workbook(tmp)

  old <- list(
    OPENROUTER_API_KEY = Sys.getenv("OPENROUTER_API_KEY"),
    OPENROUTER_BASE_URL = Sys.getenv("OPENROUTER_BASE_URL"),
    OPENROUTER_MODEL = Sys.getenv("OPENROUTER_MODEL")
  )
  on.exit(
    Sys.setenv(
      OPENROUTER_API_KEY = old$OPENROUTER_API_KEY,
      OPENROUTER_BASE_URL = old$OPENROUTER_BASE_URL,
      OPENROUTER_MODEL = old$OPENROUTER_MODEL
    ),
    add = TRUE
  )
  Sys.setenv(
    OPENROUTER_API_KEY = "",
    OPENROUTER_BASE_URL = "",
    OPENROUTER_MODEL = ""
  )

  expect_error(
    generate_config_from_als(als_file = als_test, write_files = FALSE),
    "Missing AI environment variables"
  )
})

test_that("parse_als_form_label extracts code", {
  info <- pdchecker:::parse_als_form_label("血常规(LB)")
  expect_equal(info$label, "血常规")
  expect_equal(info$code, "LB")
})

test_that("to_testcat_chinese strips parentheses and English codes", {
  expect_equal(pdchecker:::to_testcat_chinese("血常规(LB)"), "血常规")
  expect_equal(pdchecker:::to_testcat_chinese("LB"), "")
  expect_equal(pdchecker:::to_testcat_chinese("访视日期(SV)"), "访视日期")
})

test_that("is_als_check_form excludes non-examination forms", {
  check <- pdchecker:::is_als_check_form
  expect_false(check("访视日期(SV)", "访视日期", "SV"))
  expect_false(check("肿瘤病史(TMH)", "肿瘤病史", "TMH"))
  expect_false(check("签署知情同意书(ICF)", "签署知情同意书", "ICF"))
  expect_true(check("血常规(LB)", "血常规", "LB"))
  expect_true(check("生命体征(VS)", "生命体征", "VS"))
  expect_true(check("PK血液采血(PK)", "PK血液采血", "PK"))
  expect_true(check("计划外PK采血(PKUP)", "计划外PK采血", "PKUP"))
})

test_that("normalize uses AI Chinese TESTCAT when FORM is English code", {
  mock <- list(
    visitcode = data.frame(
      VISIT = "筛选期- D-28~D-1",
      VISITNUM = 101,
      WP = "",
      CYCLE = "",
      VISITDAY = "",
      stringsAsFactors = FALSE
    ),
    testconfig = data.frame(
      TESTCAT = "血常规",
      VISITNUM = "101",
      FORM = "LB",
      TBNAME = "LB",
      stringsAsFactors = FALSE
    ),
    notes = "test"
  )
  out <- pdchecker:::normalize_als_config(mock, exclude_visits = NULL, exclude_forms = NULL)
  expect_equal(out$testconfig$TESTCAT, "血常规")
})

test_that("normalize_als_config drops non-check forms and English TESTCAT", {
  mock <- list(
    visitcode = data.frame(
      VISIT = c("筛选期- D-28~D-1", "维持治疗期_C1D8"),
      VISITNUM = c(101, 301),
      WP = "",
      CYCLE = "",
      VISITDAY = "",
      stringsAsFactors = FALSE
    ),
    testconfig = data.frame(
      TESTCAT = c("LB", "访视日期", "血常规(LB)"),
      VISITNUM = c("101", "101,301", "101"),
      FORM = c("LB", "访视日期(SV)", "血常规(LB)"),
      TBNAME = c("LB", "SV", "LB"),
      stringsAsFactors = FALSE
    ),
    notes = "test"
  )
  out <- pdchecker:::normalize_als_config(mock, exclude_visits = NULL, exclude_forms = NULL)
  expect_equal(nrow(out$testconfig), 1L)
  expect_equal(out$testconfig$TESTCAT, "血常规")
})
