# =============================================================================
# 项目：XXXX 研究
# 用途：方案偏离自动检查
# 作者：
# 日期：
# =============================================================================

library(pdchecker)

# ── 路径配置（修改为实际路径）────────────────────────────────────────────────
DATA_DIR     <- "D:/projects/XXXX研究/data/sas"
CATALOG_FILE <- "D:/projects/XXXX研究/data/sas/formats.sas7bcat"
VISIT_FILE   <- "D:/projects/XXXX研究/config/visit_schedule.xlsx"
TEST_FILE    <- "D:/projects/XXXX研究/config/test_config.xlsx"
OUTPUT_FILE  <- "D:/projects/XXXX研究/output/deviation_report.xlsx"
CUTOFF_DATE  <- as.Date("2025-12-31")

# ── 第一步：读取配置文件 ──────────────────────────────────────────────────────
visitcode  <- read_visitcode_file(VISIT_FILE)
testconfig <- read_testconfig_file(TEST_FILE, visitcode = visitcode)

# ── 第二步：读取临床数据 ──────────────────────────────────────────────────────
data <- read_raw_data_with_formats(
  data_dir     = DATA_DIR,
  catalog_file = CATALOG_FILE,
  encoding     = "UTF-8"     # 中文数据可能需要 "GBK"
)

# ── 第三步：设置全局选项（根据项目实际变量名调整）────────────────────────────
set_pdchecker_options(
  sv_dataset      = "SV",
  sv_visit_var    = "VISIT",
  sv_visitnum_var = "VISITNUM",
  sv_date_var     = "SVSTDTC",   # ← 根据实际变量名修改
  ex_datasets     = c("EC"),     # ← 根据实际数据集名修改
  ex_date_var     = "ECSTDTC",
  ic_dataset      = "IC",
  ic_date_var     = "ICDTC",
  test_date_var   = "LBDTC",
  test_cat_var    = "LBCAT"
)

# ── 第四步：生成计划访视日期 ──────────────────────────────────────────────────
planned_dates <- generate_planned_visit_dates(data = data, cycle_days = 28)

# ── 第五步：准备检查项数据 ────────────────────────────────────────────────────
prepared_lb <- prepare_test_data(data = data, test_dataset = "LB")

prepared_vs <- prepare_test_data(
  data          = data,
  test_dataset  = "VS",
  test_date_var = "VSDAT",
  test_cat_var  = "VSCAT"
)

prepared_eg <- prepare_test_data(
  data          = data,
  test_dataset  = "EG",
  test_date_var = "EGDAT",
  test_cat_var  = "EGCAT"
)

# ── 第六步：执行各项检查 ──────────────────────────────────────────────────────
res_icf <- check_icf_time_deviation(
  data,
  ignore_vars = c("BRTHDAT", "MHSTDAT")
)

res_ic <- check_screen_without_ic(
  data,
  visit_pattern = "Screening|screening|筛选"
)

res_mv <- check_missing_visit(planned_dates, cutoffdt = CUTOFF_DATE)

res_vw <- check_visit_window(planned_dates)

res_lb <- check_missing_test(data = prepared_lb)
res_vs <- check_missing_test(data = prepared_vs)
res_eg <- check_missing_test(data = prepared_eg)

# ── 第七步：合并所有结果 ──────────────────────────────────────────────────────
all_results <- combine_check_results(
  res_icf,
  res_ic,
  res_mv,
  res_vw,
  res_lb,
  res_vs,
  res_eg
)

# ── 第八步：生成 Excel 报告 ───────────────────────────────────────────────────
generate_excel_report(
  checks_df   = all_results,
  output_file = OUTPUT_FILE,
  title       = "XXXX研究 - 方案偏离检查报告"
)

message("✓ 报告已生成：", OUTPUT_FILE)
