#------------------------------------知情同意相关------------------------------

# 排除遍历数据集
exclude_ds <- c("TH", "MH", "AH", "CM")

# 排除指定日期变量
ic_ignore_vars <- c("BRTHDAT")

# 检查-知情同意之前进行的试验操作
ic_deviation <- check_icf_time_deviation(
  data = raw,
  ignore_vars = ic_ignore_vars,
  exclude_datasets = exclude_ds,
  pdno = "2.1.1"
)

# 检查结果List 转换为 数据框dataframe
pd_2_1_1_ouput <- as_check_df(ic_deviation)


# 检查-未签署知情同意
# visit_pattern 用来指定 筛选期访视
ic_without <- check_screen_without_ic(raw,
  visit_pattern = "筛选|Screening|screening"
)

# 检查结果List 转换为 数据框dataframe
pd_2_4_1_ouput <- as_check_df(ic_without)


#---------------------------------------访视相关--------------------------------------------

# 读取访视编码文件（推荐命名为visitcode，后续函数结果可直接读取环境内visitcode数据框）
visitcode <- read_visitcode_file("inst/extdata/example_visitcode.xlsx",
  sheet_name = "QL0911-302"
)

# 产生计划方式日期（visitcode参数自动读取环境内visitcode数据框，其他参数可以有setup中set_pdchecker_options函数统一设置）
plan_svdate <- generate_planned_visit_dates(raw,
  cycle_days = 21
)


# 检查-访视遗漏（输入数据集为上一步产生的计划日期数据框，cutoffdt默认为程序运行当天）
missing_visit <- check_missing_visit(plan_svdate,
  cutoffdt = as.Date("2025-10-31"),
  pdno = "8.2.1"
)

# 检查-访视超长（输入数据集为上一步产生的计划日期数据框）
window_visit <- check_visit_window(plan_svdate,
  pdno = "8.4.1"
)

# 检查结果List 转换为 数据框dataframe
pd_8_2_1_output <- as_check_df(missing_visit)

# 检查结果List 转换为 数据框dataframe
pd_8_4_1_output <- as_check_df(window_visit)


#---------------------------------------检查项相关--------------------------------------------

# 读取检查项文件（推荐命名为testconfig，后续函数结果可直接读取环境内testconfig数据框）
testconfig <- read_testconfig_file(
  file_path = "inst/extdata/example_test.xlsx",
  sheet_name = "QL0911-302"
)

# "血常规 血生化 尿常规 凝血功能" filter_cond筛选入组中的受试者
lb_data <- prepare_test_data(raw,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_result_var = "LBORRES",
  test_yn_var = "LBYN",
  test_cat_var = "LBCAT",
  test_de_var = "LBTEST",
  config_cat = c("血常规", "血生化", "尿常规", "凝血功能"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)

# "大便常规" filter_cond筛选入组中的受试者
lb1_data <- prepare_test_data(raw,
  test_dataset = "LB1",
  test_date_var = "LBDAT",
  test_result_var = "LBORRES",
  test_yn_var = "LBYN",
  test_cat_var = "LBCAT",
  test_de_var = "LBTEST",
  config_cat = c("大便常规"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)

## ----- 对于`宽数据`如果要查具体检查项的缺失，可以先转置为`长数据`，并添加到raw中----
# 再使用prepare_test_data 准备数据
# 例如："生命体征"
raw$VSLONG <- pivot_longer(raw$VS,
  cols = c("VSSBP", "VSDBP", "VSRESP", "VSPUL", "VSTEMP"),
  names_to = "VSTESTCD",
  values_to = "VSORRES"
) %>%
  mutate(VSTEST = case_when(
    VSTESTCD == "VSSBP" ~ "收缩压",
    VSTESTCD == "VSDBP" ~ "舒张压",
    VSTESTCD == "VSRESP" ~ "呼吸",
    VSTESTCD == "VSPUL" ~ "脉搏",
    VSTESTCD == "VSTEMP" ~ "体温"
  ))

vs_data <- prepare_test_data(raw,
  test_dataset = "VSLONG",
  test_date_var = "VSDAT",
  test_result_var = "VSORRES",
  test_yn_var = "VSYN",
  test_cat_var = "TNAME",
  test_de_var = "VSTEST",
  config_cat = c("生命体征"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)


# `宽数据` ----→ `长数据`
# "12导联心电图"
raw$EGLONG <- pivot_longer(raw$EG,
  cols = c("EGHR", "EGPR", "EGQT", "EGQTC"),
  names_to = "EGTESTCD",
  values_to = "EGORRES"
) %>%
  mutate(EGTEST = case_when(
    EGTESTCD == "EGHR" ~ "心率",
    EGTESTCD == "EGPR" ~ "PR间期",
    EGTESTCD == "EGQT" ~ "QT间期",
    EGTESTCD == "EGQTC" ~ "QTc间期"
  ))

eg_data <- prepare_test_data(raw,
  test_dataset = "EGLONG",
  test_date_var = "EGDAT",
  test_result_var = "EGORRES",
  test_yn_var = "EGPERF",
  test_cat_var = "TNAME",
  test_de_var = "EGTEST",
  config_cat = c("12-导联心电图"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)

# "体格检查"
pe_data <- prepare_test_data(raw,
  test_dataset = "PE",
  test_date_var = "PEDAT",
  test_result_var = "PEORRES",
  test_yn_var = "PEPERF",
  test_cat_var = "TNAME",
  test_de_var = "PETEST",
  config_cat = c("体格检查"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)


# "妊娠检查"
# 对于不需要具体检查指标的项目，使test_de_var = NULL
# 同时多个筛选条件使用英文";"连接，例如：DSRAND|DSRANDYN=='是';DM|SEX=='女性'
hcg_data <- prepare_test_data(raw,
  test_dataset = "LBHCG",
  test_date_var = "LBDAT",
  test_result_var = "LBRES",
  test_yn_var = "LBPERF",
  test_cat_var = "TNAME",
  test_de_var = NULL,
  config_cat = c("妊娠试验"),
  filter_cond = "DSRAND|DSRANDYN=='是';DM|SEX=='女性'"
)

# "病毒学检查"
vir_data <- prepare_test_data(raw,
  test_dataset = "LBVT",
  test_date_var = "LBDAT",
  test_result_var = "LBRES",
  test_yn_var = "LBPERF",
  test_cat_var = "TNAME",
  test_de_var = "LBTEST",
  config_cat = c("病毒血清学检查"),
  filter_cond = "DSRAND|DSRANDYN=='是'"
)


# 合并所有需要检查缺失的数据
test_data <- bind_rows(lb_data, lb1_data, vs_data, eg_data, pe_data, hcg_data, vir_data)

# 检查项缺失

# 通过test_var参数指定用来筛选的变量，通过test参数筛选需要对哪些检查项进行检查
# missing_de = TRUE 代表 对`具体指标`进行检查缺失（例如：红细胞计数）
missing_test_1 <- check_missing_test(test_data,
  test_var = "TESTCAT",
  test = c("血常规", "血生化", "尿常规", "凝血功能", "大便常规", "生命体征", "12-导联心电图", "体格检查", "病毒血清学检查"),
  missing_de = TRUE
)

# 通过test_var参数指定用来筛选的变量，通过test参数筛选需要对哪些检查项进行检查
# missing_de = FALSE 代表 不对`具体指标`进行检查缺失
missing_test_2 <- check_missing_test(test_data,
  test_var = "TESTCAT",
  test = c("妊娠试验"),
  missing_de = FALSE
)

pd_8_3_output <- bind_rows(as_check_df(missing_test_1), as_check_df(missing_test_2))


#------------------------------------输出Excel----------------------------------

# 合并所有方案偏离检查结果
# 可选：与方案偏离SPEC merge，并命名输出Excel的变量
all_output <- bind_rows(pd_2_1_1_ouput, pd_2_4_1_ouput, pd_8_2_1_output, pd_8_4_1_output, pd_8_3_output) %>%
  left_join(pdspec, join_by(PDNO == "方案偏离类别编号")) %>%
  mutate(
    `受试者编号` = SUBJID,
    `表单名称` = TBNAME,
    `方案偏离的具体描述` = DESCRIPTION,
    `方案偏离类别编号` = PDNO
  )


# 输出Excel
# 输出的检查结果中必须包括 PDNO,SUBJID,has_deviation 列，用来排序使用，可以不输出到Excel中
# report_cols 参数用来指定输出到Excel的列名
generate_excel_report(
  checks_df   = all_output,
  output_file = "inst/extdata/example_output.xlsx",
  title       = "Study1 - 方案偏离检查报告",
  report_cols = c("受试者编号", "表单名称", "方案偏离分类", "方案偏离具体分类", "方案偏离类别编号", "方案偏离的统一描述", "方案偏离的具体描述", "检查方法", "严重程度")
)
