

#------------------------------------知情同意相关------------------------------

#排除遍历数据集
exclude_ds = c("TH","MH","AH","CM")

#排除指定日期变量
ic_ignore_vars = c("BRTHDAT")

# 检查-知情同意之前进行的试验操作
ic_deviation <- check_icf_time_deviation(data=raw,
                                         ignore_vars = ic_ignore_vars,
                                         exclude_datasets = exclude_ds,
                                         pdno = "2.1.1"
)

#检查结果List 转换为 数据框dataframe
pd_2_1_1_ouput <- as_check_df(ic_deviation)


# 检查-未签署知情同意
# visit_pattern 用来指定 筛选期访视
ic_without <- check_screen_without_ic(raw,
                                      visit_pattern = "筛选|Screening|screening"
)

#检查结果List 转换为 数据框dataframe
pd_2_4_1_ouput <- as_check_df(ic_without)



#---------------------------------------访视相关--------------------------------------------

#读取访视编码文件（推荐命名为visitcode，后续函数结果可直接读取环境内visitcode数据框）
visitcode <- read_visitcode_file("inst/extdata/example_visitcode.xlsx",
                                 sheet_name = "QL0911-302")

#产生计划方式日期（visitcode参数自动读取环境内visitcode数据框，其他参数可以有setup中set_pdchecker_options函数统一设置）
plan_svdate <-generate_planned_visit_dates(raw,
                                           cycle_days = 21)


# 检查-访视遗漏（输入数据集为上一步产生的计划日期数据框，cutoffdt默认为程序运行当天）
missing_visit <- check_missing_visit(plan_svdate,
                                     cutoffdt = as.Date("2025-10-31"),
                                     pdno = "8.2.1")

# 检查-访视超长（输入数据集为上一步产生的计划日期数据框）
window_visit <- check_visit_window(plan_svdate,
                                   pdno = "8.4.1")

#检查结果List 转换为 数据框dataframe
pd_8_1_1_output <- as_check_df(missing_visit)

#检查结果List 转换为 数据框dataframe
pd_8_4_1_output <- as_check_df(window_visit)



#---------------------------------------检查项相关--------------------------------------------

testconfig <- read_testconfig_file(file_path = "~/R/QLC7401-303/qlc7401-303-pd-listing/External/example_test.xlsx",
                                   sheet_name = "QLC7401-303")

# "血常规 血生化 尿常规 空腹血脂 凝血功能 甲状腺功能 糖化血红蛋白"
lb_data <- prepare_test_data(raw,
                             test_dataset = "LB",
                             test_date_var = "LBDAT",
                             test_result_var = "LBORRES",
                             test_yn_var = "LBPERF",
                             test_cat_var = "LBCAT",
                             test_de_var = "LBTEST",
                             config_cat = c("血常规","血生化","尿常规","空腹血脂","凝血功能","甲状腺功能","糖化血红蛋白"),
                             filter_cond = "ENROL|RANDYN=='是'"
)


# "生命体征"
vs_data <- prepare_test_data(raw,
                             test_dataset = "VS",
                             test_date_var = "VSDAT",
                             test_result_var = "VSORRES",
                             test_yn_var = "VSPERF",
                             test_cat_var = "TNAME",
                             test_de_var = "VSTEST",
                             config_cat = c("生命体征"),
                             filter_cond = "ENROL|RANDYN=='是'"
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
                             filter_cond = "ENROL|RANDYN=='是'"
)

# "12导联心电图"

raw$EGLONG <- pivot_longer(raw$EG,cols = c("HR","PR","QRS","QT","QTC","QTCF"),
                        names_to = "EGTESTCD",
                        values_to = "EGORRES") %>%
  mutate(EGTEST=case_when(EGTESTCD=="HR" ~ "心率",
                          EGTESTCD=="PR" ~ "PR间期",
                          EGTESTCD=="QRS" ~ "QRS间期",
                          EGTESTCD=="QT" ~ "QT间期",
                          EGTESTCD=="QTC" ~ "QTc间期",
                          EGTESTCD=="QTCF" ~ "QTcF"))

eg_data <- prepare_test_data(raw,
                             test_dataset = "EGLONG",
                             test_date_var = "EGDAT",
                             test_result_var = "EGORRES",
                             test_yn_var = "EGPERF",
                             test_cat_var = "TNAME",
                             test_de_var = "EGTEST",
                             config_cat = c("12导联心电图"),
                             filter_cond = "ENROL|RANDYN=='是'"
)

# "妊娠检查"
rp_data <- prepare_test_data(raw,
                             test_dataset = "RP",
                             test_date_var = "RPDAT",
                             test_result_var = "RPRES",
                             test_yn_var = "RPPERF",
                             test_cat_var = "TNAME",
                             # test_de_var = "RPTEST",
                             config_cat = c("妊娠检查"),
                             filter_cond = "ENROL|RANDYN=='是';DM|SEX=='女'"
)

# "病毒学检查"
vir_data <- prepare_test_data(raw,
                             test_dataset = "VIR",
                             test_date_var = "VIRDAT",
                             test_result_var = "VIRRES",
                             test_yn_var = "VIRPERF",
                             test_cat_var = "TNAME",
                             # test_de_var = "VIRTEST",
                             config_cat = c("病毒学检查"),
                             filter_cond = "ENROL|RANDYN=='是'"
)

# "免疫原性样本采集"
ada_data <- prepare_test_data(raw,
                              test_dataset = "ADA",
                              test_date_var = "ADADAT",
                              test_result_var = "ADAPERF",
                              test_yn_var = "ADAPERF",
                              test_cat_var = "TNAME",
                              # test_de_var = "VIRTEST",
                              config_cat = c("ADA样本采集"),
                              filter_cond = "ENROL|RANDYN=='是'"
)

test_data <- bind_rows(lb_data,vs_data,pe_data,eg_data,rp_data,vir_data,ada_data)

missing_test <- check_missing_test(test_data,missing_de = T)

pd_8_2_output <- as_check_df(missing_test)
