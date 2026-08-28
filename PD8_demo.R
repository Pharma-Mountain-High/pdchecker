# 读取 检查项配置文件
testconfig <- read_testconfig_file(file_path = testconfig_path,
                                   sheet_name = "QLC7401-303")

# 准备 检查项数据集
# "血常规 血生化 尿常规 空腹血脂 凝血功能 甲状腺功能 糖化血红蛋白"
lb_data <- prepare_test_data(raw,
                             test_dataset = "LB",
                             test_date_var = "LBDAT",
                             test_time_var = "LBTIM",
                             test_result_var = "LBORRES",
                             test_yn_var = "LBPERF",
                             test_cat_var = "LBCAT",
                             test_de_var = "LBTEST",
                             config_cat = c("血常规","血生化","尿常规","空腹血脂","凝血功能","甲状腺功能","糖化血红蛋白"),
                             filter_cond = "ENROL|RANDYN=='是'"
)

# 检查 检查项缺失
missing_test <- check_missing_test(lb_data,missing_de = T)

pd_8_2_output <- as_check_df(missing_test,
                             check_name = "8.2 检查项缺失")