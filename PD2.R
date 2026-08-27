

# 排除 不需要与知情同意日期比较的数据集
exclude_ds = c("TH","MH","AH","CM","PR")

# 排除 不需要与知情同意日期比较的变量
ic_ignore_vars = c("BRTHDAT")

# 检查 知情同意前进行的试验操作
ic_deviation <- check_icf_time_deviation(data=raw,
                                         ignore_vars = ic_ignore_vars,
                                         exclude_datasets = exclude_ds
                                         )
pd_2_1_1_ouput <- as_check_df(ic_deviation,
                              check_name = "2.1.1 获得ICF前进行了试验相关操作")

# 检查 未签署知情同意书
ic_without <- check_screen_without_ic(raw,
                                      visit_pattern = "筛选|Screening|screening"
                                      )
pd_2_4_1_ouput <- as_check_df(ic_without,
                              check_name = "2.4.1 未签署知情同意书")
