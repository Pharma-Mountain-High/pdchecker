
#---------------------------------------肿瘤评估访视相关--------------------------------------------
#读取RSCYC编码文件
rscode <- read_visitcode_file("inst/extdata/example_rscyc.xlsx",
  sheet_name = "RSCYC"
)

#衍生 肿瘤评估周期对应的编号(TUCYCN)，与RSCYC.XLSX文件中保持一致即可
#作为新数据集存放到raw中

raw$TU_TL_def <- raw$TU_TL %>%
  mutate(TUCYCN=case_when(
                  grepl("W",TUCYC) ~ 10000+as.numeric(substr(TUCYC,str_locate(TUCYC,"W")+1,length(TUCYC))),
                  grepl("治疗结束",TUCYC) ~ 19999)
  )

raw$TU_NTL_def <- raw$TU_NTL %>%
  mutate(TUCYCN=case_when(
    grepl("W",TUCYC) ~ 10000+as.numeric(substr(TUCYC,str_locate(TUCYC,"W")+1,length(TUCYC))),
    grepl("治疗结束",TUCYC) ~ 19999)
  ) %>%
  filter(TUYN=="是")

# 将 `肿瘤评估数据集`作为`访视数据集`传入 sv_dataset 参数
# 指定评估周期变量(TUCYC)作为sv_visit_var
# 指定衍生的评估周期编号变量(TUCYCN)作为sv_visitnum_var
# 指定评估日期(TUDAT)作为sv_date_var
# 衍生计划日期
rs_plan_tl <- generate_planned_visit_dates(
  raw,
  visitcode = rscode,
  sv_dataset = "TU_TL_def",
  sv_visit_var = "TUCYC",
  sv_visitnum_var = "TUCYCN",
  sv_date_var = "TUDAT"
)

rs_plan_ntl <- generate_planned_visit_dates(
  raw,
  visitcode = rscode,
  sv_dataset = "TU_NTL_def",
  sv_visit_var = "TUCYC",
  sv_visitnum_var = "TUCYCN",
  sv_date_var = "TUDAT"
)

# 检查肿瘤评估周期访视缺失
missing_rs_tl <- check_missing_visit(rs_plan_tl,pdno = "6.1.1")
missing_rs_ntl <- check_missing_visit(rs_plan_ntl,pdno = "6.1.1")

# 检查肿瘤评估周期访视超窗
window_rs_tl <-  check_visit_window(rs_plan_tl,pdno = "6.1.2")
window_rs_ntl <-  check_visit_window(rs_plan_ntl,pdno = "6.1.2")

pd_5_1_1_output <- rbind(as_check_df(missing_rs_tl,check_name = "6.1.1 肿瘤影像学检查缺失"),
                         as_check_df(missing_rs_ntl,check_name = "6.1.1 肿瘤影像学检查缺失")) %>%
  mutate(DESCRIPTION=str_replace(DESCRIPTION,"访视","评估周期"))

pd_5_1_2_output <- rbind(as_check_df(window_rs_tl,check_name = "6.1.2 肿瘤影像学检查超窗"),
                         as_check_df(window_rs_ntl,check_name = "6.1.2 肿瘤影像学检查超窗"))
