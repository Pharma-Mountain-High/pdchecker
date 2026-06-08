
rand <- raw$ENROL %>%
  select(SUBJID,RANDYN,RANDDAT)

icf <- raw$IC %>%
  select(SUBJID,RFICDAT)

first_ex <- get_first_dose_date(raw)
#---------------------------------------访视相关--------------------------------------------
visitcode <- read_visitcode_file(visitcode_path,
                                 sheet_name = "QLC7401-303")

plan_svdate <-generate_planned_visit_dates(raw)

missing_visit <- check_missing_visit(plan_svdate, cutoffdt = as.Date("2026-04-24"),pdno = "8.1.1")
window_visit <- check_visit_window(plan_svdate,pdno = "8.4.1")

pd_8_1_1_output <- as_check_df(missing_visit,
                               check_name = "8.1.1 整个访视遗漏")

pd_8_4_1_output <- as_check_df(window_visit,
                               check_name = "8.4.1 整个访视超窗")



#---------------------------------------检查项相关--------------------------------------------

testconfig <- read_testconfig_file(file_path = testconfig_path,
                                   sheet_name = "QLC7401-303")

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


# "生命体征"
vs_data <- prepare_test_data(raw,
                             test_dataset = "VS",
                             test_date_var = "VSDAT",
                             test_time_var = "VSTIM",
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
                             test_time_var = "PETIM",
                             test_result_var = "PERES",
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
                             test_time_var = "EGTIM",
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
                             test_time_var = "RPTIM",
                             test_result_var = "RPRES",
                             test_yn_var = "RPPERF",
                             test_cat_var = "TNAME",
                             # test_de_var = "RPTEST",
                             config_cat = c("妊娠检查"),
                             filter_cond = "ENROL|RANDYN=='是';DM|SEX=='女';DM|RPYN=='是';"
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
                              test_time_var = "ADATIM",
                              test_result_var = "ADAPERF",
                              test_yn_var = "ADAPERF",
                              test_cat_var = "TNAME",
                              # test_de_var = "VIRTEST",
                              config_cat = c("ADA样本采集"),
                              filter_cond = "ENROL|RANDYN=='是'"
)

# "PK样本采集"
pc_data <- prepare_test_data(raw,
                              test_dataset = "PC",
                              test_date_var = "PCDAT",
                              test_time_var = "PCTIM",
                              test_result_var = "PCPERF",
                              test_yn_var = "PCPERF",
                              test_cat_var = "TNAME",
                              # test_de_var = "VIRTEST",
                              config_cat = c("PK血液样本采集"),
                              filter_cond = "ENROL|RANDYN=='是'"
)

test_data <- bind_rows(lb_data,vs_data,pe_data,eg_data,rp_data,vir_data,ada_data)

#D1之前最近检查日期
d1_svdat <- test_data %>%
  filter(VISIT=="V2(D1)") %>%
  select(SUBJID,D1DAT=SVDAT) %>%
  distinct(SUBJID,D1DAT)

before_d1 <- test_data %>%
  left_join(d1_svdat,join_by(SUBJID==SUBJID)) %>%
  filter(TESTDAT<=D1DAT) %>%
  arrange(SUBJID,TESTCAT,desc(TESTDAT)) %>%
  group_by(SUBJID,TESTCAT) %>% slice(1) %>%
  select(SUBJID,TESTCAT,before_d1dat=TESTDAT)

#尿素/尿素氮 只需检查其中一个
urea <- test_data %>%
  filter(grepl("尿素",TESTDE)) %>%
  group_by(SUBJID,VISIT) %>%
  mutate(ureayn = case_when(
    any(!is_sas_na(ORRES)) ~ "是",
    TRUE ~ "否")) %>%
  select(SUBJID,VISIT,TESTCAT,TESTDE,ureayn) %>%
  distinct(SUBJID,VISIT,TESTCAT,ureayn)

missing_test <- check_missing_test(test_data,missing_de = T)

missing_test$details <- missing_test$details %>%
  mutate(PDNO=case_when(grepl("生命体征",test_name) ~ "8.2.1",
                        grepl("体格检查",test_name) ~ "8.2.2",
                        grepl("身高",test_name) ~ "8.2.3",
                        grepl("体重",test_name) ~ "8.2.4",
                        grepl("12导联心电图",test_name) ~ "8.2.5",
                        grepl("血常规",test_name) ~ "8.2.6",
                        grepl("血生化",test_name) ~ "8.2.7",
                        grepl("尿常规",test_name) ~ "8.2.8",
                        grepl("空腹血脂",test_name) ~ "8.2.9",
                        grepl("甲状腺功能",test_name) ~ "8.2.10",
                        grepl("凝血功能",test_name) ~ "8.2.11",
                        grepl("糖化血红蛋白",test_name) ~ "8.2.12",
                        grepl("妊娠检查",test_name) ~ "8.2.13",
                        grepl("病毒学检查",test_name) ~ "8.2.14",
                        grepl("ADA样本采集",test_name) ~ "8.2.15")) %>%
  mutate(TESTCAT=sub("-.*", "", test_name)) %>%
  left_join(before_d1,join_by(SUBJID==SUBJID,TESTCAT==TESTCAT)) %>%
  left_join(urea,join_by(SUBJID==SUBJID,VISIT==VISIT,TESTCAT==TESTCAT)) %>%
  mutate(d_d1_days = visit_date-before_d1dat) %>%
  filter(case_when(
    VISIT=="V2(D1)" & d_d1_days<=7 ~ F,
    grepl("尿素",test_name) & ureayn=="是" ~ F,
    TRUE ~ T
  ))

pd_8_2_output <- as_check_df(missing_test,
                             check_name = "8.2 检查项缺失")


ex_8 <- raw$EX %>%
  select(SUBJID,VISITNAME,VISITOID,EXSTDAT,EXSTTIM)

test_data_y <- test_data %>%
  filter(TESTYN=="是" | !is_sas_na(TESTDAT)) %>%
  left_join(ex_8,join_by(SUBJID==SUBJID,VISIT==VISITNAME)) %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  left_join(first_ex,join_by(SUBJID==SUBJID)) %>%
  select(SUBJID,RFICDAT,VISIT,VISITNUM,SVDAT,TNAME,TESTCAT,TESTDAT,TESTTIM,EXSTDAT,EXSTTIM,first_dose_date) %>%
  unite(col = "TESTDTMC",TESTDAT,TESTTIM, sep = " ", na.rm = TRUE,remove = F) %>%
  unite(col = "EXSTDTMC",EXSTDAT,EXSTTIM, sep = " ", na.rm = TRUE,remove = F) %>%
  mutate(TESTDTM=as.POSIXct(TESTDTMC,format= "%Y-%m-%d %H:%M:%S"),
         EXSTDTM=as.POSIXct(EXSTDTMC,format= "%Y-%m-%d %H:%M:%S"),
         d_day=case_when(VISITNUM %in% c("1") ~ first_dose_date-TESTDAT,
                         TRUE ~ TESTDAT-SVDAT),
         bexyn=ifelse(TESTDTM<EXSTDTM,T,F)) %>%
  mutate(wp=case_when(
    VISITNUM %in% c("2","5","11") ~ "0",
    VISITNUM %in% c("3","4","6","7","8") ~ "3",
    VISITNUM %in% c("9","10","12","13","14") ~ "7",
    VISITNUM %in% c("1") ~ "14"
  ))

pd_8_5 <- list()
pd_8_5_muti <- test_data_y %>%
  # filter(TESTCAT=="生命体征") %>%
  filter(case_when(
    VISITNUM %in% c("2","5","11") & (bexyn==F | abs(d_day!=0)) ~ T,
    VISITNUM %in% c("3","4","6","7","8") & abs(d_day)>3 ~T,
    VISITNUM %in% c("9","10","12","13","14") & abs(d_day)>7 ~T,
    VISITNUM %in% c("1") & abs(d_day) > 14 ~T
    )) %>%
  group_by(SUBJID,VISIT,TESTCAT,TESTDAT) %>% slice(1) %>%
  mutate(PDNO=case_when(grepl("生命体征",TESTCAT) ~ "8.5.1",
                        grepl("体格检查",TESTCAT) ~ "8.5.2",
                        grepl("身高",TESTCAT) ~ "8.5.3",
                        grepl("体重",TESTCAT) ~ "8.5.4",
                        grepl("12导联心电图",TESTCAT) ~ "8.5.5",
                        grepl("血常规",TESTCAT) ~ "8.5.6",
                        grepl("血生化",TESTCAT) ~ "8.5.7",
                        grepl("尿常规",TESTCAT) ~ "8.5.8",
                        grepl("空腹血脂",TESTCAT) ~ "8.5.9",
                        grepl("甲状腺功能",TESTCAT) ~ "8.5.10",
                        grepl("凝血功能",TESTCAT) ~ "8.5.11",
                        grepl("糖化血红蛋白",TESTCAT) ~ "8.5.12",
                        grepl("妊娠检查",TESTCAT) ~ "8.5.13",
                        grepl("病毒学检查",TESTCAT) ~ "8.5.14",
                        grepl("ADA样本采集",TESTCAT) ~ "8.5.16")) %>%
  mutate(TBNAME=TNAME,
         DESCRIPTION=case_when(
           VISITNUM %in% c("2","5","11") ~ sprintf("受试者编号%s，首次知情同意在%s签署，在访视%s[%s]，给药时间：%s，%s检查计划在给药当天给药前进行，实际发生在%s。",
                             SUBJID,
                             RFICDAT,
                             VISIT,
                             SVDAT,
                             EXSTDTMC,
                             TESTCAT,
                             TESTDTMC),
           TRUE ~ sprintf("受试者编号%s，首次知情同意在%s签署，在访视%s[%s]，%s检查时间：%s，偏离访视日期%d天，不在窗口期%s内。",
                          SUBJID,
                          RFICDAT,
                          VISIT,
                          SVDAT,
                          TESTCAT,
                          TESTDAT,
                          abs(d_day),
                          wp)
           )
  )

test_pc_y <- pc_data %>%
  filter(TESTYN=="是" | !is_sas_na(TESTDAT)) %>%
  left_join(ex_8,join_by(SUBJID==SUBJID,VISIT==VISITNAME)) %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  left_join(first_ex,join_by(SUBJID==SUBJID)) %>%
  select(SUBJID,RFICDAT,VISIT,VISITNUM,SVDAT,TNAME,TESTCAT,CSN,PCTPT,TESTDAT,TESTTIM,EXSTDAT,EXSTTIM,first_dose_date) %>%
  unite(col = "TESTDTMC",TESTDAT,TESTTIM, sep = " ", na.rm = TRUE,remove = F) %>%
  unite(col = "EXSTDTMC",EXSTDAT,EXSTTIM, sep = " ", na.rm = TRUE,remove = F) %>%
  mutate(TESTDTM=as.POSIXct(TESTDTMC,format= "%Y-%m-%d %H:%M:%S"),
         EXSTDTM=as.POSIXct(EXSTDTMC,format= "%Y-%m-%d %H:%M:%S"),
         dtm_ex=round(difftime(TESTDTM,EXSTDTM, units = "hours"),2)) %>%
  mutate(wp=case_when(
    CSN == 1 ~ "2~4h",
    CSN == 2 ~ "4~24h"
  ))

pd_8_5_15 <- test_pc_y %>%
  filter(case_when(
    CSN == 1 & (dtm_ex<2 | dtm_ex>4)  ~ T,
    CSN == 2 & (dtm_ex<4 | dtm_ex>24)  ~ T
  )) %>%
  mutate(PDNO="8.5.15",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，在访视%s[%s]，给药时间：%s，%s检查计划在%s进行，实际发生在%s，与给药时间间隔%s小时，不在窗口期内。",
                             SUBJID,
                             RFICDAT,
                             VISIT,
                             SVDAT,
                             EXSTDTMC,
                             TESTCAT,
                             PCTPT,
                             TESTDTMC,
                             dtm_ex)
  )

pd_8_5$details <- bind_rows(pd_8_5_muti,pd_8_5_15)

pd_8_5$has_deviation <- ifelse(nrow(pd_8_5$details)>0,TRUE,FALSE)

pd_8_5_output <- as_check_df(pd_8_5,
                             check_name = "8.5 检查项超窗")

