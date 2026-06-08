
testconfig <- read_testconfig_file(file_path = testconfig_path,
                                   sheet_name = "QLC7401-303")

# "血脂样本采集"
bl_data <- prepare_test_data(raw,
                             test_dataset = "BL",
                             test_date_var = "BLDAT",
                             test_time_var = "BLTIM",
                             test_result_var = "BLDAT",
                             test_yn_var = "BLPERF",
                             test_cat_var = "TNAME",
                             test_de_var = "",
                             config_cat = c("血脂样本采集"),
                             filter_cond = "ENROL|RANDYN=='是'"
)

# PCSK9 样本采集
pcsk_data <- prepare_test_data(raw,
                             test_dataset = "PCSK",
                             test_date_var = "PCSKDAT",
                             test_time_var = "PCSKTIM",
                             test_result_var = "PCSKDAT",
                             test_yn_var = "PCSKPERF",
                             test_cat_var = "TNAME",
                             test_de_var = "",
                             config_cat = c("PCSK9样本采集"),
                             filter_cond = "ENROL|RANDYN=='是'"
)

#-----------------------PD 6.1.1 -------------------
# D330
bl_d330 <- bl_data %>%
  filter(grepl("D330",VISIT))


bld330_missing <- check_missing_test(bl_d330,pdno = "6.1.1")

pd_6_1_1_output <- as_check_df(bld330_missing,
                               check_name = "6.1.1 主要疗效终点样本采集缺失")

#-----------------------PD 6.1.2 -------------------
# D330
bl_oth <- bl_data %>%
  filter(!grepl("D330",VISIT))

pcsk_oth <- pcsk_data %>%
  filter(!grepl("D330",VISIT))

blpc_missing <- check_missing_test(bind_rows(bl_oth,pcsk_oth),pdno = "6.1.2")

pd_6_1_2_output <- as_check_df(blpc_missing,
                               check_name = "6.1.2 次要疗效终点样本采集缺失")

#----------------------PD 6.1.3------------------------------
rand <- raw$ENROL %>%
  select(SUBJID,RANDYN,RANDDAT)

icf <- raw$IC %>%
  select(SUBJID,RFICDAT)

first_ex <- get_first_dose_date(raw)

ex_6 <- raw$EX %>%
  select(SUBJID,VISITNAME,VISITOID,EXSTDAT,EXSTTIM)

blpc_data_y <- bind_rows(bl_data,pcsk_data) %>%
  filter(TESTYN=="是" | !is_sas_na(TESTDAT)) %>%
  left_join(ex_6,join_by(SUBJID==SUBJID,VISIT==VISITNAME)) %>%
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

pd_6_1_3 <- list()
pd_6_1_3$details <- blpc_data_y %>%
  filter(case_when(
    VISITNUM %in% c("2","5","11") & (bexyn==F | abs(d_day!=0)) ~ T,
    VISITNUM %in% c("3","4","6","7","8") & abs(d_day)>3 ~T,
    VISITNUM %in% c("9","10","12","13","14") & abs(d_day)>7 ~T,
    VISITNUM %in% c("1") & abs(d_day) > 14 ~T
  )) %>%
  group_by(SUBJID,VISIT,TESTCAT,TESTDAT) %>% slice(1) %>%
  mutate(PDNO="6.1.3",
         TBNAME=TNAME,
         DESCRIPTION=case_when(
           VISITNUM %in% c("2","5","11") ~ sprintf("受试者编号%s，首次知情同意在%s签署，在访视%s[%s]，给药时间：%s，%s计划在给药当天给药前进行，实际发生在%s。",
                                                   SUBJID,
                                                   RFICDAT,
                                                   VISIT,
                                                   SVDAT,
                                                   EXSTDTMC,
                                                   TESTCAT,
                                                   TESTDTMC),
           TRUE ~ sprintf("受试者编号%s，首次知情同意在%s签署，在访视%s[%s]，%s时间：%s，偏离访视日期%d天，不在窗口期%s内。",
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
pd_6_1_3$has_deviation <- ifelse(nrow(pd_6_1_3$details)>0,TRUE,FALSE)

pd_6_1_3_output <- as_check_df(pd_6_1_3,
                             check_name = "6.1.3 疗效终点样本采集超窗")
