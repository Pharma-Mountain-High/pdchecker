
icf <- raw$IC %>%
  select(SUBJID,RFICDAT)

first_ex <- get_first_dose_date(raw)

rand <- raw$ENROL %>%
  select(SUBJID,RANDYN,RANDDAT,RANDLDL)

ldlc <- raw$LB %>%
  filter(grepl("LDL-C",LBTEST)) %>%
  left_join(first_ex,join_by(SUBJID==SUBJID)) %>%
  select(SUBJID,TNAME,VISITNAME,LBDAT,GFR,LBTEST,LBORRES,LBORRESU,first_dose_date) %>%
  filter(LBDAT<=first_dose_date) %>%
  group_by(SUBJID) %>% arrange(SUBJID,desc(LBDAT)) %>% slice(1) %>%
  # filter(VISITNAME=="筛选期V1") %>%
  mutate(EDCLDL = case_when(
    as.numeric(LBORRES)>=3.4 ~ "≥3.4 mmol/L",
    as.numeric(LBORRES)<3.4 ~ "＜3.4 mmol/L"
  ))


pd_1_1_1 <- list()

pd_1_1_1$details <- ldlc %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是") %>%
  filter(case_when(
    RANDLDL!=EDCLDL ~ T,
    TRUE ~ F
  )) %>%
  mutate(PDNO="1.1.1",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，首次给药日期%s，于%s(%s)检测LDL-C结果为%s mmol/L，但随机时分层因素“基线LDL-C水平”选择“%s”。",
                             SUBJID,
                             RFICDAT,
                             first_dose_date,
                             VISITNAME,
                             LBDAT,
                             LBORRES,
                             RANDLDL))
pd_1_1_1$has_deviation <- ifelse(nrow(pd_1_1_1$details)>0,TRUE,FALSE)

pd_1_1_1_output <- as_check_df(pd_1_1_1,
                               check_name = "1.1 随机分层因素出现错误")

