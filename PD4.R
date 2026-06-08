
ex <- raw$EX %>%
  select(SUBJID,VISITNAME,VISITOID,EXSTDAT,TNAME,EXREAS,EXROTH,EXDSTXT)

sv <- raw$SV %>%
  filter(SVOCCUR=="是") %>%
  select(SUBJID,VISITNAME,VISDAT)

rand <- raw$ENROL %>%
  select(SUBJID,RANDYN,RANDDAT)

first_ex <- get_first_dose_date(raw)

ex_plan <- sv %>%
  left_join(ex,join_by(SUBJID==SUBJID, VISITNAME==VISITNAME)) %>%
  left_join(first_ex,join_by(SUBJID==SUBJID)) %>%
  left_join(visitcode,join_by(VISITNAME==VISIT)) %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  filter(grepl("D",VISITNAME)) %>%
  mutate(plan_date = first_dose_date+ days(as.numeric(VISITDAY)-1)) %>%
  fill(TNAME)

pd_4_3_1 <- list()

pd_4_3_1$details <- ex_plan %>%
  mutate(d_day=abs(plan_date-as.Date(EXSTDAT))) %>%
  filter(case_when(
    grepl("D1",VISITNAME) & d_day!=0 ~ T,
    grepl("D90",VISITNAME) & d_day>3~ T,
    grepl("D270",VISITNAME) & d_day>7 ~ T,
    TRUE ~ F
  )) %>%
  mutate(PDNO="4.3.1",
         VISIT=VISITNAME,
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，在访视%s（%s），计划给药日期为%d，实际给药日期为%s，偏离%s天，不在窗口期内。",
                             SUBJID,
                             VISIT,
                             VISDAT,
                             plan_date,
                             EXSTDAT,
                             d_day))

pd_4_3_1$has_deviation <- ifelse(nrow(pd_4_3_1$details)>0,TRUE,FALSE)

pd_4_3_1_output <- as_check_df(pd_4_3_1,
                               check_name = "4.3.1 受试者超过时间窗要求用药")

pd_4_3_2 <- list()

pd_4_3_2$details <- ex_plan %>%
  filter(RANDYN == "是") %>%
  filter( grepl("D1|D90|D270",VISITNAME)) %>%
  filter(is.na(EXSTDAT)) %>%
  mutate(PDNO="4.3.2",
         VISIT=VISITNAME,
         TBNAME=TNAME,
         reaall=str_c(EXREAS,EXROTH,sep = "：",na.rm=T),
         DESCRIPTION=sprintf("受试者编号%s，因[%s]，在访视%s（%s），未用药。",
                             SUBJID,
                             reaall,
                             VISIT,
                             VISDAT))

pd_4_3_2$has_deviation <- ifelse(nrow(pd_4_3_2$details)>0,TRUE,FALSE)

pd_4_3_2_output <- as_check_df(pd_4_3_2,
                               check_name = "4.3.2 受试者未用药")

pd_4_4_1 <- list()

pd_4_4_1$details <- ex_plan %>%
  filter(!is.na(EXSTDAT) & EXDSTXT!="1.5") %>%
  mutate(PDNO="4.4.1",
         VISIT=VISITNAME,
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，在访视%s（%s），计划给药量为1.5mL，实际给药量为%smL。",
                             SUBJID,
                             VISIT,
                             VISDAT,
                             EXDSTXT))

pd_4_4_1$has_deviation <- ifelse(nrow(pd_4_4_1$details)>0,TRUE,FALSE)

pd_4_4_1_output <- as_check_df(pd_4_4_1,
                               check_name = "4.4.1 药物剂量出现错误")
