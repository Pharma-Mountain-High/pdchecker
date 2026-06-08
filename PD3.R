

dm <- raw$DM %>%
  select(SUBJID,AGE,SEX,BRTHDAT,TNAME)

subject <- raw$SUBJECT %>%
  select(SUBJID,STATUS)

rand <- raw$ENROL %>%
  select(SUBJID,RANDYN,RANDDAT)

icf <- raw$IC %>%
  select(SUBJID,RFICDAT)

lbdata3 <- raw$LB %>%
  filter(grepl("TG|LDL-C|ALT|AST|TBIL",LBTEST) & VISITNAME == "筛选期V1") %>%
  select(SUBJID,TNAME,LBDAT,GFR,LBTEST,LBORRES,LBORRESU,LBORNRLO,LBORNRHI)

ascv <- raw$ASCV %>%
  select(SUBJID,TNAME,ASCVDAT,ASCVRES)

mh <- raw$MH %>%
  select(SUBJID,TNAME,MHTERM,MHSTDAT)


#-------------------3.1.1----------------------------
pd_3_1_1 <- list()

pd_3_1_1$details <- dm %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN == "是") %>%
  filter(AGE<18) %>%
  mutate(PDNO="3.1.1",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，出生日期为%s，年龄为%s岁，不符合入组标准第1条。",
                             SUBJID,
                             RFICDAT,
                             BRTHDAT,
                             AGE))
pd_3_1_1$has_deviation <- ifelse(nrow(pd_3_1_1$details)>0,TRUE,FALSE)

pd_3_1_1_output <- as_check_df(pd_3_1_1,
                               check_name = "3.1 不符合入选标准却入组")

#---------------------------3.1.2------------------------------------

pd_3_1_2 <- list()

pd_3_1_2$details <- lbdata3 %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是" & LBTEST=="低密度脂蛋白胆固醇(LDL-C)") %>%
  filter(as.numeric(LBORRES)<2.6 | as.numeric(LBORRES)>=4.9) %>%
  mutate(PDNO="3.1.2",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s检测LDL-C结果为%smmol/L，不满足入组标准第2条。",
                             SUBJID,
                             RFICDAT,
                             LBDAT,
                             LBORRES))
pd_3_1_2$has_deviation <- ifelse(nrow(pd_3_1_2$details)>0,TRUE,FALSE)

pd_3_1_2_output <- as_check_df(pd_3_1_2,
                               check_name = "3.1 不符合入选标准却入组")


#---------------------------3.1.3------------------------------------

pd_3_1_3 <- list()

pd_3_1_3$details <- lbdata3 %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是" & LBTEST=="甘油三酯(TG)") %>%
  filter(case_when(
    LBORRESU=="mg/dL" & as.numeric(LBORRES)>400 ~ T,
    LBORRESU=="mmol/L" & as.numeric(LBORRES)>4.5 ~ T,
    is_sas_na(LBORRESU) & as.numeric(LBORRES)>4.5 ~ T,
    TRUE ~ F
  )) %>%
  mutate(PDNO="3.1.3",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s检测TG结果为%s %s，不满足入组标准第3条。",
                             SUBJID,
                             RFICDAT,
                             LBDAT,
                             LBORRES,
                             LBORRESU))
pd_3_1_3$has_deviation <- ifelse(nrow(pd_3_1_3$details)>0,TRUE,FALSE)

pd_3_1_3_output <- as_check_df(pd_3_1_3,
                               check_name = "3.1 不符合入选标准却入组")

#---------------------------3.1.4------------------------------------

pd_3_1_4 <- list()

pd_3_1_4$details <- ascv %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是") %>%
  filter(ASCVRES=="高危及以上") %>%
  mutate(PDNO="3.1.4",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s进行的ASCVD风险等级评估结果为%s，不满足入组标准第4条。",
                             SUBJID,
                             RFICDAT,
                             ASCVDAT,
                             ASCVRES))
pd_3_1_4$has_deviation <- ifelse(nrow(pd_3_1_4$details)>0,TRUE,FALSE)

pd_3_1_4_output <- as_check_df(pd_3_1_4,
                               check_name = "3.1 不符合入选标准却入组")

#---------------------------3.1.6------------------------------------

pd_3_1_6 <- list()

pd_3_1_6$details <- lbdata3 %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是") %>%
  arrange(SUBJID,desc(GFR)) %>% group_by(SUBJID) %>% slice(1) %>%
  mutate(GFRRES=ifelse(is_sas_na(GFR),"空",GFR)) %>%
  filter(as.numeric(GFR)<=30 | is_sas_na(GFR)) %>%
  group_by(SUBJID) %>% slice(1) %>%
  mutate(PDNO="3.1.6",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s进行的肾小球滤过率结果为%s，不满足入组标准第6条。",
                             SUBJID,
                             RFICDAT,
                             LBDAT,
                             GFRRES))
pd_3_1_6$has_deviation <- ifelse(nrow(pd_3_1_6$details)>0,TRUE,FALSE)

pd_3_1_6_output <- as_check_df(pd_3_1_6,
                               check_name = "3.1 不符合入选标准却入组")

#---------------------------3.2.1-1------------------------------------

pd_3_2_1 <- list()

pd_3_2_1$details <- mh %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是") %>%
  filter(grepl("冠脉综合征|心肌病|稳定性冠心病|冠状动脉|血运重建|卒中|脑缺血|外周动脉疾病",MHTERM)) %>%
  filter(MHSTDAT<RFICDAT) %>%
  mutate(PDNO="3.2.1-1",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s出现[%s]，符合排除标准第1条。",
                             SUBJID,
                             RFICDAT,
                             MHSTDAT,
                             MHTERM))
pd_3_2_1$has_deviation <- ifelse(nrow(pd_3_2_1$details)>0,TRUE,FALSE)

pd_3_2_1_output <- as_check_df(pd_3_2_1,
                               check_name = "3.2 符合排除标准却入组")


#---------------------------3.2.4-1------------------------------------

pd_3_2_4 <- list()

pd_3_2_4$details <- lbdata3 %>%
  left_join(rand,join_by(SUBJID==SUBJID)) %>%
  left_join(icf,join_by(SUBJID==SUBJID)) %>%
  filter(RANDYN=="是") %>%
  mutate(ULN=ifelse(is_sas_na(LBORNRHI),"空",LBORNRHI)) %>%
  filter(grepl("ALT|AST|TBIL",LBTEST)) %>%
  filter(case_when(
    grepl("ALT",LBTEST) & as.numeric(LBORRES)>3*as.numeric(LBORNRHI) ~ T,
    grepl("AST",LBTEST) & as.numeric(LBORRES)>3*as.numeric(LBORNRHI) ~ T,
    grepl("TBIL",LBTEST) & as.numeric(LBORRES)>2*as.numeric(LBORNRHI) ~ T,
    is_sas_na(LBORNRHI) ~ T,
    TRUE ~ F
  )) %>%
  mutate(PDNO="3.2.4-1",
         TBNAME=TNAME,
         DESCRIPTION=sprintf("受试者编号%s，首次知情同意在%s签署，于%s进行的%s检查结果为%s（ULN：%s），符合排除标准第4条。",
                             SUBJID,
                             RFICDAT,
                             LBDAT,
                             LBTEST,
                             LBORRES,
                             ULN))
pd_3_2_4$has_deviation <- ifelse(nrow(pd_3_2_4$details)>0,TRUE,FALSE)

pd_3_2_4_output <- as_check_df(pd_3_2_4,
                               check_name = "3.2 符合排除标准却入组")
