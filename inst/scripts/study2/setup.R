rm(list = ls())

library(haven)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(devtools)
library(dotenv)
library(stringr)

if (!requireNamespace("pdchecker", quietly = TRUE)) {
  devtools::install_github("Pharma-Mountain-High/pdchecker",build_vignettes = TRUE,force = TRUE)
}

library(pdchecker)
#------------------------------读取 SAS 原始数据-------------------------------

# Posit
raw_path <- "/mnt/Development/Projects02/QLC1401/QLC1401-201/SP/rawdata"

raw <- read_raw_data_with_formats( raw_path,
                                   paste0(raw_path,"/formats.sas7bcat"))

#-----------------------------统一设置PDCHECKER包使用的参数-------------------------

set_pdchecker_options(
  sv_dataset      = "SV",   # 访视数据集名称
  sv_visit_var    = "VISITNAME",   # 访视名称变量
  sv_visitnum_var = "VISITOID",   # 访视编号变量
  sv_date_var     = "VISDAT",   # 访视日期变量
  ex_datasets     = c("EX","EX2"),   # 用药数据集名称
  ex_date_var     = "EXSTDAT",   # 用药开始日期变量
  ex_end_date_var = "EXENDAT",   # 用药结束日期变量
  eot_dataset     = "EOT",   # 治疗结束数据集名称
  eot_date_var    = "EOTDAT",   # 治疗结束日期变量
  ds_dataset      = "DS",   # 研究结束数据集名称
  ds_date_var     = "DSDAT",   # 研究结束日期变量
  ic_dataset      = "IC",   # 知情同意数据集名称
  ic_date_var     = "ICDAT",   # 知情同意日期变量
  tb_name_var     = "TNAME",   # 表名变量
  test_date_var   = NULL,   # 检查日期变量
  test_yn_var     = NULL,   # 检查执行标记变量
  test_result_var = NULL,   # 检查结果变量
  test_cat_var    = NULL,   # 检查类别变量
  test_de_var     = NULL    # 检查指标变量
)

