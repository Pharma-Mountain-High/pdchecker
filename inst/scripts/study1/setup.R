rm(list = ls())

if (!requireNamespace("pdchecker", quietly = TRUE)) {
  devtools::install_github("Pharma-Mountain-High/pdchecker",build_vignettes = TRUE,force = TRUE)
}

library(dplyr)
library(tidyr)
library(readxl)
library(pdchecker)

#------------------------------读取方案偏离定义列表-------------------------
file_path <- "~/STA-Server/Projects02/QL0911/QL0911-302/SP/documents/05 PD Listing/QL0911-302_方案偏离定义列表-final-2024-12-27-wqh_yi.xlsx"

pdspec <- read_excel(file_path, sheet = "方案偏离定义列表") %>%
  filter(grepl("编程",`检查方法`)) %>%
  fill(`方案偏离分类`,`方案偏离具体分类`)


#------------------------------读取 SAS 原始数据-------------------------------

raw_path <- "~/STA-Server/Projects02/QL0911/QL0911-302/SP/rawdata"

raw <- read_raw_data_with_formats( raw_path,
                                   paste0(raw_path,"/formats.sas7bcat"))

#-----------------------------统一设置PDCHECKER包使用的参数-------------------------

set_pdchecker_options(
  sv_dataset      = "SV",   # 访视数据集名称
  sv_visit_var    = "VISITNAME",   # 访视名称变量
  sv_visitnum_var = "VISITOID",   # 访视编号变量
  sv_date_var     = "SVDAT",   # 访视日期变量
  ex_datasets     = c("EXA","EXB"),   # 用药数据集名称（可多个数据集）
  ex_date_var     = "EXDAT",   # 用药开始日期变量
  ex_end_date_var = NULL,   # 用药结束日期变量（可为空）
  eot_dataset     = "EOT",   # 治疗结束数据集名称
  eot_date_var    = "EOTENDAT",   # 治疗结束日期变量
  ds_dataset      = "DS",   # 研究结束数据集名称
  ds_date_var     = "DSDAT",   # 研究结束日期变量
  ic_dataset      = "ICF",   # 知情同意数据集名称
  ic_date_var     = "ICFDAT",   # 知情同意日期变量
  tb_name_var     = "TNAME",   # 表名变量
)

