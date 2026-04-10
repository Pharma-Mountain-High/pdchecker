rm(list = ls())

library(haven)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)
library(devtools)
library(dotenv)

if (!requireNamespace("pdchecker", quietly = TRUE)) {
  devtools::install_github("Pharma-Mountain-High/pdchecker",ref = "devel-qinghuan",
                           dependencies = TRUE,build_vignettes = TRUE,force = TRUE,
                           auth_token = Sys.getenv("GITHUB_TOKEN"))
}

library(pdchecker)

##############################  读取方案偏离定义列表 ###########################
file_path <- "~/STA-Server/Projects02/QLC7401/QLC7401-303/SP/documents/04_PD/QLC7401-303 方案偏离定义列表-定稿版.xlsx"

pdspec <- readxl::read_excel(file_path, sheet = "方案偏离定义列表") %>%
  filter(grepl("编程",`检查方法`)) %>%
  fill(`方案偏离分类`,`方案偏离具体分类`)


######################## 读取 SAS 原始数据（整合为列表格式）####################

raw_path <- "~/STA-Server/Projects02/QLC7401/QLC7401-303/SP/rawdata"

raw <- read_raw_data_with_formats( raw_path,
                                   paste0(raw_path,"/formats.sas7bcat"))

######################## 设置PDCHECKER包使用的参数 ####################

set_pdchecker_options(
  sv_dataset      = "SV",   # 访视数据集名称
  sv_visit_var    = "VISITNAME",   # 访视名称变量
  sv_visitnum_var = "VISITOID",   # 访视编号变量
  sv_date_var     = "VISDAT",   # 访视日期变量
  ex_datasets     = "EX",   # 用药数据集名称
  ex_date_var     = "EXSTDAT",   # 用药开始日期变量
  ex_end_date_var = NULL,   # 用药结束日期变量
  eot_dataset     = "EOT",   # 治疗结束数据集名称
  eot_date_var    = "EOTDAT",   # 治疗结束日期变量
  ds_dataset      = "DS",   # 研究结束数据集名称
  ds_date_var     = "DSDAT",   # 研究结束日期变量
  ic_dataset      = "IC",   # 知情同意数据集名称
  ic_date_var     = "RFICDAT",   # 知情同意日期变量
  tb_name_var     = "TNAME",   # 表名变量
  test_date_var   = NULL,   # 检查日期变量
  test_yn_var     = NULL,   # 检查执行标记变量
  test_result_var = NULL,   # 检查结果变量
  test_cat_var    = NULL,   # 检查类别变量
  test_de_var     = NULL    # 检查指标变量
)

