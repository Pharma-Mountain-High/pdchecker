

allput <- mget(ls(pattern="_output$")) %>%
  bind_rows() %>%
  left_join(pdspec,join_by(PDNO==`方案偏离类别编号`)) %>%
  left_join(raw$SUBJECT[,c("SUBJID","SITEID")],join_by(SUBJID==SUBJID)) %>%
  mutate(
    `研究中心标识符` = SITEID,
    `受试者编号` = SUBJID,
    `表单名称` = TBNAME,
    `方案偏离的具体描述` = DESCRIPTION,
    `方案偏离类别编号` = PDNO
  )

generate_excel_report(allput,
  output_file = paste0("~/gitlab/qlc7401-303-pd-listing/Output/QLC7401-303方案偏离定义列表_",Sys.Date(),".xlsx"),
  title = "QLC7401-303",
   report_cols = c("研究中心标识符","受试者编号", "表单名称", "方案偏离分类", "方案偏离具体分类",
                    "方案偏离类别编号","方案中的标准或GCP等法规要求",
                    "方案偏离的统一描述", "方案偏离的具体描述", "检查方法", "严重程度")
)


generate_excel_report(allput,
  output_file = paste0("/mnt/Development/Projects02/QLC7401/QLC7401-303/SP/pd/outputs/listings/QLC7401-303方案偏离定义列表_",Sys.Date(),".xlsx"),
  title = "QLC7401-303",
  report_cols = c("研究中心标识符","受试者编号", "表单名称", "方案偏离分类", "方案偏离具体分类",
                  "方案偏离类别编号","方案中的标准或GCP等法规要求",
                  "方案偏离的统一描述", "方案偏离的具体描述", "检查方法", "严重程度")
)

