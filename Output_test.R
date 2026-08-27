
# 合并 所有_output结尾 Dateframe 
allput <- mget(ls(pattern="_output$")) %>%
  bind_rows() 

# 输出 Excel
generate_excel_report(allput,
  output_file = paste0("~/outputs/QLC7401-303方案偏离定义列表_",Sys.Date(),".xlsx"),
  title = "QLC7401-303",
  report_cols = c("研究中心标识符","受试者编号", "表单名称", "方案偏离分类", "方案偏离具体分类",
                  "方案偏离类别编号","方案中的标准或GCP等法规要求",
                  "方案偏离的统一描述", "方案偏离的具体描述", "检查方法", "严重程度")
)

