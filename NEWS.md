# pdchecker 0.5.0

初始开发版本。

## 数据读取

- 新增 `read_raw_data()`：批量读取 SAS 数据集，支持 IWRS CSV 和 Excel 格式映射。
- 新增 `read_raw_data_with_formats()`：使用 SAS 格式目录（`.sas7bcat`）读取并映射编码。
- 新增 `read_visitcode_file()`：读取访视计划文件，自动解析窗口期（支持 `±Nd`、`<=Nh`、`Nw` 等格式）。
- 新增 `read_testconfig_file()`：读取检查项配置文件，自动展开逗号分隔的 VISITNUM。

## 日期提取

- 新增 `get_first_dose_date()`：提取每位受试者的首次给药日期，支持多数据集。
- 新增 `get_last_dose_date()`：提取末次给药日期，支持给药开始/结束日期。
- 新增 `get_eot_date()`：提取治疗结束日期。
- 新增 `get_eos_date()`：提取研究结束日期。

## 数据准备

- 新增 `generate_planned_visit_dates()`：根据访视计划和临床数据，为每位受试者生成计划访视日期与窗口范围。
- 新增 `prepare_test_data()`：准备和标准化检查项数据，支持受试者筛选条件（`filter_cond`）。

## 方案偏离检查

- 新增 `check_icf_time_deviation()`：检测知情同意签署前执行的研究程序。
- 新增 `check_screen_without_ic()`：识别有筛选访视但缺少知情同意的受试者。
- 新增 `check_missing_visit()`：基于计划访视日期和截止日期检查遗漏访视。
- 新增 `check_visit_window()`：检查已完成访视是否在规定的访视窗口内。
- 新增 `check_missing_test()`：检查每次访视中缺失的检查项目。

## 结果处理与报告

- 新增 `as_check_df()`：将单个检查结果转换为标准化数据框。
- 新增 `combine_check_results()`：合并多个检查结果为统一数据框。
- 新增 `capture_check_results()`：批量运行多个检查函数并合并结果。
- 新增 `generate_excel_report()`：生成包含汇总和明细两个工作表的 Excel 报告。
- 新增 `generate_html_report()`：生成自包含 HTML 格式报告。
- 新增 `generate_markdown_report()`：生成 Markdown 格式报告。

## 全局配置

- 新增 `set_pdchecker_options()` / `get_pdchecker_options()`：统一设置和查看全局参数，避免重复指定数据集名称和变量名。

## 工具函数

- 新增 `is_sas_na()`：判断 SAS 缺失值（`NA`、`"."`、`""`）。

## 文档

- 新增用户手册（vignette），涵盖所有函数的参数说明、使用示例和常见问题。
- 新增示例配置文件（`inst/extdata/example_visitcode.xlsx`、`inst/extdata/example_test.xlsx`）。
