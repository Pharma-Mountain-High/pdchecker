# pdchecker 1.0.0

## 新功能

### 检查项超窗检查

新增检查项超窗（`check_test_window()`）功能链，用于检测受试者在访视中执行的检查项是否超出方案规定的窗口期。该功能由以下四个函数组成：

- **`read_testwp_file()`**：读取检查项窗口配置文件。该文件采用矩阵布局（每访视一行 × 每检查类别一列），非空单元格填写 `REF(WP)` 窗口规则（如 `RD(-7d)`、`EX(≤24h)`、`SV(±3d)`），其中 `REF` 指定锚点（RD / SV / EX / FD），`WP` 支持 `±`、`+`、`-`、`≤`、`≥`、`0`、`PREV` 等语法。输出包含 `wp_rule/ref/wp/type/wpvalue/wp_unit` 列。

- **`prepare_test_data()`**：`config` 参数新增支持 `read_testwp_file()` 的输出。此时生成的窗口规则列（`wp_rule/ref/wp/type/wpvalue/wp_unit`）会被保留并在结果中回填，供下游窗口推导使用；同时自动附加随机化日期（`rd_date`）、首次给药日期（`first_dose_date`）及逐次给药日期（`cyc_dose_date`）。从数据源读取给药时间需要新增 `ex_time_var` 参数。

- **`generate_test_window_dates()`**：解析每条检查记录的锚点日期，并推导窗口范围。新增 `anchor_date`、`anchor_datetime`、`window_start`、`window_end`、`window_start_dt`、`window_end_dt` 及 `window_status` 列。支持天级（Date）和小时级（POSIXct，如 `≤24h`）两种窗口。锚点类型包括 RD（随机日期）、SV（实际访视日期）、EX（实际给药日期）、FD（首次给药日期）。`PREV` 规则为无下界窗口，仅设定上界（锚点之前，含当天/时刻）。

- **`check_test_window()`**：按「受试者 × 访视 × 检查项类别」分组，判断实际检查日期（时间）是否落在窗口范围内。同一组内至少一条记录在窗内即合规；全部在窗外时记录一条偏离，并取离锚点最近的一条作为明细。支持自定义 `pdno`（默认 `"8.4.2"`）。

**窗口规则支持：**

- 天级（`wp_unit = "d"`）：比较 `TESTDAT` 与 `window_start` / `window_end`（Date）。
- 小时级（`wp_unit = "h"`，如 `EX(≤24h)`）：将 `TESTDAT` + `TESTTIM` 合并为日期时间，与 `window_start_dt` / `window_end_dt`（POSIXct）比较；时间缺省按 `00:00:00`。

**要点：**

- `TESTDAT` 缺失的检查项由 `check_missing_test()` 处理，不参与超窗判断。
- 窗口无法推导（`window_status` 为 `no_rule` / `no_anchor_data` / `missing_anchor_date` / `unsupported_rule`）的记录会被跳过。

示例见 `inst/extdata/example_test_wp.xlsx` 及配套示例脚本。

---

# pdchecker 0.9.5

## 新功能

- 访视计划文件支持可选列 **CYCDAY**：在各治疗周期 D1 行填写与上一周期间隔天数。
  `generate_planned_visit_dates()` 在 `cycle_days = NULL`（新默认值）时按 CYCDAY 计算；
  传入 `cycle_days` 时仍统一使用参数值。示例见 `inst/extdata/example_visitcode.xlsx`。

- `generate_planned_visit_dates()` 治疗结束参数(`eot_dataset/eot_date_var`)支持输入多个数据集。

---

# pdchecker 0.9.1

## 新功能

- `prepare_test_data()` 添加 `test_time_var` 参数，返回值包括 TESTTIM 列。

- 访视计划文件支持可选列 **CYCDAY**：在各治疗周期 D1 行填写与上一周期间隔天数。
  `generate_planned_visit_dates()` 在 `cycle_days = NULL`（新默认值）时按 CYCDAY 计算；
  传入 `cycle_days` 时仍统一使用参数值。示例见 `inst/extdata/example_visitcode.xlsx`。

---

# pdchecker 0.9.0

## 新功能

- `generate_excel_report()` 移除了 `include_no_deviation` 参数，改为 `report_cols` 参数，
  支持自定义（"All Deviations" 工作表）输出列，
  默认输出列为 `c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")`。

- `generate_excel_report()` 汇总表（"Summary"）现在列出所有检查项（含无偏离的检查）；
  明细表（"All Deviations"）仅保留有偏离的行，并按 `PDNO`、`SUBJID` 排序。

- 检查函数输出结果中新增 `TBNAME` 列（数据集来源），影响
  `check_missing_visit()`、`check_screen_without_ic()`、`check_visit_window()`
  以及 `generate_planned_visit_dates()`。

- `inst/scripts/` 新增示例脚本（`setup.R`、`pd_scripts.R`），
  提供开箱即用的 PD 检查工作流参考。

## Bug 修复

- 修复 `check_missing_test()` 在 `test` 指定多个值时，过滤逻辑不正确的问题。

---

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

## 全局配置

- 新增 `set_pdchecker_options()` / `get_pdchecker_options()`：统一设置和查看全局参数，避免重复指定数据集名称和变量名。

## 工具函数

- 新增 `is_sas_na()`：判断 SAS 缺失值（`NA`、`"."`、`""`）。

## 文档

- 新增用户手册（vignette），涵盖所有函数的参数说明、使用示例和常见问题。
- 新增示例配置文件（`inst/extdata/example_visitcode.xlsx`、`inst/extdata/example_test.xlsx`）。
