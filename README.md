# pdchecker <a href="https://github.com/Pharma-Mountain-High/pdchecker"><img src="man/figures/logo.png" align="right" height="138" alt="pdchecker logo" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Apache License 2.0](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)
<!-- badges: end -->

**pdchecker** (Protocol Deviation Checker) 是一个用于临床试验方案偏离自动检测的 R 包。目前包括数据读取、访视缺失/超窗、检查项缺失、知情同意检查等功能，并支持将结果导出为 Excel 报告。

## 安装

**方式一：从 GitHub 安装**

```r
# install.packages("devtools")
devtools::install_github("Pharma-Mountain-High/pdchecker")
```

**方式二：下载压缩包本地安装**

从 [Releases](https://github.com/Pharma-Mountain-High/pdchecker/releases) 页面下载 `.tar.gz` 源码包，然后本地安装：

```r
install.packages("pdchecker_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

## 快速开始

```r
library(pdchecker)

# 1. 读取临床数据
data <- read_raw_data("path/to/sas/datasets")

# 2. 读取配置文件
visitcode  <- read_visitcode_file("path/to/visit_schedule.xlsx")
testconfig <- read_testconfig_file("path/to/test_config.xlsx", visitcode = visitcode)

# 3. 生成计划访视日期
planned_dates <- generate_planned_visit_dates(data, cycle_days = 28)

# 4. 准备检查项数据
prepared_lb <- prepare_test_data(data, test_dataset = "LB")

# 5. 执行方案偏离检查
res_1 <- check_screen_without_ic(data)       # 筛查但无知情同意
res_2 <- check_icf_time_deviation(data)      # 知情同意前操作
res_3 <- check_missing_visit(planned_dates)  # 遗漏访视
res_4 <- check_visit_window(planned_dates)   # 访视超窗
res_5 <- check_missing_test(prepared_lb)     # LB 检查项缺失

# 6. 合并所有结果
all_results <- combine_check_results(
  res_1,res_2,res_3,res_4,res_5
)

# 7. 生成报告
generate_excel_report(all_results, "pd_report.xlsx")
```

## 外部配置文件说明

pdchecker 需要两个外部配置文件：**访视计划文件**和**检查项配置文件**，均支持 Excel（`.xlsx`/`.xls`）和 CSV 格式。

### 访视计划文件

用于 `read_visitcode_file()` 读取，供 `generate_planned_visit_dates()` 生成计划访视日期。

**必需列：**

| 列名 | 说明 | 示例 |
|------|------|------|
| VISIT | 访视名称 | C1D1 |
| VISITNUM | 访视编号（数值） | 1 |
| WP | 窗口期 | ±3d |
| CYCLE | 周期描述（用于自动识别访视类别） | 治疗周期1 |
| VISITDAY | 访视日（相对于周期 D1 的天数） | 8 |

**WP 列支持的格式：**

| 格式 | 含义 | 转换结果 |
|------|------|----------|
| `±3d` 或 `+/-3d` | 前后 N 天 | ±3 天 |
| `+7d` | 后 N 天内 | +7 天 |
| `-3d` | 前 N 天内 | -3 天 |
| `≤3d` 或 `<=3d` | 不超过 N 天 | ≤3 天 |
| `24h` 或 `24小时` | 小时（自动 ÷24 转天） | 1 天 |
| `2w` 或 `2周` | 周（自动 ×7 转天） | 14 天 |
| 空值 | 无窗口期限制 | — |

**CYCLE 列自动识别访视类别：**

| CYCLE 关键词 | 识别为 |
|-------------|--------|
| 筛选 / Screening | screening |
| 预治疗 / 预激 | pre_treatment |
| 治疗（不含"治疗结束"）/ Cycle | treatment |
| 治疗结束 / End of Treatment | end_of_treatment |
| 随访 / Follow | follow_up |

**示例文件：**

| VISIT | VISITNUM | WP | CYCLE | VISITDAY |
|-------|----------|-----|-------|----------|
| 筛选访视 | 0 | | 筛选 | -28 |
| C1D1 | 1 | ±1d | 治疗周期1 | 1 |
| C1D8 | 2 | ±2d | 治疗周期1 | 8 |
| C1D15 | 3 | ±2d | 治疗周期1 | 15 |
| C2D1 | 4 | ±3d | 治疗周期2 | 1 |
| EOT | 99 | +7d | 治疗结束 | EOT |
| FU1 | 100 | ±7d | 随访 | LD+28 |
| EOS | 999 | | 研究结束 | EOS |
```r
visitcode <- read_visitcode_file("visit_schedule.xlsx")
```

### 检查项配置文件

用于 `read_testconfig_file()` 读取，定义每个检查类别需要在哪些访视执行，供 `check_missing_test()` 使用。

**必需列：**

| 列名 | 说明 | 示例 |
|------|------|------|
| TESTCAT | 检查类别名称 | 血常规 |
| VISITNUM | 需执行的访视编号，逗号分隔 | 0,1,4,6,99 |

VISITNUM 列支持中英文逗号分隔，函数会自动展开为多行。

**示例文件：**

| TESTCAT | VISITNUM |
|---------|----------|
| 血常规 | 0,1,4,6,99 |
| 生化 | 0,1,4,6,99 |
| 尿常规 | 0,1,99 |
| 凝血功能 | 0,1,99 |
| 心电图 | 0,1,4,6,99 |
| 生命体征 | 0,1,2,3,4,5,6,99 |

```r
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
```

> **提示**：建议将访视计划的读取结果赋值给 `visitcode`，检查项配置的读取结果赋值给 `testconfig`。后续的 `generate_planned_visit_dates()` 和 `prepare_test_data()` 会自动在调用环境中查找这两个变量名，无需手动传参。

> **示例文件**：包内 `inst/extdata` 目录提供了两个可直接参考的模板文件：`example_visitcode.xlsx`（访视计划）和 `example_test.xlsx`（检查项配置）。安装包后可通过以下方式获取路径：
>
> ```r
> system.file("extdata", "example_visitcode.xlsx", package = "pdchecker")
> system.file("extdata", "example_test.xlsx", package = "pdchecker")
> ```

## 功能概览

### 数据读取

| 函数 | 说明 |
|------|------|
| `read_raw_data()` | 批量读取 SAS 数据集，支持 IWRS CSV 和 Excel 编码文件映射 |
| `read_raw_data_with_formats()` | 使用（`.sas7bcat`）读取数据并进行编码映射 |

### 数据准备

| 函数 | 说明 |
|------|------|
| `read_visitcode_file()` | 从 Excel读取访视计划并解析窗口期（如 `+/-3d`、`<=24h`） |
| `read_testconfig_file()` | 从 Excel读取各访视需要进行的检查项 |
| `get_first_dose_date()` | 提取每位受试者的首次给药日期 |
| `get_last_dose_date()` | 提取每位受试者的末次给药日期 |
| `get_eot_date()` | 提取治疗结束日期 |
| `get_eos_date()` | 提取研究结束日期 |
| `generate_planned_visit_dates()` | 根据访视计划和临床数据生成每位受试者的计划访视日期与窗口范围 |
| `prepare_test_data()` | 准备和标准化检查项数据，用于检查项缺失检查 |

### 方案偏离检查

| 函数 | 说明 |
|------|------|
| `check_screen_without_ic()` | 识别有筛选访视但缺少知情同意的受试者 |
| `check_icf_time_deviation()` | 检测在知情同意之前执行的研究程序 |
| `check_missing_visit()` | 基于计划访视日期和截止标准检查遗漏访视 |
| `check_visit_window()` | 检查已完成访视是否在规定的访视窗口内 |
| `check_missing_test()` | 检查每次访视中缺失的检测项目 |

### 结果处理与报告

| 函数 | 说明 |
|------|------|
| `capture_check_results()` | 批量运行多个检查函数并合并结果 |
| `combine_check_results()` | 合并多个检查结果数据框 |
| `generate_markdown_report()` | 生成 Markdown 格式报告 |
| `generate_html_report()` | 生成 HTML 格式报告 |
| `generate_excel_report()` | 生成 Excel 格式报告 |

### 工具函数

| 函数 | 说明 |
|------|------|
| `set_pdchecker_options()` / `get_pdchecker_options()` | 设置/获取Function参数 |
| `is_sas_na()` | 判断值是否为 SAS 缺失值（`NA`、`"."`、`""`） |

## 注意事项

- 所有Rawdata列名在处理后统一转换为大写
- SAS 缺失值（`NA`、`"."`、`""`）会被自动处理
- 日期变量保持原始格式
- 包含完善的错误处理和信息提示

## 许可证

本项目基于 [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) 发布。详见 [LICENSE](LICENSE) 文件。

## 问题反馈

如有问题或建议，请在 [GitHub Issues](https://github.com/Pharma-Mountain-High/pdchecker/issues) 提交。
