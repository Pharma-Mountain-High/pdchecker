# pdchecker 包使用手册

## 1. 概述

`pdchecker` 是一个用于临床试验方案偏离（Protocol Deviation, PD）检查的 R 包。该包提供了一系列函数，用于自动化检测临床试验数据中的常见方案偏离情况，帮助临床数据管理人员快速识别和报告数据质量问题。

## 2. 函数开发状态

### 已完成的用户函数

| 函数名 | 功能 | 状态 |
|--------|------|------|
| `read_raw_data()` | 读取 SAS 数据文件 | ✅ 已完成 |
| `read_raw_data_with_formats()` | 读取带格式目录的 SAS 数据 | ✅ 已完成 |
| `read_visitcode_file()` | 读取访视计划文件 | ✅ 已完成 |
| `prepare_test_data()` | 准备检查数据 | ✅ 已完成 |
| `generate_planned_visit_dates()` | 生成计划访视日期 | ✅ 已完成 |
| `check_icf_time_deviation()` | 检查 ICF 时间偏离 | ✅ 已完成 |
| `check_screen_without_ic()` | 检查未签署知情同意 | ✅ 已完成 |
| `check_missing_visit()` | 检查遗漏访视 | ✅ 已完成 |
| `check_visit_window()` | 检查访视超窗 | ✅ 已完成 |
| `check_missing_test()` | 检查检查项缺失 | ✅ 已完成 |
| `get_first_dose_date()` | 获取首次给药日期 | ✅ 已完成 |
| `get_last_dose_date()` | 获取末次给药日期 | ✅ 已完成 |
| `get_eot_date()` | 获取治疗结束日期 | ✅ 已完成 |
| `get_eos_date()` | 获取研究结束日期 | ✅ 已完成 |
| `generate_markdown_report()` | 生成 Markdown 报告 | ✅ 已完成 |
| `generate_html_report()` | 生成 HTML 报告 | ✅ 已完成 |
| `generate_excel_report()` | 生成 Excel 报告 | ✅ 已完成 |

---

## 3. 外部文件准备指南

本包的部分函数依赖于外部配置文件。本章节详细说明如何准备这些外部文件。

### 3.1 访视计划文件（用于 read_visitcode_file）

#### 文件格式
支持 Excel（.xlsx, .xls）或 CSV（.csv）格式。

#### 必需列

| 列名 | 类型 | 说明 | 是否必需 |
|------|------|------|----------|
| **WP** | 文本 | 访视窗口期 | ✅ 必需 |
| VISIT | 文本 | 访视名称 | ✅ 必需 |
| VISITNUM | 数值 | 访视编号 | ✅ 必需 |
| CYCLE | 文本 | 访视周期/类型描述 | ✅ 必需（用于自动识别访视类别） |
| VISITDAY | 文本/数值 | 访视日（相对于周期第1天） | ✅ 必需 |


#### WP 列填写规则

WP（Window Period）列用于定义访视窗口期，支持以下格式：

| 格式示例 | 含义 | 解析结果 |
|----------|------|----------|
| `±3d` 或 `+/-3d` | 计划日期前后3天 | type=±, wpvalue=3 |
| `+3d` | 计划日期后3天内 | type=+, wpvalue=3 |
| `-3d` | 计划日期前3天内 | type=-, wpvalue=3 |
| `≤3d` 或 `<=3d` | 计划日期前3天内 | type=≤, wpvalue=3 |
| `≥3d` 或 `>=3d` | 计划日期后3天内 | type=≥, wpvalue=3 |
| `24h` 或 `24小时` | 24小时（自动转换为1天） | type=+, wpvalue=1 |
| `2w` 或 `2周` | 2周（自动转换为14天） | type=+, wpvalue=14 |
| `±2w` | 前后2周 | type=±, wpvalue=14 |
| 空值 | 无窗口期限制 | type=NA, wpvalue=NA |

#### 时间单位说明

| 单位 | 后缀 | 转换规则 |
|------|------|----------|
| 小时 | `h` 或 `小时` | ÷ 24 转换为天 |
| 天 | `d`、`天` 或 `日` | 保持不变 |
| 周 | `w` 或 `周` | × 7 转换为天 |

#### CYCLE 列填写规则

CYCLE 列用于描述访视所属的周期或阶段，系统会自动识别访视类别：

| CYCLE 列内容（包含以下关键词） | 识别为 visit_category |
|-------------------------------|----------------------|
| 筛选 | screening（筛选期） |
| 预激剂量、预激、预治疗 | pre_treatment（预治疗期） |
| 治疗（但不含"治疗结束"） | treatment（治疗期） |
| 治疗结束、退出 | end_of_treatment（治疗结束） |
| 研究结束 | end_of_study（研究结束） |
| 随访 | follow_up（随访期） |
| 其他 | unknown（未知） |

#### 示例文件

**visit_schedule.xlsx 示例内容：**

| VISIT | VISITNUM | WP | CYCLE | VISITDAY |
|-------|----------|-----|-------|----------|
| 筛选访视 | 0 | | 筛选 | -28 |
| C1D1 | 1 | ±1d | 治疗周期1 | 1 |
| C1D8 | 2 | ±2d | 治疗周期1 | 8 |
| C1D15 | 3 | ±2d | 治疗周期1 | 15 |
| C2D1 | 4 | ±3d | 治疗周期2 | 1 |
| C2D8 | 5 | ±2d | 治疗周期2 | 8 |
| C3D1 | 6 | ±3d | 治疗周期3 | 1 |
| EOT | 99 | +7d | 治疗结束 | EOT |
| FU1 | 100 | ±7d | 随访 | 28 |
| FU2 | 101 | ±7d | 随访 | 56 |
| EOS | 999 | | 研究结束 | EOS |

---

### 3.2 检查配置文件（用于 prepare_test_data）

#### 文件格式
支持 Excel（.xlsx）格式或直接使用 R 数据框。

#### 必需列

| 列名 | 类型 | 说明 | 是否必需 |
|------|------|------|----------|
| **TESTCAT** | 文本 | 检查类别名称 | ✅ 必需 |
| **VISITNUM** | 文本/数值 | 需要执行该检查的访视编号 | ✅ 必需 |

#### TESTCAT 列填写规则

TESTCAT 应与实际数据中的检查类别变量值一致。例如：
- 血常规
- 生化
- 尿常规
- 凝血功能
- 心电图
- 生命体征

#### VISITNUM 列填写规则

VISITNUM 用于指定哪些访视需要执行该检查，支持以下格式：

| 格式 | 说明 | 示例 |
|------|------|------|
| 单个数值 | 只在该访视执行 | `1` |
| 逗号分隔的多个数值 | 在多个访视执行 | `1,2,3,4` 或 `1，2，3，4`（支持中英文逗号） |

#### 示例文件

**test_config.xlsx 示例内容：**

| TESTCAT | VISITNUM |
|---------|----------|
| 血常规 | 0,1,4,6,99 |
| 生化 | 0,1,4,6,99 |
| 尿常规 | 0,1,99 |
| 凝血功能 | 0,1,99 |
| 心电图 | 0,1,4,6,99 |
| 生命体征 | 0,1,2,3,4,5,6,99 |
| 妊娠试验 | 0,1,99 |

**解释**：
- 血常规需要在访视0（筛选）、访视1（C1D1）、访视4（C2D1）、访视6（C3D1）、访视99（EOT）执行
- 生命体征需要在几乎所有访视执行

#### 使用 R 数据框代替文件

如果不想创建外部文件，也可以直接在 R 中创建配置数据框：

```r
# 方法1：使用 read_testconfig_file 读取配置文件（推荐）
testconfig <- read_testconfig_file("test_config.xlsx")
prepared_data <- prepare_test_data(
  data = data,
  test_dataset = "LB"
  # config 默认使用 testconfig 变量
)

# 方法2：直接创建配置数据框
testconfig <- data.frame(
  TESTCAT = c("血常规", "生化", "尿常规", "心电图"),
  VISITNUM = c("1", "1", "1", "1"),  # 注意：每行一个 VISITNUM
  stringsAsFactors = FALSE
)
# 如果 VISITNUM 是逗号分隔的，使用 read_testconfig_file 会自动展开

# 使用配置数据框
prepared_data <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  config = testconfig
)
```

---

### 3.3 文件准备最佳实践

1. **使用 Excel 格式**：推荐使用 .xlsx 格式，避免 CSV 的编码问题
2. **保持列名一致**：列名区分大小写，请严格按照文档要求填写
3. **检查空值**：确保必需列没有意外的空值
4. **数值格式**：VISITNUM 等数值列可以是数值或文本格式，系统会自动处理
5. **中文支持**：系统完全支持中文内容，但确保文件以 UTF-8 编码保存
6. **模板文件**：建议为每个项目创建模板文件，便于复用

---

## 4. 数据读取函数

### 4.1 read_raw_data() - 读取 SAS 数据文件

#### 功能说明
扫描指定目录，读取所有 SAS 格式文件（.sas7bdat），并可选择读取 IWRS CSV 文件和 Excel 格式映射文件。

#### 使用方法

```r
data <- read_raw_data(
  folder,           # SAS 数据文件所在目录路径
  iwrs_file = NULL, # IWRS CSV 文件路径（可选）
  format_file = NULL # Excel 格式映射文件路径（可选）
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `folder` | 字符串 | SAS 数据文件所在目录的路径 |
| `iwrs_file` | 字符串 | IWRS CSV 文件路径。如提供，CSV 文件的前两行表头将被跳过 |
| `format_file` | 字符串 | Excel 格式映射文件路径（.xlsx），用于将编码值映射为标签 |

#### 返回值
返回一个命名列表，包含所有成功读取的数据集。列表元素名称为数据集名称（大写），例如：`data$DM`、`data$AE`、`data$LB`。

#### 使用示例

```r
# 基本用法：仅读取 SAS 文件
data <- read_raw_data("path/to/sas/files")

# 读取 SAS 文件和 IWRS 文件
data <- read_raw_data(
  folder = "path/to/sas/files",
  iwrs_file = "path/to/iwrs.csv"
)

# 读取时应用格式映射
data <- read_raw_data(
  folder = "path/to/sas/files",
  iwrs_file = "path/to/iwrs.csv",
  format_file = "path/to/formats.xlsx"
)

# 访问读取的数据集
dm_data <- data$DM
ae_data <- data$AE
```

#### 注意事项
- 所有数据集的列名会自动转换为大写
- 目录中如果存在名为 "formats" 的 SAS 文件，会被用作格式映射文件
- 读取失败的文件会显示警告信息，但不会中断程序

---

### 4.2 read_raw_data_with_formats() - 读取带格式目录的 SAS 数据

#### 功能说明
使用 SAS 格式目录文件（.sas7bcat）读取数据，自动应用格式标签。

#### 使用方法

```r
data <- read_raw_data_with_formats(
  data_dir,         # SAS 数据文件目录
  catalog_file,     # SAS 格式目录文件路径
  iwrs_file = NULL, # IWRS CSV 文件路径（可选）
  encoding = "UTF-8" # 字符编码
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data_dir` | 字符串 | SAS 数据文件所在目录 |
| `catalog_file` | 字符串 | SAS 格式目录文件路径（.sas7bcat） |
| `iwrs_file` | 字符串 | IWRS CSV 文件路径 |
| `encoding` | 字符串 | 读取 SAS 文件时使用的字符编码，默认 "UTF-8" |

#### 使用示例

```r
# 读取带格式目录的数据
data <- read_raw_data_with_formats(
  data_dir = "path/to/sas/files",
  catalog_file = "path/to/formats.sas7bcat",
  iwrs_file = "path/to/iwrs.csv"
)

# 处理中文编码
data <- read_raw_data_with_formats(
  data_dir = "path/to/sas/files",
  catalog_file = "path/to/formats.sas7bcat",
  encoding = "GBK"
)
```

---

### 4.3 read_visitcode_file() - 读取访视计划文件

#### 功能说明
读取访视计划 Excel 或 CSV 文件，自动解析窗口期（WP）列，生成窗口类型和窗口值。

#### 使用方法

```r
visit_schedule <- read_visitcode_file(
  file_path,            # 文件路径
  sheet_name = "Sheet1" # Excel 工作表名称
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `file_path` | 字符串 | Excel 或 CSV 文件路径 |
| `sheet_name` | 字符串 | Excel 工作表名称，默认 "Sheet1" |

#### 窗口期格式支持

| 输入格式 | 解析结果（type） | 解析结果（wpvalue） |
|----------|------------------|---------------------|
| `+/-3d` | ± | 3 |
| `±3d` | ± | 3 |
| `<=24h` | ≤ | 1（24小时=1天） |
| `+2d` | + | 2 |
| `-1d` | - | 1 |
| `1w` | + | 7（1周=7天） |
| `+/-2w` | ± | 14（2周=14天） |

#### 返回值
返回一个 tibble，包含原始文件中的所有列，以及新增的列：
- `type`：窗口类型（±, ≤, ≥, +, -, 范围, 其他, 或 NA）
- `wpvalue`：窗口值（数值型，以天为单位）
- `visit_category`：访视类别（如果存在 CYCLE 列）

#### 访视类别自动识别规则

| CYCLE 列内容 | 识别为 visit_category |
|--------------|----------------------|
| 包含"筛选" | screening |
| 包含"预激剂量/预激/预治疗" | pre_treatment |
| 包含"治疗"（但不包含"治疗结束"） | treatment |
| 包含"治疗结束/退出" | end_of_treatment |
| 包含"研究结束" | end_of_study |
| 包含"随访" | follow_up |
| 其他 | unknown |

#### 使用示例

```r
# 读取 Excel 文件
visit_schedule <- read_visitcode_file("visit_schedule.xlsx")

# 读取指定工作表
visit_schedule <- read_visitcode_file(
  "visit_schedule.xlsx",
  sheet_name = "Schedule"
)

# 读取 CSV 文件
visit_schedule <- read_visitcode_file("visit_schedule.csv")
```

---

## 5. 数据准备函数

### 5.1 prepare_test_data() - 准备检查数据

#### 功能说明
从数据列表中提取指定的检查数据集，与访视数据集合并，创建标准化的列名（TBNAME, TESTCAT, TESTDE, TESTDAT, TESTYN, ORRES），为后续的缺失检查做准备。

#### 使用方法

```r
prepared_data <- prepare_test_data(
  data,                       # 包含所有数据集的列表
  test_dataset,               # 检查数据集名称（如 "LB"）
  test_date_var = "LBDAT",    # 检查日期变量名
  test_yn_var = "YN",         # 是否执行检查变量名
  test_result_var = "ORRES",  # 检查结果变量名
  test_cat_var = "LBCAT",     # 检查类别变量名
  test_de_var = NULL,         # 检查项目变量名
  tb_name_var = NULL,         # 表名变量名
  sv_dataset = "SV",          # 访视数据集名称
  sv_visit_var = "VISIT",     # 访视名称变量
  sv_visitnum_var = "VISITNUM", # 访视编号变量
  sv_date_var = "SVDAT",      # 访视日期变量
  config = NULL,              # 配置数据框（必须，默认自动查找 testconfig 变量）
  config_cat = NULL,          # 筛选的检查类别
  filter_cond = NULL          # 受试者筛选条件
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | 列表 | 包含所有临床试验数据集的列表 |
| `test_dataset` | 字符串 | 要提取的检查数据集名称，如 "LB"、"VS"、"EG" |
| `test_date_var` | 字符串 | 原始检查日期变量名，默认 "LBDAT" |
| `test_yn_var` | 字符串 | 原始是否执行变量名，默认 "YN" |
| `test_result_var` | 字符串 | 原始检查结果变量名，默认 "ORRES" |
| `test_cat_var` | 字符串 | 原始检查类别变量名，默认 "LBCAT" |
| `test_de_var` | 字符串 | 原始检查项目变量名，用于具体指标名称 |
| `tb_name_var` | 字符串 | 原始表名变量名 |
| `sv_dataset` | 字符串 | 访视数据集名称，默认 "SV" |
| `sv_visit_var` | 字符串 | 访视名称变量，默认 "VISIT" |
| `sv_visitnum_var` | 字符串 | 访视编号变量，默认 "VISITNUM" |
| `sv_date_var` | 字符串 | 访视日期变量，默认 "SVDAT" |
| `config` | 数据框 | 配置数据框（必须参数）。如果为 NULL，会自动查找调用环境中的 `testconfig` 变量；如果找不到则报错。通过 `read_testconfig_file()` 读取 |
| `config_cat` | 字符向量 | 要筛选的检查类别 |
| `filter_cond` | 字符串 | 受试者筛选条件 |

#### 筛选条件语法
筛选条件格式为 `"数据集|条件表达式"`，多个条件用分号分隔（取交集）：
- `"ENROL|ENRYN=='Y'"` - 筛选已入组受试者
- `"SUBJECT|SEX=='M'"` - 筛选男性受试者
- `"ENROL|ENRYN=='Y';SUBJECT|SEX=='M'"` - 筛选已入组的男性受试者

#### 使用示例

```r
# 推荐工作流程：先读取配置文件
testconfig <- read_testconfig_file("config/test_config.xlsx")

# 基本用法（自动使用 testconfig 变量）
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB"
)

# 多次调用，无需重复读取配置文件
prepared_vs <- prepare_test_data(data = data, test_dataset = "VS")
prepared_eg <- prepare_test_data(data = data, test_dataset = "EG")

# 指定变量映射
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_cat_var = "LBCAT",
  test_de_var = "LBTEST"
)

# 使用配置时筛选特定检查类别
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  config_cat = c("血常规", "生化")
)

# 筛选特定受试者
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  filter_cond = "ENROL|ENRYN=='Y'"
)
```

---

### 5.2 generate_planned_visit_dates() - 生成计划访视日期

#### 功能说明
根据访视计划数据和临床试验数据，为每个受试者计算计划访视日期和访视窗口范围。支持筛选期、治疗期、治疗结束和随访期访视的日期计算。

#### 使用方法

```r
planned_dates <- generate_planned_visit_dates(
  data,                        # 临床试验数据列表
  visitcode = NULL,            # 访视计划数据（默认自动查找 visitcode 变量）
  ex_datasets = "EX",          # 用药数据集名称
  ex_date_var = "EXSTDAT",     # 用药开始日期变量
  ex_end_date_var = NULL,      # 用药结束日期变量
  sv_dataset = "SV",           # 访视数据集名称
  sv_visit_var = "VISIT",      # 访视名称变量
  sv_visitnum_var = "VISITNUM", # 访视编号变量
  sv_date_var = "SVDAT",       # 访视日期变量
  eot_dataset = "EOT",         # 治疗结束数据集
  eot_date_var = "EOTDAT",     # 治疗结束日期变量
  ds_dataset = "DS",           # 研究结束数据集
  ds_date_var = "DSDAT",       # 研究结束日期变量
  cycle_days = 28              # 治疗周期天数
)
```

#### 计划日期计算规则

| 访视类别 | 计算基准 |
|----------|----------|
| 筛选期（screening） | 基于首次给药日期 |
| 预治疗期（pre_treatment） | 基于首次给药日期 |
| 治疗期 D1（treatment D1） | 使用迭代周期计算 |
| 治疗期非 D1（treatment non-D1） | 基于当前周期 D1 日期 |
| 治疗结束（end_of_treatment） | 基于 EOT 日期或 EOS 日期 |
| 随访（follow_up） | 基于 EOT 日期或末次给药日期 |

#### 窗口期计算规则

| 窗口类型 | 窗口范围 |
|----------|----------|
| ±Nd | [计划日期 - N, 计划日期 + N] |
| +Nd | [计划日期, 计划日期 + N] |
| -Nd | [计划日期 - N, 计划日期] |
| ≤Nd | [计划日期 - N, 计划日期] |
| ≥Nd | [计划日期, 计划日期 + N] |

#### 返回值
返回一个数据框，包含以下列：

| 列名 | 说明 |
|------|------|
| SUBJID | 受试者编号 |
| VISIT | 访视名称 |
| VISITNUM | 访视编号 |
| visittype | 访视类型（周期信息） |
| visitday | 访视日 |
| visit_category | 访视类别 |
| planned_date | 计划访视日期 |
| wp_start | 窗口期开始日期 |
| wp_end | 窗口期结束日期 |
| wp_type | 窗口类型 |
| wp_value | 窗口值（天） |
| actual_date | 实际访视日期 |
| status | 访视状态（completed/missing） |
| first_dose_date | 首次给药日期 |
| last_dose_date | 末次给药日期 |
| eot_date | 治疗结束日期 |
| eos_date | 研究结束日期 |

#### 使用示例

```r
# 读取访视计划（结果保存为 visitcode 变量）
visitcode <- read_visitcode_file("visit_schedule.xlsx")

# 生成计划访视日期（自动使用 visitcode 变量）
planned_dates <- generate_planned_visit_dates(data = data)

# 或显式传入
planned_dates <- generate_planned_visit_dates(
  data = data,
  visitcode = visitcode
)

# 自定义周期天数（21天周期）
planned_dates <- generate_planned_visit_dates(
  data = data,
  visitcode = visitcode,
  cycle_days = 21
)

# 多个用药数据集
planned_dates <- generate_planned_visit_dates(
  data = data,
  visitcode = visitcode,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = c("EXSTDAT1", "EXSTDAT2")
)
```

---

## 6. 方案偏离检查函数

### 6.1 check_icf_time_deviation() - 检查 ICF 时间偏离

#### 功能说明
检查是否有任何操作（日期变量）发生在知情同意书签署日期之前。

#### 判断规则
**偏离条件**：任何数据集中以 "DAT" 结尾的日期变量值 < 知情同意书签署日期

#### 使用方法

```r
result <- check_icf_time_deviation(
  data,                        # 数据列表
  ic_dataset = "IC",           # 知情同意数据集名称
  ic_date_var = "ICDAT",       # 知情同意日期变量
  ignore_vars = "BRTHDAT",     # 忽略的变量
  exclude_datasets = NULL,     # 排除的数据集
  tb_name_var = NULL,          # 表名变量
  pdno = "2.1.1"               # 偏离编号
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | 列表 | 包含所有数据集的列表 |
| `ic_dataset` | 字符串 | 知情同意数据集名称，默认 "IC" |
| `ic_date_var` | 字符串 | 知情同意日期变量名，默认 "ICDAT" |
| `ignore_vars` | 字符向量 | 要忽略的变量名，默认 "BRTHDAT"（出生日期） |
| `exclude_datasets` | 字符向量 | 要排除的数据集名称 |
| `tb_name_var` | 字符串 | 用于显示表名的变量 |
| `pdno` | 字符串 | 方案偏离编号，默认 "2.1.1" |

#### 返回值
返回一个列表，包含：
- `has_deviation`：是否存在偏离（TRUE/FALSE）
- `messages`：偏离摘要信息
- `details`：偏离详情数据框

#### 使用示例

```r
# 基本用法
result <- check_icf_time_deviation(data)
print(result)

# 排除特定数据集
result <- check_icf_time_deviation(
  data,
  exclude_datasets = c("DM", "DS")
)

# 忽略额外的日期变量
result <- check_icf_time_deviation(
  data,
  ignore_vars = c("BRTHDAT", "MHSTDAT", "DSSTDAT")
)

# 查看详细结果
if (result$has_deviation) {
  print(result$details)
}
```

---

### 6.2 check_screen_without_ic() - 检查未签署知情同意

#### 功能说明
检查是否存在进行了筛选访视但未签署知情同意书的受试者。

#### 判断规则
**偏离条件**：受试者有筛选访视记录，但在 IC 数据集中没有知情同意日期

#### 使用方法

```r
result <- check_screen_without_ic(
  data,                              # 数据列表
  sv_dataset = "SV",                 # 访视数据集名称
  ic_dataset = "IC",                 # 知情同意数据集名称
  visit_var = "VISIT",               # 访视名称变量
  visit_pattern = "Screening|screening", # 筛选访视识别模式
  ic_date_var = "ICDAT",             # 知情同意日期变量
  pdno = "2.4.1"                     # 偏离编号
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | 列表 | 包含所有数据集的列表 |
| `sv_dataset` | 字符串 | 访视数据集名称，默认 "SV" |
| `ic_dataset` | 字符串 | 知情同意数据集名称，默认 "IC" |
| `visit_var` | 字符串 | 访视名称变量，默认 "VISIT" |
| `visit_pattern` | 字符串 | 筛选访视的正则表达式模式 |
| `ic_date_var` | 字符串 | 知情同意日期变量，默认 "ICDAT" |
| `pdno` | 字符串 | 方案偏离编号，默认 "2.4.1" |

#### 使用示例

```r
# 基本用法
result <- check_screen_without_ic(data)
print(result)

# 自定义筛选访视模式
result <- check_screen_without_ic(
  data,
  visit_pattern = "Scr|筛选"
)
```

---

### 6.3 check_missing_visit() - 检查遗漏访视

#### 功能说明
基于计划访视日期，检查是否存在应该完成但未完成的访视。

#### 判断规则

| 访视类别 | 应完成条件 |
|----------|------------|
| 筛选期、预治疗期、治疗期 | 计划日期 < min(治疗结束日期, 研究结束日期, 截止日期) |
| 治疗结束、随访 | 计划日期 ≤ min(研究结束日期, 截止日期) |

**偏离条件**：满足应完成条件但状态为 "missing" 的访视

#### 使用方法

```r
result <- check_missing_visit(
  planned_dates,              # 计划访视日期数据框
  cutoffdt = Sys.Date(),      # 数据截止日期
  pdno = "8.2.1"              # 偏离编号
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `planned_dates` | 数据框 | 由 `generate_planned_visit_dates()` 生成的计划访视数据 |
| `cutoffdt` | 日期 | 数据截止日期，默认当前日期 |
| `pdno` | 字符串 | 方案偏离编号，默认 "8.2.1" |

#### 使用示例

```r
# 先读取访视计划并生成计划访视日期
visitcode <- read_visitcode_file("visit_schedule.xlsx")
planned_dates <- generate_planned_visit_dates(data = data)

# 检查遗漏访视（使用当前日期作为截止日期）
result <- check_missing_visit(planned_dates)
print(result)

# 指定截止日期
result <- check_missing_visit(
  planned_dates = planned_dates,
  cutoffdt = as.Date("2024-06-30")
)

# 查看详细结果
if (result$has_deviation) {
  missing_details <- result$details
  print(missing_details)
}
```

---

### 6.4 check_visit_window() - 检查访视超窗

#### 功能说明
检查已完成的访视是否在规定的窗口期内进行。

#### 判断规则
对于已完成的访视（status == "completed"）：
- **合规**：窗口开始日期 ≤ 实际访视日期 ≤ 窗口结束日期
- **偏离**：实际访视日期 < 窗口开始日期 或 实际访视日期 > 窗口结束日期

#### 使用方法

```r
result <- check_visit_window(
  planned_dates,    # 计划访视日期数据框
  pdno = "8.4.1"    # 偏离编号
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `planned_dates` | 数据框 | 由 `generate_planned_visit_dates()` 生成的计划访视数据 |
| `pdno` | 字符串 | 方案偏离编号，默认 "8.4.1" |

#### 返回值详情
`details` 数据框包含以下关键列：
- `deviation_days`：偏离天数（正值=延迟，负值=提前）
- `wp_start`：窗口开始日期
- `wp_end`：窗口结束日期
- `actual_date`：实际访视日期
- `planned_date`：计划访视日期

#### 使用示例

```r
# 读取访视计划并生成计划访视日期
visitcode <- read_visitcode_file("visit_schedule.xlsx")
planned_dates <- generate_planned_visit_dates(data = data)

# 检查访视超窗
result <- check_visit_window(planned_dates)
print(result)

# 查看超窗详情
if (result$has_deviation) {
  window_details <- result$details
  # 获取有超窗问题的受试者
  subjects_with_issues <- unique(window_details$SUBJID)
}
```

---

### 6.5 check_missing_test() - 检查检查项缺失

#### 功能说明
对于每个受试者的已完成访视记录，检查是否执行了特定的检查项目。

#### 判断规则

该函数区分三种缺失类型：

| 缺失类型 | 条件 | 说明 |
|----------|------|------|
| TESTCAT_EMPTY | TESTCAT 为空或 NA | 访视无检查记录 |
| TESTCAT_MISSING | TESTCAT 非空，但 TESTDAT 为空或 TESTYN ≠ "是" | 整体检查项未执行 |
| TESTDE_MISSING | TESTCAT 非空，TESTDAT 非空，但 ORRES 为空 | 具体指标结果缺失 |

#### 使用方法

```r
result <- check_missing_test(
  data,                  # 准备好的检查数据
  test_var = NULL,       # 筛选变量名
  test = NULL,           # 筛选值
  missing_de = TRUE,     # 是否检查具体指标缺失
  pdno = "8.3.1"         # 偏离编号
)
```

#### 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | 数据框 | 由 `prepare_test_data()` 准备的数据 |
| `test_var` | 字符串 | 用于筛选的变量名，如 "TESTDE" 或 "TESTCAT" |
| `test` | 字符串 | `test_var` 的筛选值 |
| `missing_de` | 逻辑值 | 是否检查具体指标缺失，默认 TRUE |
| `pdno` | 字符串 | 方案偏离编号，默认 "8.3.1" |

#### 使用示例

```r
# 步骤1：读取配置文件
testconfig <- read_testconfig_file("config/test_config.xlsx")

# 步骤2：准备数据（自动使用 testconfig）
prepared_data <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_cat_var = "LBCAT",
  test_de_var = "LBTEST"
)

# 步骤3：检查所有检查项
result <- check_missing_test(data = prepared_data)
print(result)

# 只检查特定检查项
result <- check_missing_test(
  data = prepared_data,
  test_var = "TESTDE",
  test = "红细胞计数"
)

# 只检查整体检查项缺失，不检查具体指标
result <- check_missing_test(
  data = prepared_data,
  missing_de = FALSE
)
```

---

## 7. 日期提取函数

### 7.1 get_first_dose_date() - 获取首次给药日期

#### 功能说明
从用药数据集中提取每个受试者的最早给药日期。

#### 使用方法

```r
first_dates <- get_first_dose_date(
  data,                    # 数据列表
  ex_datasets = "EX",      # 用药数据集名称
  ex_date_var = "EXSTDAT"  # 用药日期变量
)
```

#### 使用示例

```r
# 单个数据集
first_dates <- get_first_dose_date(data)

# 多个数据集
first_dates <- get_first_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = "EXSTDAT"
)
```

---

### 7.2 get_last_dose_date() - 获取末次给药日期

#### 功能说明
从用药数据集中提取每个受试者的最后给药日期。

#### 使用方法

```r
last_dates <- get_last_dose_date(
  data,                      # 数据列表
  ex_datasets = "EX",        # 用药数据集名称
  ex_date_var = "EXSTDAT",   # 用药开始日期变量
  ex_end_date_var = NULL     # 用药结束日期变量（可选）
)
```

#### 计算规则
- 如果未指定 `ex_end_date_var`：使用用药开始日期的最大值
- 如果指定了 `ex_end_date_var`：使用用药结束日期的最大值

---

### 7.3 get_eot_date() - 获取治疗结束日期

#### 功能说明
从 EOT 数据集中提取每个受试者的治疗结束日期。

```r
eot_dates <- get_eot_date(
  data,
  eot_dataset = "EOT",
  eot_date_var = "EOTDAT"
)
```

---

### 7.4 get_eos_date() - 获取研究结束日期

#### 功能说明
从 DS 数据集中提取每个受试者的研究结束日期。

```r
eos_dates <- get_eos_date(
  data,
  ds_dataset = "DS",
  ds_date_var = "DSDAT"
)
```

---

## 8. 报告生成函数

### 8.1 generate_markdown_report() - 生成 Markdown 报告

#### 功能说明
将检查结果转换为 Markdown 格式的报告。

#### 使用方法

```r
generate_markdown_report(
  checks_df,                      # 检查结果数据框
  output_file = NULL,             # 输出文件路径
  include_no_deviation = FALSE,   # 是否包含无偏离的检查
  title = "Study Deviation Report" # 报告标题
)
```

---

### 8.2 generate_html_report() - 生成 HTML 报告

#### 功能说明
将检查结果转换为 HTML 格式的报告。

#### 使用方法

```r
generate_html_report(
  checks_df,                      # 检查结果数据框
  output_file,                    # 输出文件路径（必填）
  include_no_deviation = FALSE,   # 是否包含无偏离的检查
  title = "Study Deviation Report", # 报告标题
  css_file = NULL                 # 自定义 CSS 文件
)
```

---

### 8.3 generate_excel_report() - 生成 Excel 报告

#### 功能说明
将检查结果转换为 Excel 格式的报告，包含摘要页和详细页。

#### 使用方法

```r
generate_excel_report(
  checks_df,                      # 检查结果数据框
  output_file,                    # 输出文件路径（必填）
  include_no_deviation = FALSE,   # 是否包含无偏离的检查
  title = "Study Deviation Report" # 报告标题
)
```

---

## 9. 完整工作流程示例

以下是一个完整的方案偏离检查工作流程示例：

```r
library(pdchecker)

# ============================================
# 步骤1：读取数据
# ============================================

# 读取 SAS 数据
data <- read_raw_data(
  folder = "path/to/sas/files",
  iwrs_file = "path/to/iwrs.csv"
)

# 读取访视计划
visit_schedule <- read_visitcode_file("path/to/visit_schedule.xlsx")

# ============================================
# 步骤2：生成计划访视日期
# ============================================

visitcode <- read_visitcode_file("visit_schedule.xlsx")
planned_dates <- generate_planned_visit_dates(
  data = data,
  cycle_days = 28
)

# ============================================
# 步骤3：执行各项检查
# ============================================

# 检查 ICF 时间偏离
icf_result <- check_icf_time_deviation(data)

# 检查未签署知情同意
ic_result <- check_screen_without_ic(data)

# 检查遗漏访视
missing_visit_result <- check_missing_visit(
  planned_dates = planned_dates,
  cutoffdt = as.Date("2024-12-31")
)

# 检查访视超窗
window_result <- check_visit_window(planned_dates)

# 读取检查配置并准备检查数据
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
prepared_lb <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  test_date_var = "LBDAT",
  test_cat_var = "LBCAT"
)
missing_test_result <- check_missing_test(prepared_lb)

# ============================================
# 步骤4：查看结果
# ============================================

# 打印各检查结果
print(icf_result)
print(ic_result)
print(missing_visit_result)
print(window_result)
print(missing_test_result)

# ============================================
# 步骤5：合并结果并生成报告
# ============================================

# 使用 combine_check_results 合并结果
all_results <- combine_check_results(
  icf_result,
  ic_result,
  missing_visit_result,
  window_result,
  missing_test_result
)

# 生成 Excel 报告
generate_excel_report(
  checks_df = all_results,
  output_file = "deviation_report.xlsx",
  title = "方案偏离检查报告"
)

# 生成 HTML 报告
generate_html_report(
  checks_df = all_results,
  output_file = "deviation_report.html",
  title = "方案偏离检查报告"
)
```

---

## 10. 常见问题解答

### Q1: 如何处理中文编码问题？

使用 `read_raw_data_with_formats()` 时，可以指定 encoding 参数：

```r
data <- read_raw_data_with_formats(
  data_dir = "path/to/data",
  catalog_file = "path/to/formats.sas7bcat",
  encoding = "GBK"  # 或 "GB2312", "GB18030"
)
```

### Q2: 如何只检查特定受试者？

在 `prepare_test_data()` 中使用 `filter_cond` 参数：

```r
prepared_data <- prepare_test_data(
  data = data,
  test_dataset = "LB",
  filter_cond = "ENROL|ENRYN=='Y'"  # 只检查已入组受试者
)
```

### Q3: 如何自定义方案偏离编号？

所有检查函数都支持 `pdno` 参数：

```r
result <- check_icf_time_deviation(data, pdno = "PD-001")
```

### Q4: 如何忽略特定的日期变量检查？

在 `check_icf_time_deviation()` 中使用 `ignore_vars` 参数：

```r
result <- check_icf_time_deviation(
  data,
  ignore_vars = c("BRTHDAT", "MHSTDAT", "DIAG_DATE")
)
```

---

## 11. 版本信息

本手册基于 pdchecker 包当前开发版本编写。如有更新，请参阅包的 NEWS.md 文件。

---

*文档最后更新：2026年2月*
