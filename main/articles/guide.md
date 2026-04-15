# pdchecker 包使用手册

## 1. 概述

`pdchecker` 是一个用于临床试验方案偏离（Protocol Deviation, PD）检查的 R
包。它提供了一套完整的工作流程，帮助临床数据管理人员自动化检测和报告数据中的常见方案偏离问题。

**核心功能：**

- **知情同意类检查**：检测 ICF 签署前操作、未签署知情同意等问题
- **访视合规类检查**：检测遗漏访视、访视超窗等问题
- **检查项缺失类检查**：检测实验室检查、体征检查等缺失情况
- **报告生成**：将检查结果导出为 Excel 格式报告

## 2. 准备工作

在执行方案偏离检查之前，需要完成数据读取、加载配置文件、设置全局选项（可选）等准备工作。

### 2.1 read_raw_data_with_formats() — 使用 SAS catalog 读取（推荐）

使用 SAS catalog
文件（`.sas7bcat`）读取数据并进行编码映射，适用于数据中包含编码值需要转换的场景。

> **推荐**：优先使用
> [`read_raw_data_with_formats()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data_with_formats.md)
> 而非
> [`read_raw_data()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data.md)。该函数通过
> SAS catalog 文件自动完成编码值到标签的映射，无需额外准备 Excel
> 格式映射文件，流程更简洁，映射也更准确。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data_dir` | 字符串 | （必填） | SAS 数据文件（`.sas7bdat`）所在目录 |
| `catalog_file` | 字符串 | （必填） | SAS 格式目录文件（`.sas7bcat`）路径，用于编码值到标签的映射 |
| `iwrs_file` | 字符串 | `NULL` | IWRS CSV 文件路径（可选）。该文件前 2 行为表头，会自动跳过；若为 `NULL` 或文件不存在则跳过 |
| `encoding` | 字符串 | `"UTF-8"` | 读取 SAS 文件和 catalog 文件时使用的字符编码，中文环境可改为 `"GBK"` |

**返回值：** 命名列表，每个元素为一个数据框，数据集名称取自 SAS
文件名（转为大写），所有列名统一转为大写；带 SAS format
标签的列自动转换为因子。若包含 IWRS 文件，第一个元素为 `iwrs`。

``` r

data <- read_raw_data_with_formats(
  data_dir     = "path/to/sas/files",
  catalog_file = "path/to/formats.sas7bcat",
  encoding     = "UTF-8"               # 默认使用 UTF-8 编码
)

# 读取后按数据集名称访问
dm <- data$DM
ae <- data$AE
```

------------------------------------------------------------------------

### 2.2 read_raw_data() — 读取 SAS 数据

扫描指定目录，批量读取所有 SAS 格式文件（`.sas7bdat`），可选附加 IWRS
CSV 和 Excel 格式映射文件。

**参数：**

| 参数          | 类型   | 默认值   | 说明                                         |
|---------------|--------|----------|----------------------------------------------|
| `folder`      | 字符串 | （必填） | SAS 文件所在目录                             |
| `iwrs_file`   | 字符串 | `NULL`   | IWRS CSV 文件路径（前 2 行为表头会自动跳过） |
| `format_file` | 字符串 | `NULL`   | Excel 格式映射文件路径（`.xlsx`）            |

**返回值：** 命名列表，每个元素是一个数据框，名称为大写的数据集名（如
`DM`、`AE`、`LB`）。

``` r

# 基本用法
data <- read_raw_data(folder = "path/to/sas/files")

# 附加 IWRS 和格式映射
data <- read_raw_data(
  folder      = "path/to/sas/files",
  iwrs_file   = "path/to/iwrs.csv",
  format_file = "path/to/formats.xlsx"
)

# 访问单个数据集
dm <- data$DM
ae <- data$AE
lb <- data$LB
```

**注意事项：** - 所有列名统一转换为大写 - 格式映射优先级：Excel 格式文件
\> SAS 格式文件 - 读取失败的数据集会自动跳过并发出警告

------------------------------------------------------------------------

### 2.3 read_visitcode_file() — 读取访视计划文件

读取访视计划 Excel 或 CSV 文件，自动解析窗口期（WP 列），生成 `type` 和
`wpvalue` 两个新列。

**参数：**

| 参数         | 类型   | 默认值     | 说明                              |
|--------------|--------|------------|-----------------------------------|
| `file_path`  | 字符串 | （必填）   | 文件路径（`.xlsx`/`.xls`/`.csv`） |
| `sheet_name` | 字符串 | `"Sheet1"` | Excel 工作表名（CSV 忽略）        |

**返回值：** 数据框，包含原文件所有列，加上自动生成的
`type`、`wpvalue`、`visit_category`（若有 CYCLE 列）。

**输入文件格式要求：**

| 列名     | 说明     | 示例      |
|----------|----------|-----------|
| VISIT    | 访视名称 | C1D1      |
| VISITNUM | 访视编号 | 1         |
| WP       | 窗口期   | ±3d       |
| CYCLE    | 周期描述 | 治疗周期1 |
| VISITDAY | 访视日   | 8         |

**WP 列支持的格式：**

| 格式              | 解析为 type | 解析为 wpvalue  |
|-------------------|-------------|-----------------|
| `±3d` 或 `+/-3d`  | `+/-`       | `3`             |
| `+7d`             | `+`         | `7`             |
| `-3d`             | `-`         | `3`             |
| `≤3d` 或 `<=3d`   | `≤`         | `3`             |
| `≥3d` 或 `>=3d`   | `≥`         | `3`             |
| `24h` 或 `24小时` | `≤`         | `1`（自动 ÷24） |
| `2w` 或 `2周`     | `+/-`       | `14`（自动 ×7） |
| 空值              | `NA`        | `NA`            |

**VISITDAY 列支持的格式：**

| 格式 | 适用访视类别 | 含义 |
|----|----|----|
| 正整数 | treatment | 相对于周期 D1 的天数（如 `1`、`8`、`15`） |
| 负整数 | screening | 相对于首次给药的天数（如 `-28`） |
| `FD` 或 `First Dose` | pre_treatment | 首次给药日期 |
| `LD+N` 或 `Last Dose+N` | follow_up | 末次给药后 N 天（如 `LD+28`） |
| `EOT` | end_of_treatment | 治疗结束日期 |
| `EOT+N` | end_of_treatment / follow_up | 治疗结束后 N 天（如 `EOT+7`） |
| `EOS` | end_of_study | 研究结束日期 |

**CYCLE 列自动识别访视类别：**

| CYCLE 包含关键词                   | 识别为           |
|------------------------------------|------------------|
| 筛选 / Screening                   | screening        |
| 预激 / 预治疗                      | pre_treatment    |
| 治疗（不含”治疗结束”）/ Cycle      | treatment        |
| 治疗结束 / 退出 / End of Treatment | end_of_treatment |
| 研究结束 / End of Study            | end_of_study     |
| 随访 / Follow                      | follow_up        |

**示例文件（visit_schedule.xlsx）：**

| VISIT    | VISITNUM | WP  | CYCLE     | VISITDAY |
|----------|----------|-----|-----------|----------|
| 筛选访视 | 0        |     | 筛选      | -28      |
| C1D1     | 1        | ±1d | 治疗周期1 | 1        |
| C1D8     | 2        | ±2d | 治疗周期1 | 8        |
| C1D15    | 3        | ±2d | 治疗周期1 | 15       |
| C2D1     | 4        | ±3d | 治疗周期2 | 1        |
| C2D8     | 5        | ±2d | 治疗周期2 | 8        |
| C3D1     | 6        | ±3d | 治疗周期3 | 1        |
| EOT      | 99       | +7d | 治疗结束  | EOT      |
| FU1      | 100      | ±7d | 随访      | LD+28    |
| FU2      | 101      | ±7d | 随访      | LD+56    |
| EOS      | 999      |     | 研究结束  | EOS      |

``` r

visitcode <- read_visitcode_file("visit_schedule.xlsx")

# 指定工作表
visitcode <- read_visitcode_file("visit_schedule.xlsx", sheet_name = "Schedule")

# 从 CSV 读取
visitcode <- read_visitcode_file("visit_schedule.csv")
```

> **提示**：建议将结果赋值给变量名 `visitcode`，后续的
> [`generate_planned_visit_dates()`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md)
> 会自动在环境中查找该变量。

------------------------------------------------------------------------

### 2.4 read_testconfig_file() — 读取检查项配置文件

读取检查项配置文件，定义每个检查类别在哪些访视执行。VISITNUM
列支持逗号分隔，函数会自动展开为多行。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `file_path` | 字符串 | （必填） | 文件路径（`.xlsx`/`.xls`/`.csv`） |
| `sheet_name` | 字符串 | `"Sheet1"` | Excel 工作表名 |
| `visitcode` | 数据框 | `NULL` | [`read_visitcode_file()`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md) 的返回值，用于关联 VISIT 列 |

**返回值：** 数据框，包含 TESTCAT、VISITNUM（展开后每行一个），以及
VISIT（若提供了 visitcode）。

**输入文件格式要求：**

| 列名     | 说明                         | 示例       |
|----------|------------------------------|------------|
| TESTCAT  | 检查类别名称                 | 血常规     |
| VISITNUM | 需执行的访视编号（逗号分隔） | 0,1,4,6,99 |

**示例文件（test_config.xlsx）：**

| TESTCAT  | VISITNUM         |
|----------|------------------|
| 血常规   | 0,1,4,6,99       |
| 生化     | 0,1,4,6,99       |
| 尿常规   | 0,1,99           |
| 凝血功能 | 0,1,99           |
| 心电图   | 0,1,4,6,99       |
| 生命体征 | 0,1,2,3,4,5,6,99 |

``` r

# 推荐用法：传入 visitcode 以关联访视名称
visitcode  <- read_visitcode_file("visit_schedule.xlsx")
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
```

> **提示**：建议将结果赋值给变量名 `testconfig`，后续的
> [`prepare_test_data()`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md)
> 会自动在环境中查找该变量。

------------------------------------------------------------------------

### 2.5 set_pdchecker_options() / get_pdchecker_options() — 全局选项配置

不同项目的数据集名称和变量名可能不同。使用
[`set_pdchecker_options()`](https://insightsengineering.github.io/r.pkg.template/reference/set_pdchecker_options.md)
可以在脚本开头统一设置，避免后续每个函数都重复指定参数。

``` r

set_pdchecker_options(
  sv_dataset      = "SV",       # 访视数据集
  sv_visit_var    = "VISIT",    # 访视名称变量
  sv_visitnum_var = "VISITNUM", # 访视编号变量
  sv_date_var     = "SVDAT",  # 访视日期变量（项目中可能不同）
  ex_datasets     = "EX",       # 用药数据集（可多个数据集）
  ex_date_var     = "EXSTDAT",  # 用药日期变量
  ic_dataset      = "IC",       # 知情同意数据集
  ic_date_var     = "ICDAT",    # 知情同意日期变量
)

# 设置后，所有函数自动使用这些值
planned_dates <- generate_planned_visit_dates(data = data)

# 查看当前所有全局选项
get_pdchecker_options()
```

**优先级**：函数调用时直接传参 \> 全局选项 \>
默认值。即使设置了全局选项，仍可在个别调用中传参覆盖。

完整的全局选项列表请参见 [附录
A.1](#a1-%E5%85%A8%E5%B1%80%E9%80%89%E9%A1%B9%E5%AE%8C%E6%95%B4%E5%88%97%E8%A1%A8)。

------------------------------------------------------------------------

## 3. 日期提取函数

日期提取函数从临床数据中提取关键日期，返回包含 `SUBJID`
和日期列的数据框。这些函数由
[`generate_planned_visit_dates()`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md)
内部调用，通常不需要手动调用，但在需要单独获取日期时可以使用。

### 3.1 get_first_dose_date() — 首次给药日期

从用药数据集中提取每位受试者最早的给药开始日期。

**参数：**

| 参数          | 类型     | 默认值      | 说明                     |
|---------------|----------|-------------|--------------------------|
| `data`        | 列表     | （必填）    | 临床数据列表             |
| `ex_datasets` | 字符向量 | `"EX"`      | 用药数据集名称，支持多个 |
| `ex_date_var` | 字符向量 | `"EXSTDAT"` | 给药开始日期变量名       |

**返回值：** 数据框，包含 `SUBJID` 和 `first_dose_date` 列。

``` r

# 单个用药数据集
first_dates <- get_first_dose_date(data)

# 多个用药数据集
first_dates <- get_first_dose_date(data, ex_datasets = c("EX1", "EX2"))

# 不同数据集使用不同变量名
first_dates <- get_first_dose_date(
  data,
  ex_datasets = c("EX1", "EX2"),
  ex_date_var = c("STDAT1", "STDAT2")
)
```

------------------------------------------------------------------------

### 3.2 get_last_dose_date() — 末次给药日期

从用药数据集中提取每位受试者最晚的给药日期。默认使用开始日期，也可指定结束日期。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data` | 列表 | （必填） | 临床数据列表 |
| `ex_datasets` | 字符向量 | `"EX"` | 用药数据集名称 |
| `ex_date_var` | 字符向量 | `"EXSTDAT"` | 给药开始日期变量名 |
| `ex_end_date_var` | 字符向量 | `NULL` | 给药结束日期变量名（指定后使用结束日期） |

**返回值：** 数据框，包含 `SUBJID` 和 `last_dose_date` 列。

``` r

# 使用开始日期作为末次给药日期
last_dates <- get_last_dose_date(data)
```

------------------------------------------------------------------------

### 3.3 get_eot_date() — 治疗结束日期

从治疗结束数据集中提取每位受试者的 EOT 日期。

**参数：**

| 参数           | 类型   | 默认值     | 说明           |
|----------------|--------|------------|----------------|
| `data`         | 列表   | （必填）   | 临床数据列表   |
| `eot_dataset`  | 字符串 | `"EOT"`    | EOT 数据集名称 |
| `eot_date_var` | 字符串 | `"EOTDAT"` | EOT 日期变量名 |

**返回值：** 数据框，包含 `SUBJID` 和 `eot_date` 列。

``` r

eot_dates <- get_eot_date(data)
```

------------------------------------------------------------------------

### 3.4 get_eos_date() — 研究结束日期

从受试者处置数据集中提取每位受试者的 EOS 日期。

**参数：**

| 参数          | 类型   | 默认值    | 说明           |
|---------------|--------|-----------|----------------|
| `data`        | 列表   | （必填）  | 临床数据列表   |
| `ds_dataset`  | 字符串 | `"DS"`    | DS 数据集名称  |
| `ds_date_var` | 字符串 | `"DSDAT"` | EOS 日期变量名 |

**返回值：** 数据框，包含 `SUBJID` 和 `eos_date` 列。

``` r

eos_dates <- get_eos_date(data)
```

------------------------------------------------------------------------

## 4. 数据准备函数

### 4.1 generate_planned_visit_dates() — 生成计划访视日期

根据访视计划和实际临床数据，为每位受试者计算计划访视日期和窗口范围，并标记
`status`（completed /
missing）。这是访视合规类检查（[`check_missing_visit()`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_visit.md)、[`check_visit_window()`](https://insightsengineering.github.io/r.pkg.template/reference/check_visit_window.md)）的前置步骤。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data` | 列表 | （必填） | 临床数据列表 |
| `visitcode` | 数据框 | `NULL` | 访视计划（[`read_visitcode_file()`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md) 的返回值），为 NULL 时自动查找环境中的 `visitcode` 变量 |
| `ex_datasets` | 字符向量 | `"EX"` | 用药数据集名称 |
| `ex_date_var` | 字符向量 | `"EXSTDAT"` | 给药开始日期变量名 |
| `ex_end_date_var` | 字符向量 | `NULL` | 给药结束日期变量名 |
| `sv_dataset` | 字符串 | `"SV"` | 访视数据集名称 |
| `sv_visit_var` | 字符串 | `"VISIT"` | 访视名称变量 |
| `sv_visitnum_var` | 字符串 | `"VISITNUM"` | 访视编号变量 |
| `sv_date_var` | 字符串 | `"SVDAT"` | 访视日期变量 |
| `eot_dataset` | 字符串 | `"EOT"` | EOT 数据集名称 |
| `eot_date_var` | 字符串 | `"EOTDAT"` | EOT 日期变量 |
| `ds_dataset` | 字符串 | `"DS"` | DS 数据集名称 |
| `ds_date_var` | 字符串 | `"DSDAT"` | EOS 日期变量 |
| `cycle_days` | 数值 | `28` | 治疗周期天数 |

**返回值：** 数据框，每行代表一位受试者的一个计划访视，详见 [附录
A.2](#a2-%E8%BF%94%E5%9B%9E%E5%80%BC%E5%AD%97%E6%AE%B5%E8%AF%B4%E6%98%8E)。

**计划日期计算规则：**

| 访视类别           | 计算方式                                       |
|--------------------|------------------------------------------------|
| 筛选期 / 预治疗期  | 首次给药日期 + visitday                        |
| 治疗期 C1D1        | 优先使用实际访视日期；若缺失则使用首次给药日期 |
| 治疗期 CnD1（n≥2） | 上一周期 D1 日期 + cycle_days                  |
| 治疗期非 D1 访视   | 当前周期 D1 日期 + visitday − 1                |
| 治疗结束           | EOT 日期                                       |
| 随访               | EOT 或末次给药日期 + visitday                  |

**窗口期计算规则：**

| 窗口类型 | 窗口开始     | 窗口结束     |
|----------|--------------|--------------|
| ±N 天    | 计划日期 − N | 计划日期 + N |
| +N 天    | 计划日期     | 计划日期 + N |
| −N 天    | 计划日期 − N | 计划日期     |
| ≤N 天    | 计划日期 − N | 计划日期     |
| ≥N 天    | 计划日期     | 计划日期 + N |

``` r

visitcode <- read_visitcode_file("visit_schedule.xlsx")

# 基本用法（自动查找环境中的 visitcode）
planned_dates <- generate_planned_visit_dates(data = data, cycle_days = 28)

# 显式传入 visitcode
planned_dates <- generate_planned_visit_dates(
  data      = data,
  visitcode = visitcode,
  cycle_days = 21
)

# 多用药数据集
planned_dates <- generate_planned_visit_dates(
  data        = data,
  ex_datasets = c("EX", "EX2"),
  cycle_days  = 28
)
```

------------------------------------------------------------------------

### 4.2 prepare_test_data() — 准备检查项数据

从数据列表中提取指定的检查数据集，与访视数据合并，生成标准化的数据框供
[`check_missing_test()`](https://insightsengineering.github.io/r.pkg.template/reference/check_missing_test.md)
使用。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data` | 列表 | （必填） | 临床数据列表 |
| `test_dataset` | 字符串 | （必填） | 要提取的检查数据集名称（如 `"LB"`） |
| `test_date_var` | 字符串 | `"LBDAT"` | 检查日期变量 |
| `test_yn_var` | 字符串 | `"YN"` | 是否执行变量 |
| `test_result_var` | 字符串 | `"ORRES"` | 检查结果变量 |
| `test_cat_var` | 字符串 | `"LBCAT"` | 检查类别变量 |
| `test_de_var` | 字符串 | `NULL` | 检查指标变量（用于指标级别检查） |
| `tb_name_var` | 字符串 | `NULL` | 表名变量（为 NULL 时使用 `test_dataset` 值） |
| `sv_dataset` | 字符串 | `"SV"` | 访视数据集名称 |
| `sv_visit_var` | 字符串 | `"VISIT"` | 访视名称变量 |
| `sv_visitnum_var` | 字符串 | `"VISITNUM"` | 访视编号变量 |
| `sv_date_var` | 字符串 | `"SVDAT"` | 访视日期变量 |
| `config` | 数据框 | `NULL` | 检查配置（[`read_testconfig_file()`](https://insightsengineering.github.io/r.pkg.template/reference/read_testconfig_file.md) 的返回值），为 NULL 时自动查找环境中的 `testconfig` 变量 |
| `config_cat` | 字符向量 | `NULL` | 只检查指定的检查类别（为 NULL 时检查全部） |
| `filter_cond` | 字符串 | `NULL` | 受试者筛选条件 |

**返回值：** 标准化的数据框，列名统一为
`SUBJID`、`VISIT`、`VISITNUM`、`TBNAME`、`TESTCAT`、`TESTDE`、`TESTYN`、`TESTDAT`、`ORRES`
等。

``` r
testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)

# 准备实验室检查数据
prepared_lb <- prepare_test_data(
  data            = data,
  test_dataset    = "LB",
  test_date_var   = "LBDAT",
  test_cat_var    = "LBCAT",
  test_de_var     = "LBTEST",
  test_result_var = "ORRES",
  config_cat      = c("血常规", "血生化") 
)

# 准备心电图数据
prepared_eg <- prepare_test_data(
  data          = data,
  test_dataset  = "EG",
  test_date_var = "EGDAT",
  test_cat_var  = "EGCAT"，
  config_cat    = c("心电图")
)

# 对于"宽数据"如果要查具体检查项的缺失，可以先转置为"长数据"，并添加到raw中，再使用prepare_test_data 准备数据
# 例如："生命体征"
data$VSLONG <- pivot_longer(data$VS,
  cols = c("VSSBP", "VSDBP", "VSRESP", "VSPUL", "VSTEMP"),
  names_to = "VSTESTCD",
  values_to = "VSORRES"
) %>%
  mutate(VSTEST = case_when(
    VSTESTCD == "VSSBP" ~ "收缩压",
    VSTESTCD == "VSDBP" ~ "舒张压",
    VSTESTCD == "VSRESP" ~ "呼吸",
    VSTESTCD == "VSPUL" ~ "脉搏",
    VSTESTCD == "VSTEMP" ~ "体温"
  )
)

prepared_vs <- prepare_test_data(data,
  test_dataset = "VSLONG",
  test_date_var = "VSDAT",
  test_result_var = "VSORRES",
  test_yn_var = "VSYN",
  test_cat_var = "TNAME",
  test_de_var = "VSTEST",
  config_cat = c("生命体征")
)
```

**`filter_cond` 筛选受试者：** 格式为
`"数据集名|筛选表达式"`，多条件用分号分隔（取交集）。

``` r

# 只检查已入组受试者
prepared_lb <- prepare_test_data(
  data         = data,
  test_dataset = "LB",
  filter_cond  = "ENROL|ENRYN=='Y'"
)

# 多条件交集
prepared_lb <- prepare_test_data(
  data         = data,
  test_dataset = "LB",
  filter_cond  = "ENROL|ENRYN=='Y';SUBJECT|SEX=='M'"
)
```

------------------------------------------------------------------------

## 5. 知情同意类检查

知情同意类检查用于发现受试者在知情同意流程中的方案偏离。

> **返回值结构**：所有 `check_*` 函数返回一个统一结构的命名列表：
>
> | 元素 | 类型 | 说明 |
> |----|----|----|
> | `has_deviation` | 逻辑值 | 是否存在偏离 |
> | `messages` | 字符向量 | 偏离摘要信息 |
> | `details` | 数据框 | 偏离明细记录（各检查函数的列有所不同，详见 [附录 A.2](#a2-%E8%BF%94%E5%9B%9E%E5%80%BC%E5%AD%97%E6%AE%B5%E8%AF%B4%E6%98%8E)） |

### 5.1 check_icf_time_deviation() — ICF 签署前操作

遍历所有数据集中以 `DAT`
结尾的日期变量，检查是否有操作日期早于知情同意书签署日期。

**判断规则**：事件日期 \< ICF 签署日期 → 偏离

**参数：**

| 参数               | 类型     | 默认值      | 说明                         |
|--------------------|----------|-------------|------------------------------|
| `data`             | 列表     | （必填）    | 临床数据列表                 |
| `ic_dataset`       | 字符串   | `"IC"`      | 知情同意数据集名称           |
| `ic_date_var`      | 字符串   | `"ICDAT"`   | 知情同意日期变量             |
| `ignore_vars`      | 字符向量 | `"BRTHDAT"` | 忽略的日期变量（如出生日期） |
| `exclude_datasets` | 字符向量 | `NULL`      | 要排除的数据集               |
| `tb_name_var`      | 字符串   | `NULL`      | 显示表名的变量               |
| `pdno`             | 字符串   | `"2.1.1"`   | 方案偏离编号                 |

``` r

# 基本用法
result <- check_icf_time_deviation(data)
print(result)

# 排除不需要检查的数据集
result <- check_icf_time_deviation(
  data,
  exclude_datasets = c("DM", "DS")
)

# 忽略额外的日期变量
result <- check_icf_time_deviation(
  data,
  ignore_vars = c("BRTHDAT", "MHSTDAT", "DSSTDAT")
)

# 查看偏离明细
if (result$has_deviation) {
  print(result$details)
  # details 包含：PDNO, SUBJID, action, event_datetime, icf_datetime, diff_date, TBNAME, DESCRIPTION
}
```

------------------------------------------------------------------------

### 5.2 check_screen_without_ic() — 未签署知情同意

检查是否存在有筛选访视记录但在 IC 数据集中无知情同意日期的受试者。

**判断规则**：受试者有筛选访视记录，但 IC 数据集中无对应 ICF 日期 → 偏离

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data` | 列表 | （必填） | 临床数据列表 |
| `sv_dataset` | 字符串 | `"SV"` | 访视数据集名称 |
| `ic_dataset` | 字符串 | `"IC"` | 知情同意数据集名称 |
| `sv_visit_var` | 字符串 | `"VISIT"` | 访视名称变量 |
| `visit_pattern` | 字符串 | `"Screening\|screening"` | 筛选访视匹配模式（正则表达式） |
| `ic_date_var` | 字符串 | `"ICDAT"` | 知情同意日期变量 |
| `pdno` | 字符串 | `"2.4.1"` | 方案偏离编号 |

``` r

result <- check_screen_without_ic(data)
print(result)

# 匹配中文访视名
result <- check_screen_without_ic(
  data,
  visit_pattern = "Screening|screening|筛选"
)

# 查看偏离明细
if (result$has_deviation) {
  print(result$details)
  # details 包含：PDNO, SUBJID, VISIT, DESCRIPTION
}
```

------------------------------------------------------------------------

## 6. 访视合规类检查

访视合规类检查用于发现受试者在访视执行中的方案偏离。

> **前置条件**：需要先调用
> [`generate_planned_visit_dates()`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md)
> 生成计划访视日期，详见 [4.1
> 节](#id_41-generate_planned_visit_dates--%E7%94%9F%E6%88%90%E8%AE%A1%E5%88%92%E8%AE%BF%E8%A7%86%E6%97%A5%E6%9C%9F)。

### 6.1 check_missing_visit() — 遗漏访视

基于计划访视数据，判断哪些应已完成但实际缺失的访视构成方案偏离。

**判断规则：**

| 访视类别                   | 应完成的条件                                  |
|----------------------------|-----------------------------------------------|
| 筛选期 / 预治疗期 / 治疗期 | 计划日期 \< min(EOT 日期, EOS 日期, 截止日期) |
| 治疗结束 / 随访            | 计划日期 ≤ min(EOS 日期, 截止日期)            |

满足应完成条件且 `status == "missing"` → 偏离

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `planned_dates` | 数据框 | （必填） | [`generate_planned_visit_dates()`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md) 的返回值 |
| `cutoffdt` | 日期 | [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) | 数据截止日期 |
| `pdno` | 字符串 | `"8.2.1"` | 方案偏离编号 |

``` r

result <- check_missing_visit(planned_dates)
print(result)

# 指定截止日期
result <- check_missing_visit(
  planned_dates,
  cutoffdt = as.Date("2024-12-31")
)

# 查看偏离明细
if (result$has_deviation) {
  print(result$details)
  # details 包含：PDNO, SUBJID, VISIT, VISITNUM, visit_category, planned_date, DESCRIPTION
}
```

------------------------------------------------------------------------

### 6.2 check_visit_window() — 访视超窗

对已完成的访视，检查实际访视日期是否在规定窗口期内。

**判断规则：** - 合规：`wp_start ≤ actual_date ≤ wp_end` -
偏离：`actual_date < wp_start`（提前超窗）或
`actual_date > wp_end`（延迟超窗） -
无窗口期（`wp_type == NA`）的访视不参与检查

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `planned_dates` | 数据框 | （必填） | [`generate_planned_visit_dates()`](https://insightsengineering.github.io/r.pkg.template/reference/generate_planned_visit_dates.md) 的返回值 |
| `pdno` | 字符串 | `"8.4.1"` | 方案偏离编号 |

``` r

result <- check_visit_window(planned_dates)
print(result)

# 查看偏离明细
if (result$has_deviation) {
  # 查看延迟超窗
  delayed <- result$details[result$details$deviation_days > 0, ]
  print(delayed)

  # 查看提前超窗
  early <- result$details[result$details$deviation_days < 0, ]
  print(early)

  # details 包含：PDNO, SUBJID, VISIT, planned_date, actual_date,
  #               wp_start, wp_end, deviation_days, DESCRIPTION
}
```

------------------------------------------------------------------------

## 7. 检查项缺失类检查

检查项缺失类检查用于发现受试者在已完成的访视中，是否遗漏了方案规定的检查项目。

> **前置条件**：需要先调用
> [`prepare_test_data()`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md)
> 准备检查数据，详见 [4.2
> 节](#id_42-prepare_test_data--%E5%87%86%E5%A4%87%E6%A3%80%E6%9F%A5%E9%A1%B9%E6%95%B0%E6%8D%AE)。

### 7.1 check_missing_test() — 检查项缺失

对每位受试者的已完成访视，检查是否执行了配置文件中要求的检查项目。

**三种缺失类型：**

| 缺失类型 | 判断条件 | 含义 |
|----|----|----|
| `TESTCAT_EMPTY` | 该访视无任何 TESTCAT 记录 | 整个访视的检查数据缺失 |
| `TESTCAT_MISSING` | 有记录但 TESTDAT 为空，或 TESTYN ≠ “是” | 检查项目未执行 |
| `TESTDE_MISSING` | 有记录且 TESTDAT 非空，但 ORRES 为空 | 具体指标结果缺失（仅 `missing_de = TRUE`） |

> **注意**：`TESTYN` 字段使用中文 **“是”** 表示已执行。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `data` | 数据框 | （必填） | [`prepare_test_data()`](https://insightsengineering.github.io/r.pkg.template/reference/prepare_test_data.md) 的返回值 |
| `test_var` | 字符串 | `NULL` | 筛选变量名（如 `"TESTCAT"`），为 NULL 检查全部 |
| `test` | 字符串或字符向量 | `NULL` | 筛选值，支持单选或多选（如 `"血常规"` 或 `c("血常规", "血生化")`） |
| `missing_de` | 逻辑值 | `TRUE` | 是否检查指标级别缺失 |
| `pdno` | 字符串 | `"8.3.1"` | 方案偏离编号 |

``` r

# 检查所有检查项
result <- check_missing_test(data = prepared_lb)
print(result)

# 只检查特定类别
result <- check_missing_test(
  data     = prepared_lb,
  test_var = "TESTCAT",
  test     = "血常规"
)

# 同时检查多个类别且检查具体指标缺失
result <- check_missing_test(
  data     = prepared_lb,
  test_var = "TESTCAT",
  test     = c("血常规", "血生化"),
  missing_de = TRUE
)

# 只检查整体缺失，不检查指标级别
result <- check_missing_test(
  data       = prepared_lb,
  missing_de = FALSE
)

# 查看偏离明细
if (result$has_deviation) {
  print(result$details)
  # details 包含：PDNO, SUBJID, VISIT, visit_date, TBNAME,
  #               test_name, missing_type, DESCRIPTION
}
```

**多个检查数据集的完整流程：**

``` r

# 为每种检查类型分别准备和检查
prepared_lb <- prepare_test_data(data = data, test_dataset = "LB")
prepared_vs <- prepare_test_data(data = data, test_dataset = "VS", test_date_var = "VSDAT", test_cat_var = "VSCAT")
prepared_eg <- prepare_test_data(data = data, test_dataset = "EG", test_date_var = "EGDAT", test_cat_var = "EGCAT")

result_lb <- check_missing_test(data = prepared_lb)
result_vs <- check_missing_test(data = prepared_vs)
result_eg <- check_missing_test(data = prepared_eg)

# 合并所有检查数据一起进行检查
test_data <- bind_rows(prepared_lb, prepared_vs, prepared_eg)
result <- check_missing_test(data = test_data)
```

------------------------------------------------------------------------

## 8. 结果汇总与报告生成

完成各项检查后，可以将结果汇总并导出为报告。

### 8.1 combine_check_results() — 合并检查结果

将多个检查函数的返回值合并为一个统一的数据框。每个结果先通过
[`as_check_df()`](https://insightsengineering.github.io/r.pkg.template/reference/as_check_df.md)
转为标准格式再合并。

**参数：** 任意数量的检查结果对象（`check_*` 函数的返回值），`NULL`
值自动跳过。

``` r

all_results <- combine_check_results(
  check_icf_time_deviation(data),
  check_screen_without_ic(data),
  check_missing_visit(planned_dates),
  check_visit_window(planned_dates),
  check_missing_test(prepared_lb)
)
```

------------------------------------------------------------------------

### 8.2 generate_excel_report() — Excel 报告

生成包含 Summary（汇总统计）和 All Deviations（逐条明细）两个工作表的
Excel 文件。Summary 列出全部 `check_name`（含无偏离）；All Deviations
仅含 `has_deviation == TRUE` 的行。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `checks_df` | 数据框 | （必填） | 合并后的检查结果 |
| `output_file` | 字符串 | （必填） | 输出文件路径 |
| `title` | 字符串 | `"Study Deviation Report"` | 报告标题 |
| `report_cols` | 字符向量 | 见下 | 写入「All Deviations」工作表的列名；默认含 `PDNO`、`SITEID`、`SUBJID`、`TBNAME`、`DESCRIPTION` |

``` r

generate_excel_report(
  checks_df   = all_results,
  output_file = "deviation_report.xlsx",
  title       = "方案偏离检查报告",
  report_cols = c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")
)
```

------------------------------------------------------------------------

## 9. 工具函数

### 9.1 is_sas_na() — SAS 缺失值判断

判断值是否为 SAS 缺失值（`NA`、`"."`、`""`）。

``` r

is_sas_na(NA)    # TRUE
is_sas_na(".")   # TRUE
is_sas_na("")    # TRUE
is_sas_na("abc") # FALSE
```

------------------------------------------------------------------------

### 9.2 as_check_df() — 检查结果转数据框

将单个 `check_*`
函数的返回值转换为标准化的数据框格式。[`combine_check_results()`](https://insightsengineering.github.io/r.pkg.template/reference/combine_check_results.md)
内部使用此函数。

返回的数据框始终包含以下列：`check_name`、`has_deviation`、`message`、`PDNO`、`SUBJID`、`DESCRIPTION`。当检查结果
`details` 为空时，`PDNO`、`SUBJID`、`DESCRIPTION` 列的值为 `NA`。

**参数：**

| 参数 | 类型 | 默认值 | 说明 |
|----|----|----|----|
| `check_result` | 列表 | （必填） | 检查函数的返回值 |
| `check_name` | 字符串 | `NULL` | 检查名称（为 NULL 时从对象的 class 属性提取） |

``` r

result <- check_icf_time_deviation(data)
df <- as_check_df(result, check_name = "icf_time_deviation")
```

------------------------------------------------------------------------

## 附录

### A.1 全局选项完整列表

以下参数均可通过
[`set_pdchecker_options()`](https://insightsengineering.github.io/r.pkg.template/reference/set_pdchecker_options.md)
设置。只需设置与默认值不同的参数。

| 参数 | 默认值 | 影响的函数 |
|----|----|----|
| `sv_dataset` | `"SV"` | `generate_planned_visit_dates`, `prepare_test_data`, `check_screen_without_ic` |
| `sv_visit_var` | `"VISIT"` | `generate_planned_visit_dates`, `prepare_test_data`, `check_screen_without_ic` |
| `sv_visitnum_var` | `"VISITNUM"` | `generate_planned_visit_dates`, `prepare_test_data` |
| `sv_date_var` | `"SVDAT"` | `generate_planned_visit_dates`, `prepare_test_data` |
| `ex_datasets` | `"EX"` | `get_first_dose_date`, `get_last_dose_date`, `generate_planned_visit_dates` |
| `ex_date_var` | `"EXSTDAT"` | `get_first_dose_date`, `get_last_dose_date`, `generate_planned_visit_dates` |
| `ex_end_date_var` | `NULL` | `get_last_dose_date`, `generate_planned_visit_dates` |
| `eot_dataset` | `"EOT"` | `get_eot_date`, `generate_planned_visit_dates` |
| `eot_date_var` | `"EOTDAT"` | `get_eot_date`, `generate_planned_visit_dates` |
| `ds_dataset` | `"DS"` | `get_eos_date`, `generate_planned_visit_dates` |
| `ds_date_var` | `"DSDAT"` | `get_eos_date`, `generate_planned_visit_dates` |
| `ic_dataset` | `"IC"` | `check_icf_time_deviation`, `check_screen_without_ic` |
| `ic_date_var` | `"ICDAT"` | `check_icf_time_deviation`, `check_screen_without_ic` |
| `tb_name_var` | `NULL` | `check_icf_time_deviation`, `prepare_test_data` |
| `test_date_var` | `"LBDAT"` | `prepare_test_data` |
| `test_yn_var` | `"YN"` | `prepare_test_data` |
| `test_result_var` | `"ORRES"` | `prepare_test_data` |
| `test_cat_var` | `"LBCAT"` | `prepare_test_data` |
| `test_de_var` | `NULL` | `prepare_test_data` |

------------------------------------------------------------------------

### A.2 返回值字段说明

**check_icf_time_deviation() 的 details 列：**

| 列名           | 说明                                 |
|----------------|--------------------------------------|
| PDNO           | 偏离编号                             |
| SUBJID         | 受试者编号                           |
| action         | 操作事件描述（格式：数据集.变量名）  |
| event_datetime | 事件日期                             |
| icf_datetime   | ICF 签署日期                         |
| diff_date      | 两者相差天数（负值表示事件早于 ICF） |
| TBNAME         | 数据集名称                           |
| DESCRIPTION    | 偏离描述                             |

**check_screen_without_ic() 的 details 列：**

| 列名        | 说明         |
|-------------|--------------|
| PDNO        | 偏离编号     |
| SUBJID      | 受试者编号   |
| VISIT       | 筛选访视名称 |
| DESCRIPTION | 偏离描述     |

**check_missing_visit() 的 details 列：**

| 列名           | 说明           |
|----------------|----------------|
| PDNO           | 偏离编号       |
| SUBJID         | 受试者编号     |
| VISIT          | 遗漏的访视名称 |
| VISITNUM       | 访视编号       |
| visit_category | 访视类别       |
| planned_date   | 计划访视日期   |
| DESCRIPTION    | 偏离描述       |

**check_visit_window() 的 details 列：**

| 列名           | 说明                                     |
|----------------|------------------------------------------|
| PDNO           | 偏离编号                                 |
| SUBJID         | 受试者编号                               |
| VISIT          | 访视名称                                 |
| planned_date   | 计划访视日期                             |
| actual_date    | 实际访视日期                             |
| wp_start       | 窗口开始日期                             |
| wp_end         | 窗口结束日期                             |
| deviation_days | 偏离天数（正值=延迟超窗，负值=提前超窗） |
| DESCRIPTION    | 偏离描述                                 |

**check_missing_test() 的 details 列：**

| 列名         | 说明                                                         |
|--------------|--------------------------------------------------------------|
| PDNO         | 偏离编号                                                     |
| SUBJID       | 受试者编号                                                   |
| VISIT        | 访视名称                                                     |
| visit_date   | 访视日期                                                     |
| TBNAME       | 数据集表名                                                   |
| test_name    | 缺失的检查项名称                                             |
| missing_type | 缺失类型（TESTCAT_EMPTY / TESTCAT_MISSING / TESTDE_MISSING） |
| DESCRIPTION  | 偏离描述                                                     |

**generate_planned_visit_dates() 的返回列：**

| 列名            | 说明                          |
|-----------------|-------------------------------|
| SUBJID          | 受试者编号                    |
| VISIT           | 访视名称                      |
| VISITNUM        | 访视编号                      |
| visittype       | 访视类型（周期信息）          |
| visitday        | 访视日                        |
| visit_category  | 访视类别                      |
| planned_date    | 计划访视日期                  |
| wp_start        | 窗口期开始日期                |
| wp_end          | 窗口期结束日期                |
| wp_type         | 窗口类型（±/+/-/≤/≥）         |
| wp_value        | 窗口天数                      |
| actual_date     | 实际访视日期                  |
| status          | 访视状态：completed / missing |
| first_dose_date | 首次给药日期                  |
| last_dose_date  | 末次给药日期                  |
| eot_date        | 治疗结束日期                  |
| eos_date        | 研究结束日期                  |

------------------------------------------------------------------------

### A.3 常见问题

**Q1: 如何处理中文编码问题？**

使用
[`read_raw_data_with_formats()`](https://insightsengineering.github.io/r.pkg.template/reference/read_raw_data_with_formats.md)
时，指定 `encoding` 参数：

``` r

data <- read_raw_data_with_formats(
  data_dir     = "path/to/data",
  catalog_file = "path/to/formats.sas7bcat",
  encoding     = "GBK"
)
```

**Q2: 如何自定义方案偏离编号？**

所有检查函数都支持 `pdno` 参数：

``` r

result <- check_icf_time_deviation(data, pdno = "PD-001")
```

**Q3: visitcode 和 testconfig 变量名必须固定吗？**

推荐使用这两个固定名称，函数会自动查找。也可以手动传入：

``` r

my_visit <- read_visitcode_file("visit_schedule.xlsx")
planned_dates <- generate_planned_visit_dates(data = data, visitcode = my_visit)

my_config <- read_testconfig_file("test_config.xlsx")
prepared_lb <- prepare_test_data(data = data, test_dataset = "LB", config = my_config)
```

**Q4: 如何避免每个函数都重复指定相同参数？**

使用
[`set_pdchecker_options()`](https://insightsengineering.github.io/r.pkg.template/reference/set_pdchecker_options.md)
在脚本开头统一设置，详见 [2.5
全局选项配置](#id_25-set_pdchecker_options--get_pdchecker_options--%E5%85%A8%E5%B1%80%E9%80%89%E9%A1%B9%E9%85%8D%E7%BD%AE)。
\`\`\`

------------------------------------------------------------------------

## 版本信息

本手册基于 pdchecker 包当前版本（0.9.0）编写。如有更新，请参阅包的
`NEWS.md` 文件。

------------------------------------------------------------------------

*文档最后更新：2026年4月*
