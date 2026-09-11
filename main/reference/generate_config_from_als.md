# Generate Visit and Test Config Files from ALS Using AI

Read an ALS (eCRF) Excel workbook, use an LLM to identify visit schedule
and visit-matrix structure across projects, and generate the two
external configuration files required by pdchecker.

## Usage

``` r
generate_config_from_als(
  als_file,
  visitcode_file = NULL,
  testconfig_file = NULL,
  output_dir = NULL,
  write_files = TRUE,
  overwrite = FALSE,
  exclude_visits = c("共同页", "计划外访视"),
  exclude_forms = NULL,
  ai_instruction = NULL,
  verbose = FALSE
)
```

## Arguments

- als_file:

  Character. Path to the ALS Excel file (.xlsx).

- visitcode_file:

  Character. Output path for visit schedule file. If `NULL` and
  `write_files = TRUE`, writes `visitcode.xlsx` under `output_dir`.

- testconfig_file:

  Character. Output path for test config file. If `NULL` and
  `write_files = TRUE`, writes `testconfig.xlsx` under `output_dir`.

- output_dir:

  Character. Directory for output files when paths are `NULL`. Defaults
  to the directory containing `als_file`.

- write_files:

  Logical. If `TRUE`, write Excel files to disk (default).

- overwrite:

  Logical. If `FALSE` (default), stop when output files already exist.

- exclude_visits:

  Character vector. Visit names to exclude from visitcode (default:
  `c("共同页", "计划外访视")`).

- exclude_forms:

  Character vector. Form labels to exclude from testconfig.

- ai_instruction:

  Character. Optional hints for AI (sheet/column names, exclusions).

- verbose:

  Logical. Print raw AI response.

## Value

An invisible list with elements:

- visitcode:

  Data frame for `read_visitcode_file`

- testconfig:

  Data frame for `read_testconfig_file`

- visitcode_file:

  Path written, or `NULL`

- testconfig_file:

  Path written, or `NULL`

- ai_notes:

  Character, notes from AI on sheet/column mapping

## Details

The function serializes all worksheet content (truncated per sheet) and
sends it to an OpenAI-compatible API. The model returns JSON with
`visitcode` and `testconfig` tables compatible with
[`read_visitcode_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_visitcode_file.md)
and
[`read_testconfig_file`](https://insightsengineering.github.io/r.pkg.template/reference/read_testconfig_file.md).

Set `OPENROUTER_API_KEY`, `OPENROUTER_BASE_URL`, and `OPENROUTER_MODEL`
in `.Renviron`. Use `ai_instruction` to describe project-specific sheet
names or column meanings when layouts differ.

### What AI Identifies (priority)

**visitcode** — from the visit schedule sheet (访视 / Visit / Folders):

- `VISIT`: visit name

- `VISITNUM`: visit code/number (must match EDC, not matrix column
  order)

- `VISITDAY`: inferred from `VISIT` when absent in ALS (e.g. D-28, D1,
  EOT)

- `CYCLE`: inferred from `VISIT` —
  预激、预治疗、筛选期、治疗期1/治疗期2…、 治疗结束、研究结束、随访期;
  unrecognized visits left empty

- `WP`: left **empty** for manual completion

**testconfig** — from the eCRF visit matrix (检查类 only):

- `TESTCAT`: Chinese only, strip parenthetical codes (血常规, not
  血常规(LB) or LB)

- Excludes visit admin, ICF, demographics, medical history, AE,
  concomitant meds, etc.

- `VISITNUM`: comma-separated visit numbers for required checks

- `FORM` kept for reference when editing

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- generate_config_from_als(
  als_file = system.file("extdata", "ALS_VISIT.xlsx", package = "pdchecker"),
  output_dir = tempdir(),
  ai_instruction = "访视编号在访视 sheet 的 VISITNUM 列"
)
visitcode <- read_visitcode_file(cfg$visitcode_file)
testconfig <- read_testconfig_file(cfg$testconfig_file, visitcode = visitcode)
} # }
```
