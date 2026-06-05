#' Generate Visit and Test Config Files from ALS Using AI
#'
#' @description
#' Read an ALS (eCRF) Excel workbook, use an LLM to identify visit schedule and
#' visit-matrix structure across projects, and generate the two external
#' configuration files required by pdchecker.
#'
#' @details
#' The function serializes all worksheet content (truncated per sheet) and sends
#' it to an OpenAI-compatible API. The model returns JSON with `visitcode` and
#' `testconfig` tables compatible with \code{\link{read_visitcode_file}} and
#' \code{\link{read_testconfig_file}}.
#'
#' Set `OPENROUTER_API_KEY`, `OPENROUTER_BASE_URL`, and `OPENROUTER_MODEL` in
#' `.Renviron`. Use `ai_instruction` to describe project-specific sheet names or
#' column meanings when layouts differ.
#'
#' ## What AI Identifies (priority)
#'
#' **visitcode** — from the visit schedule sheet (访视 / Visit / Folders):
#' - `VISIT`: visit name
#' - `VISITNUM`: visit code/number (must match EDC, not matrix column order)
#' - `VISITDAY`: inferred from `VISIT` when absent in ALS (e.g. D-28, D1, EOT)
#' - `CYCLE`: inferred from `VISIT` — 预激、预治疗、筛选期、治疗期1/治疗期2…、
#'   治疗结束、研究结束、随访期; unrecognized visits left empty
#' - `WP`: left **empty** for manual completion
#'
#' **testconfig** — from the eCRF visit matrix (检查类 only):
#' - `TESTCAT`: Chinese only, strip parenthetical codes (血常规, not 血常规(LB) or LB)
#' - Excludes visit admin, ICF, demographics, medical history, AE, concomitant meds, etc.
#' - `VISITNUM`: comma-separated visit numbers for required checks
#' - `FORM` kept for reference when editing
#'
#' @param als_file Character. Path to the ALS Excel file (.xlsx).
#' @param visitcode_file Character. Output path for visit schedule file.
#'   If `NULL` and `write_files = TRUE`, writes `visitcode.xlsx` under `output_dir`.
#' @param testconfig_file Character. Output path for test config file.
#'   If `NULL` and `write_files = TRUE`, writes `testconfig.xlsx` under `output_dir`.
#' @param output_dir Character. Directory for output files when paths are `NULL`.
#'   Defaults to the directory containing `als_file`.
#' @param write_files Logical. If `TRUE`, write Excel files to disk (default).
#' @param overwrite Logical. If `FALSE` (default), stop when output files already exist.
#' @param exclude_visits Character vector. Visit names to exclude from visitcode
#'   (default: `c("共同页", "计划外访视")`).
#' @param exclude_forms Character vector. Form labels to exclude from testconfig.
#' @param ai_instruction Character. Optional hints for AI (sheet/column names, exclusions).
#' @param verbose Logical. Print raw AI response.
#'
#' @return An invisible list with elements:
#'   \describe{
#'     \item{visitcode}{Data frame for \code{read_visitcode_file}}
#'     \item{testconfig}{Data frame for \code{read_testconfig_file}}
#'     \item{visitcode_file}{Path written, or `NULL`}
#'     \item{testconfig_file}{Path written, or `NULL`}
#'     \item{ai_notes}{Character, notes from AI on sheet/column mapping}
#'   }
#'
#' @examples
#' \dontrun{
#' cfg <- generate_config_from_als(
#'   als_file = system.file("extdata", "ALS_VISIT.xlsx", package = "pdchecker"),
#'   output_dir = tempdir(),
#'   ai_instruction = "访视编号在访视 sheet 的 VISITNUM 列"
#' )
#' visitcode  <- read_visitcode_file(cfg$visitcode_file)
#' testconfig <- read_testconfig_file(cfg$testconfig_file, visitcode = visitcode)
#' }
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#' @export
generate_config_from_als <- function(als_file,
                                    visitcode_file = NULL,
                                    testconfig_file = NULL,
                                    output_dir = NULL,
                                    write_files = TRUE,
                                    overwrite = FALSE,
                                    exclude_visits = c("共同页", "计划外访视"),
                                    exclude_forms = NULL,
                                    ai_instruction = NULL,
                                    verbose = FALSE) {
  if (!is.character(als_file) || length(als_file) != 1) {
    stop("'als_file' must be a single character string.", call. = FALSE)
  }
  if (is.na(als_file) || nchar(trimws(als_file)) == 0) {
    stop("'als_file' cannot be NA or empty.", call. = FALSE)
  }
  if (!file.exists(als_file)) {
    stop("ALS file not found: ", als_file, call. = FALSE)
  }
  if (tolower(tools::file_ext(als_file)) != "xlsx") {
    stop("ALS file must be an .xlsx file.", call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required.", call. = FALSE)
  }

  parsed <- parse_als_config_with_ai(
    als_file = als_file,
    exclude_visits = exclude_visits,
    exclude_forms = exclude_forms,
    ai_instruction = ai_instruction,
    verbose = verbose
  )

  visitcode <- parsed$visitcode
  testconfig <- parsed$testconfig
  ai_notes <- parsed$ai_notes

  out_visitcode_file <- NULL
  out_testconfig_file <- NULL

  if (isTRUE(write_files)) {
    if (is.null(output_dir)) {
      output_dir <- dirname(normalizePath(als_file, winslash = "/", mustWork = FALSE))
    }
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (is.null(visitcode_file)) {
      visitcode_file <- file.path(output_dir, "visitcode.xlsx")
    }
    if (is.null(testconfig_file)) {
      testconfig_file <- file.path(output_dir, "testconfig.xlsx")
    }

    if (!overwrite) {
      existing <- c(visitcode_file, testconfig_file)[file.exists(c(visitcode_file, testconfig_file))]
      if (length(existing) > 0) {
        stop(
          "Output file(s) already exist: ", paste(existing, collapse = ", "),
          ". Set overwrite = TRUE to replace them.",
          call. = FALSE
        )
      }
    }

    write_config_xlsx(visitcode, visitcode_file, sheet_name = "visitcode")
    write_config_xlsx(testconfig, testconfig_file, sheet_name = "testconfig")

    out_visitcode_file <- normalizePath(visitcode_file, winslash = "/", mustWork = FALSE)
    out_testconfig_file <- normalizePath(testconfig_file, winslash = "/", mustWork = FALSE)

    msg <- paste0(
      "Generated visitcode (", nrow(visitcode), " visits) -> ", out_visitcode_file, "\n",
      "Generated testconfig (", nrow(testconfig), " forms) -> ", out_testconfig_file, "\n",
      "Please verify VISIT / VISITNUM / VISITDAY / CYCLE and testconfig mappings.\n",
      "WP is left empty — fill window periods manually before use."
    )
    if (nzchar(ai_notes)) {
      msg <- paste0(msg, "\nAI notes: ", ai_notes)
    }
    message(msg)
  }

  invisible(list(
    visitcode = visitcode,
    testconfig = testconfig,
    visitcode_file = out_visitcode_file,
    testconfig_file = out_testconfig_file,
    ai_notes = ai_notes
  ))
}

#' Parse Form Label like 血常规(LB)
#' @noRd
parse_als_form_label <- function(label) {
  label <- trimws(as.character(label))
  m <- regexpr("^(.+?)\\(([^)]+)\\)$", label, perl = TRUE)
  if (m > 0) {
    g <- regmatches(label, regexec("^(.+?)\\(([^)]+)\\)$", label, perl = TRUE))[[1]]
    return(list(full = label, label = g[2], code = g[3]))
  }
  list(full = label, label = label, code = "")
}

#' Whether String Contains Chinese Characters
#' @noRd
has_chinese_chars <- function(x) {
  grepl("[\u4e00-\u9fff]", x, perl = TRUE)
}

#' Extract Chinese TESTCAT (strip parentheses and English-only codes)
#' @noRd
to_testcat_chinese <- function(label) {
  label <- trimws(as.character(label))
  if (!nzchar(label)) {
    return("")
  }

  info <- parse_als_form_label(label)
  chinese <- trimws(info$label)

  # Remove any remaining parenthetical segments
  chinese <- gsub("\\([^)]*\\)", "", chinese, perl = TRUE)
  chinese <- gsub("\\（[^）]*\\）", "", chinese, perl = TRUE)
  chinese <- trimws(chinese)

  # If label is English-only (e.g. LB), prefer text before code in full form
  if (!has_chinese_chars(chinese) && has_chinese_chars(info$full)) {
    chinese <- gsub("\\([^)]*\\)|\\（[^）]*\\）", "", info$full, perl = TRUE)
    chinese <- trimws(chinese)
  }

  if (!has_chinese_chars(chinese)) {
    return("")
  }

  chinese
}

#' Whether Form Row Is a Check/Examination Item (not admin or history)
#' @noRd
is_als_check_form <- function(form, testcat, tbname) {
  blob <- paste(form, testcat, tbname, sep = " ")
  if (!has_chinese_chars(blob)) {
    return(FALSE)
  }

  exclude_patterns <- c(
    "访视日期",
    "治疗结束",
    "知情",
    "同意书",
    "ICF",
    "人口学",
    "肿瘤病史",
    "病史询问",
    "既往",
    "过敏史",
    "个人史",
    "入排",
    "筛选确认",
    "分层因素",
    "随机信息",
    "随机",
    "不良事件",
    "合并用药",
    "非药",
    "研究结束",
    "死亡记录",
    "生存信息",
    "化疗情况",
    "试验药物",
    "肿瘤评估",
    "肿瘤治疗总体",
    "升血小板",
    "再次签署",
    "其他计划外检查",
    "CMRT",
    "CMMT",
    "CMYN",
    "PRYN",
    "AEYN",
    "CHEMO",
    "CHEMOL",
    "EXA",
    "EXB",
    "TRR",
    "TRIR",
    "CTIT",
    "TMH",
    "DSSCR",
    "DSRAND",
    "DSSCR",
    "EOT",
    "SV\\)",
    "人口学",
    "DH1",
    "DH2",
    "MHYN",
    "MH\\)",
    "AH\\)",
    "SU\\)",
    "RH\\)",
    "OH\\)",
    "SH\\)"
  )

  for (pat in exclude_patterns) {
    if (grepl(pat, blob, perl = TRUE, ignore.case = TRUE)) {
      return(FALSE)
    }
  }

  TRUE
}

#' Check Whether CYCLE Value Is Recognized
#' @noRd
is_valid_als_cycle <- function(cycle) {
  if (is.na(cycle) || !nzchar(trimws(cycle))) {
    return(FALSE)
  }
  cycle <- trimws(cycle)
  if (cycle %in% c("预激", "预治疗", "筛选期", "治疗结束", "研究结束", "随访期", "治疗期")) {
    return(TRUE)
  }
  grepl("^治疗期[0-9]+$", cycle, perl = TRUE)
}

#' Infer Numbered Treatment Cycle (治疗期1, 治疗期2, ...) from VISIT
#' @noRd
infer_treatment_cycle_from_visit <- function(visit) {
  m <- regexpr("C([0-9]+)", visit, perl = TRUE)
  if (m > 0) {
    n <- sub("^C", "", regmatches(visit, m))
    return(paste0("治疗期", n))
  }
  if (grepl("维持治疗|延伸治疗", visit) && !grepl("治疗结束", visit)) {
    return("治疗期")
  }
  if (grepl("治疗期", visit) && !grepl("治疗结束|筛选|预激|预治疗|纠正", visit)) {
    return("治疗期")
  }
  ""
}

#' Infer CYCLE from visit name
#' @noRd
infer_cycle_from_visit_name <- function(visit) {
  if (is.na(visit) || !nzchar(trimws(visit))) {
    return("")
  }
  visit <- trimws(visit)

  if (grepl("研究结束", visit)) {
    return("研究结束")
  }
  if (grepl("治疗结束|退出", visit)) {
    return("治疗结束")
  }
  if (grepl("随访|安全性", visit)) {
    return("随访期")
  }
  if (grepl("筛选", visit)) {
    return("筛选期")
  }
  if (grepl("预激", visit)) {
    return("预激")
  }
  if (grepl("预治疗|纠正治疗", visit)) {
    return("预治疗")
  }

  infer_treatment_cycle_from_visit(visit)
}

#' Infer VISITDAY from visit name
#' @noRd
infer_visitday_from_visit_name <- function(visit) {
  if (is.na(visit) || !nzchar(trimws(visit))) {
    return("")
  }
  visit <- trimws(visit)

  if (grepl("研究结束", visit)) {
    return("EOS")
  }
  if (grepl("治疗结束|退出", visit)) {
    return("EOT")
  }
  if (grepl("共同页|计划外|安全性|死亡", visit)) {
    return("")
  }

  m <- regexpr("D-([0-9]+)", visit, perl = TRUE)
  if (m > 0) {
    num <- sub("D-", "", regmatches(visit, m))
    return(as.character(-as.numeric(num)))
  }

  m <- regexpr("_D([0-9]+)", visit, perl = TRUE)
  if (m > 0) {
    return(sub(".*_D", "", regmatches(visit, regexpr("_D[0-9]+", visit, perl = TRUE))))
  }

  m <- regexpr("C[0-9]+D([0-9]+)", visit, perl = TRUE)
  if (m > 0) {
    return(sub(".*D", "", regmatches(visit, regexpr("C[0-9]+D[0-9]+", visit, perl = TRUE))))
  }

  ""
}

#' Fill VISITDAY and CYCLE from VISIT when missing or invalid
#' @noRd
fill_visitcode_from_visit_names <- function(visitcode) {
  for (i in seq_len(nrow(visitcode))) {
    v <- visitcode$VISIT[i]

    if (!nzchar(visitcode$VISITDAY[i])) {
      visitcode$VISITDAY[i] <- infer_visitday_from_visit_name(v)
    }

    cycle <- visitcode$CYCLE[i]
    numbered <- infer_treatment_cycle_from_visit(v)
    if (grepl("^治疗期[0-9]+$", numbered, perl = TRUE)) {
      visitcode$CYCLE[i] <- numbered
    } else if (!nzchar(cycle) || !is_valid_als_cycle(cycle)) {
      visitcode$CYCLE[i] <- infer_cycle_from_visit_name(v)
    } else if (cycle == "治疗期" && nzchar(numbered)) {
      visitcode$CYCLE[i] <- numbered
    }
  }

  visitcode$VISITDAY[is.na(visitcode$VISITDAY)] <- ""
  visitcode$CYCLE[is.na(visitcode$CYCLE)] <- ""
  visitcode$WP <- ""
  visitcode
}

#' Serialize ALS Workbook for AI Prompt
#' @noRd
serialize_als_workbook_for_ai <- function(als_file, max_rows = 80L, max_cols = 30L) {
  sheets <- readxl::excel_sheets(als_file)
  parts <- vapply(sheets, function(sh) {
    raw <- readxl::read_excel(
      als_file,
      sheet = sh,
      col_names = FALSE,
      .name_repair = "minimal"
    )
    nr <- min(nrow(raw), max_rows)
    nc <- min(ncol(raw), max_cols)
    if (nr == 0L || nc == 0L) {
      return(paste0("### 工作表: ", sh, "\n（空）\n"))
    }
    block <- raw[seq_len(nr), seq_len(nc), drop = FALSE]
    mat <- apply(block, c(1, 2), function(x) {
      if (is.na(x) || is.null(x)) {
        ""
      } else {
        gsub("[\t\r\n]+", " ", as.character(x), perl = TRUE)
      }
    })
    lines <- apply(mat, 1, function(row) paste(row, collapse = "\t"))
    paste0(
      "### 工作表: ", sh, "（第1-", nr, "行，第1-", nc, "列）\n",
      paste(lines, collapse = "\n"),
      "\n"
    )
  }, FUN.VALUE = character(1))
  paste(parts, collapse = "\n")
}

#' Build System Prompt for ALS Config AI Parsing
#' @noRd
.build_als_config_ai_prompt <- function() {
  paste0(
    "你是临床试验数据管理专家。请解析 ALS（eCRF）Excel 导出文件，生成 pdchecker 所需的配置表。\n\n",
    "## 核心任务\n",
    "1. **visitcode（访视计划）**：从「访视」/ Visit / Folders 工作表提取：\n",
    "   - VISIT：访视名称（须与矩阵表头完全一致）\n",
    "   - VISITNUM：访视编码（必须来自访视表，禁止按矩阵列顺序编号）\n",
    "   - VISITDAY：访视日（优先访视表，否则从 VISIT 推断，如 D-28、_D8、C1D15、EOT、EOS）\n",
    "   - WP：始终填空字符串 \"\"（用户后续手工填写窗口期）\n",
    "   - CYCLE：从 VISIT 推断，取值：预激、预治疗、筛选期、治疗期1/治疗期2…、",
    "治疗结束、研究结束、随访期；无法识别则 \"\"\n",
    "2. **testconfig（检查项配置）**：仅从访视矩阵中提取**检查类**表单（实验室、",
    "体格检查、生命体征、心电图、超声、身高、体重、**血样采集/PK采血**等），",
    "矩阵中标记为必填（☑、Y、是、1等不为空值）的项：\n",
    "   - **禁止**输出：访视日期、知情同意、人口学、各类病史、入排标准、随机、",
    "用药、不良事件、研究结束、肿瘤评估等\n",
    "   - **须包含**：血样采集、PK采血、PK血液等采样类检查（视为检查项，不要排除）\n",
    "   - TESTCAT：**必填**，填写中文检查项名称，去掉括号及英文代码（写「血常规」，",
    "不要写「LB」或「血常规(LB)」）\n",
    "   - FORM：完整表单标签，保留原文（如「血常规(LB)」）\n",
    "   - VISITNUM：逗号分隔的访视编号，对应 visitcode 中的 VISITNUM\n\n",
    "## 输出格式\n",
    "仅返回一个 JSON 对象，键名保持英文，值为中文/数字：\n",
    "```\n",
    "{\n",
    "  \"visitcode\": [\n",
    "    {\"VISIT\":\"筛选期- D-28~D-1\",\"VISITNUM\":101,\"WP\":\"\",\"CYCLE\":\"筛选期\",\"VISITDAY\":\"-28\"}\n",
    "  ],\n",
    "  \"testconfig\": [\n",
    "    {\"TESTCAT\":\"血常规\",\"VISITNUM\":\"101,102\",\"FORM\":\"血常规(LB)\"}\n",
    "  ],\n",
    "  \"notes\": \"说明使用了哪些工作表和列\"\n",
    "}\n",
    "```\n\n",
    "## 规则\n",
    "- 不得编造工作簿中不存在的访视、访视编号或表单\n",
    "- 默认排除访视：共同页、计划外访视\n",
    "- testconfig 中每条记录的 TESTCAT 不能为空，且必须是中文"
  )
}

#' Build User Message for ALS Config AI Parsing
#' @noRd
.build_als_config_user_message <- function(als_file, ai_instruction = NULL) {
  parts <- character()
  if (!is.null(ai_instruction) && nzchar(trimws(ai_instruction))) {
    parts <- c(parts, paste0("项目补充说明：\n", trimws(ai_instruction), "\n"))
  }
  parts <- c(
    parts,
    "请解析以下 ALS 工作簿，生成 visitcode 和 testconfig 的 JSON。\n",
    "重点：testconfig 只含检查类表单，TESTCAT 必须填写中文名称（如「血常规」），",
    "不能为空或英文代码。\n\n",
    serialize_als_workbook_for_ai(als_file)
  )
  paste(parts, collapse = "")
}

#' Parse ALS Config Using AI
#' @noRd
parse_als_config_with_ai <- function(als_file,
                                     exclude_visits,
                                     exclude_forms,
                                     ai_instruction = NULL,
                                     verbose = FALSE) {
  creds <- .ai_resolve_credentials()
  message("Parsing ALS with AI (model: ", creds$model, ")...")

  parsed_json <- .ai_chat_json(
    system = .build_als_config_ai_prompt(),
    user = .build_als_config_user_message(als_file, ai_instruction),
    temperature = 0.1,
    timeout = 180,
    verbose = verbose
  )

  normalize_ai_als_config(parsed_json, exclude_visits, exclude_forms)
}

#' Ensure a Character Column Exists on a Data Frame
#' @noRd
.ensure_char_column <- function(df, col, default = "") {
  if (col %in% names(df)) {
    df[[col]] <- trimws(as.character(df[[col]]))
    df[[col]][is.na(df[[col]])] <- default
  } else {
    df[[col]] <- default
  }
  df
}

#' Resolve One testconfig Row from AI Output
#' @noRd
.resolve_testconfig_row <- function(form, testcat_raw, tbname = "") {
  form <- trimws(as.character(form))
  testcat_raw <- trimws(as.character(testcat_raw))
  tbname <- trimws(as.character(tbname))

  form_label <- form
  if (!grepl("[\\(（]", form_label, perl = TRUE) &&
      grepl("[\\(（]", testcat_raw, perl = TRUE)) {
    form_label <- testcat_raw
  }
  if (!nzchar(form_label)) {
    form_label <- testcat_raw
  }

  info <- parse_als_form_label(form_label)
  testcat <- to_testcat_chinese(form_label)
  if (!nzchar(testcat)) {
    testcat <- to_testcat_chinese(testcat_raw)
  }

  if (!nzchar(tbname)) {
    tbname <- info$code
  }
  form_out <- if (nzchar(form)) {
    form
  } else if (nzchar(info$full)) {
    info$full
  } else {
    form_label
  }

  list(
    TESTCAT = testcat,
    FORM = form_out,
    keep = nzchar(testcat) &&
      has_chinese_chars(testcat) &&
      is_als_check_form(form_out, testcat, tbname)
  )
}

#' Normalize AI JSON to visitcode and testconfig Tibbles
#' @noRd
normalize_ai_als_config <- function(parsed_json,
                                    exclude_visits,
                                    exclude_forms) {
  if (!is.list(parsed_json)) {
    stop("AI response is not a JSON object.", call. = FALSE)
  }

  if (is.null(parsed_json$visitcode)) {
    stop("AI response missing 'visitcode' array.", call. = FALSE)
  }
  if (is.null(parsed_json$testconfig)) {
    stop("AI response missing 'testconfig' array.", call. = FALSE)
  }

  visitcode <- as.data.frame(parsed_json$visitcode, stringsAsFactors = FALSE)
  testconfig <- as.data.frame(parsed_json$testconfig, stringsAsFactors = FALSE)

  required_vc <- c("VISIT", "VISITNUM")
  if (!all(required_vc %in% names(visitcode))) {
    stop(
      "AI visitcode missing columns: ",
      paste(setdiff(required_vc, names(visitcode)), collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(c("TESTCAT", "VISITNUM") %in% names(testconfig))) {
    stop("AI testconfig must contain TESTCAT and VISITNUM.", call. = FALSE)
  }

  visitcode$VISIT <- trimws(as.character(visitcode$VISIT))
  visitcode <- visitcode[!is.na(visitcode$VISIT) & visitcode$VISIT != "", , drop = FALSE]
  visitcode$VISITNUM <- suppressWarnings(as.numeric(visitcode$VISITNUM))
  if (any(is.na(visitcode$VISITNUM))) {
    stop("AI visitcode contains invalid VISITNUM values.", call. = FALSE)
  }

  visitcode <- .ensure_char_column(visitcode, "VISITDAY")
  visitcode <- .ensure_char_column(visitcode, "CYCLE")

  visitcode <- fill_visitcode_from_visit_names(visitcode)

  if (!is.null(exclude_visits) && length(exclude_visits) > 0) {
    visitcode <- visitcode[!visitcode$VISIT %in% exclude_visits, , drop = FALSE]
  }
  if (nrow(visitcode) == 0L) {
    stop("No visits left in visitcode after exclusions.", call. = FALSE)
  }

  testconfig$VISITNUM <- trimws(as.character(testconfig$VISITNUM))
  testconfig$FORM <- if ("FORM" %in% names(testconfig)) {
    trimws(as.character(testconfig$FORM))
  } else {
    trimws(as.character(testconfig$TESTCAT))
  }
  raw_testcat <- trimws(as.character(testconfig$TESTCAT))
  raw_tbname <- if ("TBNAME" %in% names(testconfig)) {
    trimws(as.character(testconfig$TBNAME))
  } else {
    rep("", nrow(testconfig))
  }

  resolved <- Map(
    .resolve_testconfig_row,
    testconfig$FORM,
    raw_testcat,
    raw_tbname
  )
  testconfig$TESTCAT <- vapply(resolved, `[[`, "", "TESTCAT")
  testconfig$FORM <- vapply(resolved, `[[`, "", "FORM")
  testconfig <- testconfig[vapply(resolved, `[[`, TRUE, "keep"), , drop = FALSE]

  if (nrow(testconfig) == 0L) {
    warning(
      "过滤后 testconfig 为空。请检查 AI 是否返回了中文 TESTCAT 和 FORM 字段，",
      "或适当调整 exclude_forms。",
      call. = FALSE
    )
  }

  testconfig <- testconfig[
    !is.na(testconfig$VISITNUM) & testconfig$VISITNUM != "",
    ,
    drop = FALSE
  ]

  if (!is.null(exclude_forms) && length(exclude_forms) > 0) {
    testconfig <- testconfig[!testconfig$FORM %in% exclude_forms, , drop = FALSE]
  }

  testconfig$VISITNUM <- vapply(testconfig$VISITNUM, function(vn) {
    nums <- as.numeric(unlist(strsplit(vn, "[,，]")))
    nums <- nums[!is.na(nums)]
    nums <- nums[nums %in% visitcode$VISITNUM]
    paste(nums, collapse = ",")
  }, FUN.VALUE = character(1))
  testconfig <- testconfig[testconfig$VISITNUM != "", , drop = FALSE]

  testconfig <- testconfig[, c("TESTCAT", "VISITNUM", "FORM"), drop = FALSE]

  ai_notes <- if (is.null(parsed_json$notes)) "" else as.character(parsed_json$notes)

  list(
    visitcode = tibble::as_tibble(visitcode),
    testconfig = tibble::as_tibble(testconfig),
    ai_notes = ai_notes
  )
}

#' Write a Single-Sheet Excel Config File
#' @noRd
write_config_xlsx <- function(data, file_path, sheet_name) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to write config files.", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, data)
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
}
