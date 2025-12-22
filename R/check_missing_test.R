#' Check Missing Tests
#'
#' @description
#' For each subject's completed visit records, check if a specific test item was performed.
#'
#' @details
#' ## Usage Workflow
#'
#' This function checks if corresponding tests were completed in actual visit records.
#' It is recommended to first prepare data using \code{\link{prepare_test_data}}:
#'
#' ```r
#' # Step 1: Prepare test data
#' prepared_data <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES"
#' )
#'
#' # Step 2: Check all tests (including overall missing and individual missing)
#' result <- check_missing_test(data = prepared_data)
#'
#' # Or check specific tests (e.g., RBC count)
#' result <- check_missing_test(
#'   data = prepared_data,
#'   test_var = "TESTDE",
#'   test = "RBC Count"
#' )
#'
#' # Only check overall test missing, not individual indicators
#' result <- check_missing_test(
#'   data = prepared_data,
#'   missing_de = FALSE
#' )
#'
#' # If limiting check scope, use config file in prepare_test_data
#' prepared_data_with_config <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   config = "config/test_config.xlsx"
#' )
#' result <- check_missing_test(data = prepared_data_with_config)
#' ```
#'
#' ## Missing Test Logic
#'
#' This function distinguishes three types of missing:
#'
#' **1. TESTCAT is empty** (visit has no test records)
#' - Condition: TESTCAT is empty or NA
#' - Output: One record per subject/visit/TBNAME combination
#' - Format: e.g., "LB missing"
#' - Reason: "TBNAME test not performed"
#'
#' **2. TESTCAT not empty, but entire TESTCAT missing** (e.g., entire CBC not done)
#' - Condition: TESTCAT not empty, but TESTDAT is empty or TESTYN != "Yes"
#' - Output: One record per subject/visit/TESTCAT combination
#' - Format: e.g., "CBC missing"
#' - Reason: "Entire test category not performed"
#'
#' **3. TESTCAT not empty, but individual TESTDE missing** (e.g., WBC count missing)
#' - Condition: TESTCAT not empty, TESTDAT not empty, but ORRES is empty
#' - Output: One record for each missing TESTDE per subject/visit/TESTCAT
#' - Format: e.g., "CBC-WBC Count missing"
#' - Reason: "Test result is empty"
#'
#' Note: Input data must be prepared by \code{\link{prepare_test_data}},
#' which standardizes column names to SUBJID, VISIT, VISITNUM, SVDAT, TESTCAT,
#' TESTDE, TESTYN, TESTDAT, ORRES, etc.
#'
#' @param data Data frame, test data to check.
#'   Must be prepared by \code{\link{prepare_test_data}} with standardized column names
#' @param test_var Character string, variable name for filtering specific tests (default: NULL).
#'   If NULL, checks all tests. Typically use "TESTDE" (test name) or "TESTCAT" (test category)
#' @param test Character string, specific value for test_var (default: NULL).
#'   E.g., "RBC Count", "WBC Count". Ignored if test_var is NULL
#' @param missing_de Logical, whether to check individual TESTDE missing (default: TRUE).
#'   If TRUE, checks all three missing types;
#'   If FALSE, only checks first two types (TESTCAT empty, entire TESTCAT missing)
#'
#' @return List with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if there are missing tests}
#'     \item{messages}{Character vector. Deviation description messages}
#'     \item{details}{Data frame. Missing test details with columns:
#'       \itemize{
#'         \item SUBJID: Subject ID
#'         \item VISIT: Visit name
#'         \item VISITNUM: Visit number
#'         \item visit_date: Actual visit date
#'         \item test_name: Test name
#'         \item missing_reason: Missing reason
#'         \item missing_type: Missing type
#'       }
#'     }
#'   }
#'
#' @importFrom dplyr filter select mutate group_by summarise ungroup bind_rows sym
#' @importFrom magrittr %>%
#' @export
#'
check_missing_test <- function(data,
                               test_var = NULL,
                               test = NULL,
                               missing_de = TRUE) {
  # 验证输入参数
  if (!is.data.frame(data)) {
    stop("data 必须是 data frame 类型，请使用 prepare_test_data 函数准备数据")
  }

  # 检查检查项数据集必要的列（由 prepare_test_data 生成，列名已标准化）
  test_required_cols <- c("SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME", "TESTCAT", "TESTDAT", "TESTYN", "ORRES")
  test_missing_cols <- setdiff(test_required_cols, names(data))
  if (length(test_missing_cols) > 0) {
    stop(paste0(
      "检查项数据集缺少必要的列: ",
      paste(test_missing_cols, collapse = ", "),
      "\n请使用 prepare_test_data 函数准备数据"
    ))
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # 根据是否指定test_var筛选检查项数据
  if (!is.null(test_var) && !is.null(test) && test_var %in% names(data)) {
    # 筛选特定检查项
    test_data_filtered <- data %>%
      filter(!!sym(test_var) == test)
    test_name <- test
  } else {
    # 检查所有检查项
    test_data_filtered <- data
    test_name <- "所有检查项"
  }

  # 获取实际进行了访视的记录（SVDAT不为空）
  # data 已经是 prepare_test_data 处理后的数据，包含了 SV 和检查项数据
  visits_with_tests <- test_data_filtered %>%
    filter(!is_sas_na(SVDAT)) %>%
    mutate(visit_date = as.Date(SVDAT)) %>%
    filter(!is.na(visit_date))

  # 如果没有实际访视记录，直接返回
  if (nrow(visits_with_tests) == 0) {
    return(results)
  }

  # 区分三种缺失情况：
  # 1. TESTCAT为空：访视没有对应的检查记录
  # 2. TESTCAT不为空，但整个TESTCAT缺失：TESTDAT为空 或 TESTYN=否或空
  # 3. TESTCAT不为空，但单个TESTDE指标缺失：TESTDAT不为空 且 ORRES为空

  # 情况1：TESTCAT为空（访视没有对应的检查记录）
  testcat_empty <- visits_with_tests %>%
    filter(is.na(TESTCAT) | TESTCAT == "") %>%
    group_by(SUBJID, VISIT, VISITNUM, visit_date, TBNAME) %>%
    summarise(
      missing_type = "TESTCAT为空",
      .groups = "drop"
    ) %>%
    mutate(
      TBNAME = as.character(TBNAME), # 确保 TBNAME 是字符型
      TESTCAT = NA_character_,
      TESTDE = NA_character_,
      test_name = paste0("全部", as.character(TBNAME)),
      missing_reason = paste0("未进行", as.character(TBNAME), "检查")
    )

  # 情况2：TESTCAT不为空，但整个TESTCAT缺失（按TESTCAT分组，每个TESTCAT只输出一条）
  testcat_missing <- visits_with_tests %>%
    filter(!is.na(TESTCAT) & TESTCAT != "") %>%
    filter(is_sas_na(TESTDAT) | is.na(TESTDAT) | is_sas_na(TESTYN) | TESTYN != "是") %>%
    group_by(SUBJID, VISIT, VISITNUM, visit_date, TESTCAT, TBNAME) %>%
    summarise(
      missing_type = "TESTCAT缺失",
      .groups = "drop"
    ) %>%
    mutate(
      TBNAME = as.character(TBNAME), # 确保 TBNAME 是字符型
      TESTCAT = as.character(TESTCAT), # 确保 TESTCAT 是字符型
      TESTDE = NA_character_,
      test_name = as.character(TESTCAT),
      missing_reason = "整个检查类别未进行"
    )

  # 情况3：TESTCAT不为空，单个TESTDE指标缺失（按TESTDE分组，每个TESTDE输出一条）
  # 根据 missing_de 参数决定是否检查单个指标缺失
  if (missing_de) {
    testde_missing <- visits_with_tests %>%
      filter(!is.na(TESTCAT) & TESTCAT != "") %>%
      filter(!is_sas_na(TESTDAT) & !is.na(TESTDAT)) %>%
      filter(is_sas_na(ORRES) | is.na(ORRES) | ORRES == "") %>%
      group_by(SUBJID, VISIT, VISITNUM, visit_date, TESTCAT, TESTDE, TBNAME) %>%
      summarise(
        missing_type = "TESTDE缺失",
        .groups = "drop"
      ) %>%
      mutate(
        TBNAME = as.character(TBNAME), # 确保 TBNAME 是字符型
        TESTCAT = as.character(TESTCAT), # 确保 TESTCAT 是字符型
        TESTDE = as.character(TESTDE), # 确保 TESTDE 是字符型
        test_name = paste0(as.character(TESTCAT), "-", as.character(TESTDE)),
        missing_reason = "具体检查指标缺失"
      )
  } else {
    # 如果不检查单个指标，创建空数据框
    testde_missing <- data.frame(
      SUBJID = character(),
      VISIT = character(),
      VISITNUM = character(), # 统一为字符型，与其他数据框保持一致
      visit_date = as.Date(character()),
      TESTCAT = character(),
      TESTDE = character(),
      TBNAME = character(),
      missing_type = character(),
      test_name = character(),
      missing_reason = character(),
      stringsAsFactors = FALSE
    )
  }

  # 合并三种缺失情况
  all_missing <- bind_rows(testcat_empty, testcat_missing, testde_missing)

  # 构建缺失记录
  if (nrow(all_missing) > 0) {
    missing_tests_list <- lapply(seq_len(nrow(all_missing)), function(i) {
      row <- all_missing[i, ]

      data.frame(
        SUBJID = row$SUBJID,
        VISIT = row$VISIT,
        VISITNUM = row$VISITNUM,
        visit_date = row$visit_date,
        test_name = row$test_name,
        missing_reason = row$missing_reason,
        missing_type = row$missing_type,
        stringsAsFactors = FALSE
      )
    })
  } else {
    missing_tests_list <- list()
  }

  # 合并所有缺失检查记录
  if (length(missing_tests_list) > 0) {
    missing_tests <- bind_rows(missing_tests_list)
    results$has_deviation <- TRUE

    # 根据缺失类型生成消息
    n_testcat_empty <- sum(missing_tests$missing_type == "TESTCAT为空")
    n_testcat <- sum(missing_tests$missing_type == "TESTCAT缺失")
    n_testde <- sum(missing_tests$missing_type == "TESTDE缺失")

    msg_parts <- character()
    if (n_testcat_empty > 0) {
      msg_parts <- c(msg_parts, paste0("访视无检查记录 ", n_testcat_empty, " 条"))
    }
    if (n_testcat > 0) {
      msg_parts <- c(msg_parts, paste0("整体检查项缺失 ", n_testcat, " 条"))
    }
    if (n_testde > 0) {
      msg_parts <- c(msg_parts, paste0("具体指标缺失 ", n_testde, " 条"))
    }

    # 如果用户设置了 missing_de = FALSE，在消息中说明
    if (!missing_de && length(msg_parts) > 0) {
      msg_parts <- c(msg_parts, "（仅检查整体检查项缺失）")
    }

    results$messages <- paste(msg_parts, collapse = "；")
    results$details <- missing_tests
  }

  class(results) <- c("missing_test_check", "list")
  return(results)
}


#' Print method for missing test check results
#' @param x Object of class missing_test_check
#' @param ... Additional arguments
#' @export
print.missing_test_check <- function(x, ...) {
  cat("检查项缺失检查\n")
  cat("====================================\n")
  cat(sprintf(
    "Has deviation: %s\n",
    ifelse(x$has_deviation, "YES", "NO")
  ))

  if (length(x$messages) > 0) {
    cat("\nFindings:\n")
    cat(paste("-", x$messages), sep = "\n")
  }

  if (nrow(x$details) > 0) {
    cat("\nDeviation Details:\n")
    formatted_details <- apply(x$details, 1, function(row) {
      sprintf(
        "受试者%s，在访视%s（%s），计划进行的[%s]-缺失。",
        row["SUBJID"],
        row["VISIT"],
        row["visit_date"],
        row["test_name"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
