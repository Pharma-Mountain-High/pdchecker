#' 检查检查项缺失
#'
#' @description
#' 对每个受试者进行了实际访视的访视记录，检查某一项检查项是否进行了检查。
#'
#' @details
#' ## 使用流程
#'
#' 该函数检查在实际发生访视的记录中，是否完成了相应的检查项。
#' 建议先使用 \code{\link{prepare_test_data}} 准备数据：
#'
#' ```r
#' # 步骤1：准备检查项数据
#' prepared_data <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES"
#' )
#'
#' # 步骤2：检查所有检查项（包括整体缺失和单个指标缺失）
#' result <- check_missing_test(data = prepared_data)
#'
#' # 或者检查特定检查项（如红细胞计数）
#' result <- check_missing_test(
#'   data = prepared_data,
#'   test_var = "TESTDE",  # 使用 TESTDE 列筛选具体检查项
#'   test = "红细胞计数"
#' )
#'
#' # 只检查整体检查项缺失，不检查单个指标缺失
#' result <- check_missing_test(
#'   data = prepared_data,
#'   missing_de = FALSE
#' )
#'
#' # 如果需要限定检查范围，应在 prepare_test_data 阶段使用配置文件
#' prepared_data_with_config <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   config = "config/test_config.xlsx"  # 在数据准备阶段使用配置
#' )
#' result <- check_missing_test(data = prepared_data_with_config)
#' ```
#'
#' ## 未查的判断逻辑
#'
#' 本函数区分三种缺失情况：
#'
#' **1. TESTCAT为空**（访视没有对应的检查记录）
#' - 判断条件：TESTCAT为空或NA
#' - 输出：同一受试者、同一访视、同一`TBNAME`只输出一条记录
#' - 格式：如"LB缺失"
#' - 缺失原因："未进行`TBNAME`检查"
#'
#' **2. TESTCAT不为空，但整个TESTCAT缺失**（例如整个"血常规"检查都未做）
#' - 判断条件：TESTCAT不为空，且TESTDAT为空 或 TESTYN不等于"是"
#' - 输出：同一受试者、同一访视、同一TESTCAT只输出一条记录
#' - 格式：如"血常规缺失"
#' - 缺失原因："整个检查项目未进行"
#'
#' **3. TESTCAT不为空，但单个TESTDE指标缺失**（例如"白细胞计数"缺失）
#' - 判断条件：TESTCAT不为空，TESTDAT不为空，且ORRES为空
#' - 输出：同一受试者、同一访视、同一TESTCAT，每个缺失的TESTDE输出一条记录
#' - 格式：如"血常规-白细胞计数缺失"
#' - 缺失原因："检查结果为空"
#'
#' 注意：本函数要求输入数据由 \code{\link{prepare_test_data}} 准备，
#' 该函数已将列名标准化为 SUBJID、VISIT、VISITNUM、SVDAT、TESTCAT、TESTDE、
#' TESTYN、TESTDAT、ORRES 等。
#'
#' @param data Data frame，要检查的检查项数据集。
#'   必须使用 \code{\link{prepare_test_data}} 准备的数据，包含标准化的列名
#' @param test_var Character string，筛选具体检查项目的变量名（默认: NULL）。
#'   如果为NULL，则检查所有检查项。通常使用 "TESTDE"（检查项目名称）或 "TESTCAT"（检查分类）
#' @param test Character string，test_var对应的具体值（默认: NULL）。
#'   例如："红细胞计数"、"白细胞计数"等。如果test_var为NULL，则此参数无效
#' @param missing_de Logical，是否检查单个指标TESTDE的缺失（默认: TRUE）。
#'   如果为TRUE，会检查三种缺失情况（TESTCAT为空、整个TESTCAT缺失、单个TESTDE指标缺失）；
#'   如果为FALSE，只检查前两种缺失情况（TESTCAT为空、整个TESTCAT缺失）
#'
#' @return List，包含以下组件：
#'   \describe{
#'     \item{has_deviation}{逻辑值。TRUE表示有缺失检查，FALSE表示无缺失检查}
#'     \item{messages}{字符向量。偏差描述信息}
#'     \item{details}{Data frame。缺失检查详细信息，每个缺失检查一条记录，包含以下列：
#'       \itemize{
#'         \item SUBJID: 受试者ID
#'         \item VISIT: 访视名称
#'         \item VISITNUM: 访视编号
#'         \item visit_date: 实际访视日期
#'         \item test_name: 检查项名称
#'         \item missing_reason: 缺失原因（未进行`TBNAME`检查/整个检查项目未进行/检查结果为空）
#'         \item missing_type: 缺失类型（TESTCAT为空/TESTCAT缺失/TESTDE缺失）
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
