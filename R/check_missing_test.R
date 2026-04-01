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
#' # Step 1: Read config file (required)
#' testconfig <- read_testconfig_file("config/test_config.xlsx")
#'
#' # Step 2: Prepare test data (uses testconfig automatically)
#' prepared_data <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES"
#' )
#'
#' # Step 3: Check all tests (including overall missing and individual missing)
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
#' @param pdno Character string specifying the protocol deviation number for this check (default: "8.3.1")
#'
#' @return List with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if there are missing tests}
#'     \item{messages}{Character vector. Deviation description messages}
#'     \item{details}{Data frame. Missing test details with columns:
#'       \itemize{
#'         \item PDNO: Protocol deviation number specified by \code{pdno} parameter
#'         \item SUBJID: Subject ID
#'         \item VISIT: Visit name
#'         \item VISITNUM: Visit number
#'         \item visit_date: Actual visit date
#'         \item TBNAME: Test dataset name (e.g., LB, VS, EG)
#'         \item test_name: Test name
#'         \item missing_type: Missing type
#'         \item DESCRIPTION: Detailed description of the missing test
#'       }
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#' # Step 1: Read config and prepare test data
#' testconfig <- read_testconfig_file("test_config.xlsx")
#' prepared_lb <- prepare_test_data(
#'   data = data,
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES"
#' )
#'
#' # Step 2: Check all missing tests
#' result <- check_missing_test(data = prepared_lb)
#' print(result)
#'
#' # Check specific test only
#' result <- check_missing_test(
#'   data = prepared_lb,
#'   test_var = "TESTDE",
#'   test = "RBC Count"
#' )
#'
#' # Only check overall missing, skip individual indicators
#' result <- check_missing_test(data = prepared_lb, missing_de = FALSE)
#' }
#'
#' @seealso
#' \code{\link{prepare_test_data}} for preparing test data
#' \code{\link{check_missing_visit}} for checking missing visits
#'
#' @family test checks
#'
#' @importFrom dplyr filter select mutate group_by summarise ungroup bind_rows sym
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
check_missing_test <- function(data,
                               test_var = NULL,
                               test = NULL,
                               missing_de = TRUE,
                               pdno = "8.3.1") {
  # ============================================================================
  # Part 1: Validate input parameters
  # ============================================================================
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame prepared by prepare_test_data()")
  }

  if (!is.character(pdno) || length(pdno) != 1) {
    stop("'pdno' must be a single character string")
  }

  # Check required columns (standardized by prepare_test_data)
  required_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME",
    "TESTCAT", "TESTCAT_ORIG", "TESTDAT", "TESTYN", "ORRES"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "'data' is missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nPlease use prepare_test_data() to prepare data"
    )
  }

  # ============================================================================
  # Part 2: Initialize results
  # ============================================================================
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # ============================================================================
  # Part 3: Filter and prepare data
  # ============================================================================
  # Filter by specific test if test_var and test are provided
  if (!is.null(test_var) && !is.null(test) && test_var %in% names(data)) {
    test_data_filtered <- data %>%
      filter(!!sym(test_var) == test)
  } else {
    test_data_filtered <- data
  }

  # Get records with actual visits (SVDAT not empty)
  visits_with_tests <- test_data_filtered %>%
    filter(!is_sas_na(SVDAT)) %>%
    mutate(visit_date = as.Date(SVDAT)) %>%
    filter(!is.na(visit_date))

  # Return early if no actual visit records
  if (nrow(visits_with_tests) == 0) {
    class(results) <- c("missing_test_check", "list")
    return(results)
  }

  # ============================================================================
  # Part 4: Check missing tests (three types)
  # ============================================================================
  # Type 1: TESTCAT is empty (visit has no test records)
  testcat_empty <- visits_with_tests %>%
    filter(is.na(TESTCAT_ORIG) | TESTCAT_ORIG == "") %>%
    group_by(SUBJID, VISIT, VISITNUM, visit_date, TBNAME, TESTCAT) %>%
    summarise(.groups = "drop") %>%
    mutate(
      missing_type = "TESTCAT_EMPTY",
      test_name = as.character(TESTCAT)
    )

  # Type 2: TESTCAT not empty, but entire TESTCAT missing
  testcat_missing <- visits_with_tests %>%
    filter(!is.na(TESTCAT_ORIG) & TESTCAT_ORIG != "") %>%
    filter(is_sas_na(TESTDAT) | is.na(TESTDAT) | is_sas_na(TESTYN) | TESTYN != "是") %>%
    group_by(SUBJID, VISIT, VISITNUM, visit_date, TESTCAT, TBNAME) %>%
    summarise(.groups = "drop") %>%
    mutate(
      missing_type = "TESTCAT_MISSING",
      test_name = as.character(TESTCAT)
    )

  # Type 3: Individual TESTDE missing (only if missing_de = TRUE)
  if (missing_de) {
    testde_missing <- visits_with_tests %>%
      filter(!is.na(TESTCAT_ORIG) & TESTCAT_ORIG != "") %>%
      filter(!is_sas_na(TESTDAT) & !is.na(TESTDAT)) %>%
      filter(is_sas_na(ORRES) | is.na(ORRES) | ORRES == "") %>%
      group_by(SUBJID, VISIT, VISITNUM, visit_date, TESTCAT, TESTDE, TBNAME) %>%
      summarise(.groups = "drop") %>%
      mutate(
        missing_type = "TESTDE_MISSING",
        test_name = paste0(as.character(TESTCAT), "-", as.character(TESTDE))
      )
  } else {
    testde_missing <- data.frame()
  }

  # Combine all missing types
  all_missing <- bind_rows(testcat_empty, testcat_missing, testde_missing)

  # Return early if no missing records
  if (nrow(all_missing) == 0) {
    class(results) <- c("missing_test_check", "list")
    return(results)
  }

  # ============================================================================
  # Part 5: Build results
  # ============================================================================
  # Build details data frame with DESCRIPTION
  details <- all_missing %>%
    mutate(
      PDNO = pdno,
      VISITNUM = as.numeric(VISITNUM),
      DESCRIPTION = sprintf(
        "受试者编号%s，在访视%s（%s），计划进行的[%s]缺失。",
        .data$SUBJID, .data$VISIT, .data$visit_date, .data$test_name
      )
    ) %>%
    select(
      "PDNO", "SUBJID", "VISIT", "VISITNUM", "visit_date",
      "TBNAME", "test_name", "missing_type", "DESCRIPTION"
    )

  # Count by missing type
  n_testcat_empty <- sum(details$missing_type == "TESTCAT_EMPTY")
  n_testcat <- sum(details$missing_type == "TESTCAT_MISSING")
  n_testde <- sum(details$missing_type == "TESTDE_MISSING")

  # Build messages

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
  if (!missing_de && length(msg_parts) > 0) {
    msg_parts <- c(msg_parts, "（仅检查整体检查项缺失）")
  }

  # Compile results
  results$has_deviation <- TRUE
  results$messages <- paste(msg_parts, collapse = "；")
  results$details <- details

  class(results) <- c("missing_test_check", "list")
  return(results)
}


#' Print method for missing test check results
#' @param x Object of class missing_test_check
#' @param ... Additional arguments
#' @export
print.missing_test_check <- function(x, ...) {
  pdno_display <- if (nrow(x$details) > 0) x$details$PDNO[1] else "8.3.1"
  cat(sprintf("%s 检查项缺失检查\n", pdno_display))
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
    cat(x$details$DESCRIPTION, sep = "\n")
  }
}
