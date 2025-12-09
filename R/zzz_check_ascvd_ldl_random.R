#' Check ASCVD history and LDL-C random criteria
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when left_join pull
#' @export
check_ascvd_ldl_random <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("MH", "LB", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomized subjects
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get ASCVD history from MH dataset
  ascvd_history <- data$MH %>%
    filter(
      !is_sas_na(MHTERM),
      grepl("ASC|动脉粥样硬化|缺血性心脏病|冠心病|心绞痛|缺血性卒中|短暂性脑缺血发作|外周动脉疾病|血运重建术|脑梗", MHTERM, ignore.case = TRUE)
    ) %>%
    mutate(
      has_ascvd = TRUE
    ) %>%
    filter(has_ascvd) %>%
    pull(SUBJID)

  # Get LDL-C values from LB dataset
  ldl_values <- data$LB %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    filter(
      VISITNAME == "筛选期V1",
      LBTEST == "低密度脂蛋白胆固醇"
    ) %>%
    filter(!is_sas_na(LBORRES)) %>%
    mutate(
      LDL_VALUE = as.numeric(LBORRES),
      LDL_UNIT = LBORRESU,
      # Create criterion description based on ASCVD status
      CRITERION = case_when(
        SUBJID %in% ascvd_history & LDL_VALUE < 1.8 ~
          sprintf("有确诊的ASCVD病史，LDL-C为%.2f mmol/L", LDL_VALUE),
        !SUBJID %in% ascvd_history & LDL_VALUE < 2.6 ~
          sprintf("无确诊的ASCVD病史，LDL-C为%.2f mmol/L", LDL_VALUE),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(CRITERION))

  # Get deviations
  deviations <- ldl_values %>%
    select(SUBJID, LDL_VALUE, LDL_UNIT, CRITERION)

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- paste(
      "不符合以下随机标准：",
      "1) 随机前有确诊的ASCVD病史者，随机前空腹LDL-C>=1.8 mmol/L；",
      "2) 无确诊的ASCVD病史者，随机前空腹LDL-C>=2.6 mmol/L。",
      sep = "\n"
    )
    results$details <- deviations
  }

  class(results) <- c("ascvd_ldl_check", "list")
  return(results)
}

#' Print method for ASCVD and LDL-C check results
#' @param x Object of class ascvd_ldl_check
#' @param ... Additional arguments
#' @export
print.ascvd_ldl_check <- function(x, ...) {
  cat("5.3不符合随机标准却随机\n")
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
        "受试者%s：%s",
        row["SUBJID"],
        row["CRITERION"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
