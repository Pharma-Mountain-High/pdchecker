#' Check thyroid function exclusion criteria
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate
#' @noRd
check_thyroid_exclusion <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("LB", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomization information
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get TSH test results at screening
  tsh_tests <- data$LB %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    filter(
      VISITNAME == "筛选期V1",
      LBTEST == "促甲状腺激素"
    ) %>%
    filter(!is_sas_na(LBORRES) & !is_sas_na(LBORNRHI) & !is_sas_na(LBORNRLO)) %>%
    mutate(
      TEST_VALUE = as.numeric(LBORRES),
      LLN = as.numeric(LBORNRLO),
      ULN = as.numeric(LBORNRHI),
      RATIO_ULN = TEST_VALUE / ULN,
      CRITERION = case_when(
        TEST_VALUE < LLN ~
          sprintf("TSH < 正常下限（%.2f）", LLN),
        RATIO_ULN > 1.5 ~
          sprintf("TSH > 1.5倍正常值上限（%.2f倍ULN）", RATIO_ULN),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(CRITERION))

  # Get deviations
  deviations <- tsh_tests %>%
    select(SUBJID, LBTEST, TEST_VALUE, LLN, ULN, CRITERION)

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "符合排除标准4.4任意一项，筛选时促甲状腺激素（TSH）< 正常下限或 > 1.5×正常值上限"
    results$details <- deviations
  }

  class(results) <- c("thyroid_check", "list")
  return(results)
}

#' Print method for thyroid function check results
#' @param x Object of class thyroid_check
#' @param ... Additional arguments
#' @noRd
print.thyroid_check <- function(x, ...) {
  cat("4.4符合排除标准实验室检查要求却入组\n")
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
        "受试者%s的%s为%.2f（正常范围：%.2f - %.2f）",
        row["SUBJID"],
        row["LBTEST"],
        row["TEST_VALUE"],
        row["LLN"],
        row["ULN"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
