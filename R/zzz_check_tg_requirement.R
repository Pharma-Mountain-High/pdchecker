#' Check triglyceride (TG) requirements
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate if_else
#' @noRd
check_tg_requirement <- function(data) {
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

  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get baseline TG values
  baseline_tg <- data$LB %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    filter(LBTEST == "甘油三酯" & VISITNAME == "筛选期V1") %>%
    select(SUBJID, LBTEST, LBORRES, LBORRESU) %>%
    filter(LBORRESU != "") %>%
    mutate(
      TG_VALUE = as.numeric(LBORRES),
      TG_UNIT = LBORRESU
    )

  # Check TG requirement (< 4.5 mmol/L or < 400 mg/dL)
  deviations <- baseline_tg %>%
    filter(
      if_else(TG_UNIT == "mg/dL",
        TG_VALUE >= 400,
        TG_VALUE >= 4.5
      )
    )

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "不符合入组标准3，筛选时空腹甘油三酯（TG）≥400 mg/dL（4.5 mmol/L）或未空腹"
    results$details <- deviations
  }

  class(results) <- c("tg_check", "list")
  return(results)
}

#' Print method for TG requirement check results
#' @param x Object of class tg_check
#' @param ... Additional arguments
#' @noRd
print.tg_check <- function(x, ...) {
  cat("3.1不符合入选标准却入组\n")
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
        "受试者%s的空腹甘油三酯为%s %s",
        row["SUBJID"],
        row["TG_VALUE"],
        row["TG_UNIT"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
