#' Check randomization stratification factor deviation
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter mutate left_join select right_join rowwise
#' @noRd
check_random_deviation <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("SF", "IWRS", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomized subjects
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  planned_factors <- data$IWRS %>%
    rowwise() %>%
    select(all_of(c("随机号", "分层因素"))) %>%
    mutate(
      RANDID = `随机号`,
      STRAT1 = strsplit(`分层因素`, "\\*")[[1]][1],
      STRAT2 = strsplit(`分层因素`, "\\*")[[1]][2]
    ) %>%
    right_join(rand_subjects, by = "RANDID") %>%
    select(RANDID, SUBJID, STRAT1, STRAT2, `分层因素`)

  actual_factors <- data$SF %>%
    # Get statin use before randomization
    select(SUBJID, RANDTT, RANDLDL)

  # Compare with IWRS factors
  comparison <- planned_factors %>%
    left_join(actual_factors, by = "SUBJID") %>%
    mutate(
      STRAT1FL = if_else(STRAT1 != RANDLDL, "Y", "N"),
      STRAT2FL = if_else(STRAT2 != RANDTT, "Y", "N"),
      has_deviation = (STRAT1FL == "Y") |
        (STRAT2FL == "Y")
    )

  # Extract deviations
  deviations <- comparison %>%
    filter(has_deviation) %>%
    mutate(
      iwrs_value = ifelse(STRAT1FL == "Y", STRAT1, STRAT2),
      actual_value = ifelse(STRAT1FL == "Y", RANDLDL, RANDTT)
    ) %>%
    select(
      SUBJID,
      iwrs_value, actual_value
    )

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- sprintf(
      "受试者随机分层因素出现错误"
    )
    results$details <- deviations
  }

  class(results) <- c("rand_deviation", "list")
  return(results)
}

#' Print method for randomization deviation check results
#' @param x Object of class rand_deviation
#' @param ... Additional arguments
#' @noRd
print.rand_deviation <- function(x, ...) {
  cat("1.1 随机分层因素出现错误\n")
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
    # Format each row of details into the Chinese message format
    formatted_details <- apply(x$details, 1, function(row) {
      sprintf(
        "%s受试者在IWRS系统中选择的随机分层因素为%s，实际随机分层因素为%s。",
        row["SUBJID"], # Assuming 'subjid' is the column name for subject ID
        row["iwrs_value"], # Assuming 'iwrs_value' is the column for IWRS factor
        row["actual_value"]
      ) # Assuming 'actual_value' is the column for actual factor
    })
    cat(formatted_details, sep = "\n")
  }
}
