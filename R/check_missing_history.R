#' Check for missing medical history data
#'
#' @param data List of data frames containing study data
#' @return List of data frames with missing history information
#' @importFrom dplyr filter select mutate left_join arrange bind_rows
#' @export
check_missing_history <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("PH")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Extract subject information if available
  subject_data <- if ("IC" %in% names(data)) {
    data$IC %>%
      select(SUBJID, SITEID)
  } else {
    data$PH %>%
      select(SUBJID) %>%
      distinct()
  }

  # Check for missing smoking history
  missing_drug <- data$PH %>%
    filter(is_sas_na(PHDRUG)) %>%
    select(SUBJID) %>%
    left_join(subject_data, by = "SUBJID") %>%
    mutate(issue_type = "吸毒史缺失")

  # Check for missing drinking history
  missing_drinking <- data$PH %>%
    filter(is_sas_na(PHDRSTAT)) %>%
    select(SUBJID) %>%
    left_join(subject_data, by = "SUBJID") %>%
    mutate(issue_type = "酗酒史缺失")

  # Check for missing medication use history
  missing_medication <- data$PH %>%
    filter(is_sas_na(PHSPERF)) %>%
    select(SUBJID) %>%
    left_join(subject_data, by = "SUBJID") %>%
    mutate(issue_type = "药物滥用史缺失")

  # Combine all missing history issues
  all_missing <- bind_rows(
    missing_drug,
    missing_drinking,
    missing_medication
  ) %>%
    arrange(SUBJID, issue_type)

  # Compile results
  if (nrow(all_missing) > 0) {
    results$has_deviation <- TRUE
    results$messages <- c(
      sprintf("存在%d例受试者的个人史记录不完整", nrow(all_missing))
    )
    results$details <- all_missing
  }

  class(results) <- c("missing_history_check", "list")
  return(results)
}

#' Print method for missing history check
#'
#' @param x Object of class missing_history_check
#' @param ... Additional arguments
#' @return Invisible x
#' @export
print.missing_history_check <- function(x, ...) {
  cat("5.2 个人史记录不完整\n")
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

    # Group by issue_type
    issue_counts <- table(x$details$issue_type)

    # Print summary by type
    cat(sprintf("共有%d例缺失记录:\n", nrow(x$details)))
    for (issue_type in names(issue_counts)) {
      cat(sprintf("- %s: %d例\n", issue_type, issue_counts[issue_type]))
    }

    # Format each row of details
    cat("\n详细缺失记录:\n")
    formatted_details <- apply(x$details, 1, function(row) {
      sprintf(
        "受试者%s：%s",
        row["SUBJID"],
        row["issue_type"]
      )
    })

    cat(formatted_details, sep = "\n")
  }

  invisible(x)
}
