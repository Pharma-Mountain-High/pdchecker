#' Check if any time variables occur before informed consent time
#'
#' @param data List of data frames containing study data
#' @param ignore_vars Character vector of variables to ignore in the check (default: "BRTHDAT")
#' @return List containing check results and descriptions
#' @importFrom dplyr filter mutate select left_join
#' @importFrom  lubridate is.Date
#' @export
check_icf_time_deviation <- function(data, ignore_vars = "BRTHDAT") {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Get ICF date/time
  if (!"IC" %in% names(data)) {
    stop("Missing ICF dataset")
  }

  icf_times <- data$IC %>%
    select(SUBJID, ICDAT) %>%
    mutate(icf_datetime = as.POSIXct(ICDAT))

  # Find all time variables across datasets
  time_deviations <- data.frame()

  for (dataset_name in names(data)) {
    df <- data[[dataset_name]]
    # Look for date/time columns (common suffixes)
    date_cols <- names(df)[sapply(df, function(col) lubridate::is.Date(col))]

    # Remove ignored variables from the check
    date_cols <- setdiff(date_cols, ignore_vars)

    if (length(date_cols) > 0) {
      for (col in date_cols) {
        df_times <- df %>%
          select(SUBJID, all_of(col)) %>%
          mutate(
            dataset = dataset_name,
            variable = col,
            action = paste(as.character(dataset_name), as.character(col), sep = "."),
            event_datetime = as.POSIXct(get(col))
          ) %>%
          left_join(icf_times, by = "SUBJID") %>%
          filter(!is.na(event_datetime) & !is.na(icf_datetime) &
            event_datetime < icf_datetime) %>%
          select(SUBJID, action, event_datetime, icf_datetime) %>%
          mutate(diff_date = as.numeric(difftime(event_datetime, icf_datetime, units = "days")))


        if (nrow(df_times) > 0) {
          time_deviations <- rbind(time_deviations, df_times)
        }
      }
    }
  }

  # Compile results
  if (nrow(time_deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "执行任何临床研究程序前未事先获得书面知情同意书"
    results$details <- time_deviations
  }

  class(results) <- c("icf_time_deviation", "list")
  return(results)
}

#' Print method for ICF time deviation check results
#' @param x Object of class icf_time_deviation
#' @param ... Additional arguments
#' @export
print.icf_time_deviation <- function(x, ...) {
  cat("2.1 获得ICF前进行了试验相关操作\n")
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
        "%s受试者,首次知情同意书在%s签署,但在%s进行了操作%s，早于知情同意时间%s天",
        row["SUBJID"],
        row["icf_datetime"],
        row["event_datetime"],
        row["action"],
        row["diff_date"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
