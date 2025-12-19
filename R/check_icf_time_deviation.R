#' Check if any time variables occur before informed consent time
#'
#' @param data List of data frames containing study data
#' @param ic_dataset Character string specifying the name of the IC dataset (default: "IC")
#' @param ic_date_var Character string specifying the name of the IC date variable (default: "ICDAT")
#' @param ignore_vars Character vector of variables to ignore in the check (default: "BRTHDAT").
#' Can specify multiple variables, e.g., c("BRTHDAT", "MHSTDAT")
#' @param exclude_datasets Character vector of dataset names to exclude from the check (default: NULL).
#' Can specify multiple datasets, e.g., c("DM", "DS")
#' @return A list of class "icf_time_deviation" with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if any time deviations were found, FALSE otherwise}
#'     \item{messages}{Character vector. Summary message describing the deviation}
#'     \item{details}{Data frame. Contains detailed deviation records with columns:
#'       \itemize{
#'         \item SUBJID: Subject identifier
#'         \item action: Dataset and variable name (format: "dataset.variable")
#'         \item event_datetime: Date when the event occurred
#'         \item icf_datetime: Date when informed consent was obtained
#'         \item diff_date: Numeric. Difference in days (negative values indicate event occurred before IC)
#'       }
#'     }
#'   }
#' @importFrom dplyr filter mutate select left_join all_of arrange group_by slice ungroup bind_rows
#' @export
check_icf_time_deviation <- function(data,
                                     ic_dataset = "IC",
                                     ic_date_var = "ICDAT",
                                     ignore_vars = "BRTHDAT",
                                     exclude_datasets = NULL) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Get ICF date/time
  if (!ic_dataset %in% names(data)) {
    stop(paste0("Missing IC dataset: ", ic_dataset))
  }

  icf_times <- data[[ic_dataset]] %>%
    select(SUBJID, all_of(ic_date_var)) %>%
    mutate(icf_datetime = tryCatch(
      as.Date(.data[[ic_date_var]]),
      error = function(e) as.Date(NA),
      warning = function(w) as.Date(NA)
    ))

  # Find all time variables across datasets
  # Use list to collect results for better performance
  time_deviations_list <- list()

  for (dataset_name in names(data)) {
    # Skip excluded datasets
    if (!is.null(exclude_datasets) && dataset_name %in% exclude_datasets) {
      next
    }

    df <- data[[dataset_name]]
    # Look for date/time columns (variables ending with "DAT")
    date_cols <- names(df)[grepl("DAT$", names(df))]

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
            event_datetime = tryCatch(
              as.Date(.data[[col]]),
              error = function(e) as.Date(NA),
              warning = function(w) as.Date(NA)
            )
          ) %>%
          left_join(icf_times, by = "SUBJID") %>%
          filter(!is.na(event_datetime) & !is.na(icf_datetime) & event_datetime < icf_datetime) %>%
          select(SUBJID, action, event_datetime, icf_datetime) %>%
          mutate(diff_date = as.numeric(difftime(event_datetime, icf_datetime, units = "days")))

        if (nrow(df_times) > 0) {
          # Add to list instead of rbind for better performance
          time_deviations_list[[length(time_deviations_list) + 1]] <- df_times
        }
      }
    }
  }

  # Combine all results and remove duplicates (keep earliest event per subject-action)
  if (length(time_deviations_list) > 0) {
    time_deviations <- bind_rows(time_deviations_list) %>%
      arrange(SUBJID, action, event_datetime) %>%
      group_by(SUBJID, action) %>%
      slice(1) %>%
      ungroup()
  } else {
    time_deviations <- data.frame()
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
        "受试者%s,首次知情同意书在%s签署，但在%s进行了操作%s，早于知情同意时间%s天",
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
