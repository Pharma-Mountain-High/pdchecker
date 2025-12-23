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
#'
#' @note Date variables in other datasets are identified by variable names ending with "DAT"
#' (e.g., LBDAT, VSDAT, AEENDAT). Variables not following this naming convention will not be checked.
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with default parameters
#' data <- list(
#'   IC = data.frame(
#'     SUBJID = c("001", "002", "003"),
#'     ICDAT = c("2024-01-15", "2024-02-01", "2024-03-01")
#'   ),
#'   LB = data.frame(
#'     SUBJID = c("001", "002", "003"),
#'     LBDAT = c("2024-01-10", "2024-02-05", "2024-03-05")
#'   )
#' )
#' result <- check_icf_time_deviation(data)
#' print(result)
#'
#' # Example 2: Exclude specific datasets from check
#' result <- check_icf_time_deviation(
#'   data,
#'   exclude_datasets = c("DM", "DS")
#' )
#'
#' # Example 3: Ignore additional date variables
#' result <- check_icf_time_deviation(
#'   data,
#'   ignore_vars = c("BRTHDAT", "MHSTDAT", "DSSTDAT")
#' )
#'
#' # Example 4: Use custom IC dataset and variable names
#' data_custom <- list(
#'   ICF = data.frame(
#'     SUBJID = c("001", "002"),
#'     ICFDAT = c("2024-01-15", "2024-02-01")
#'   ),
#'   VS = data.frame(
#'     SUBJID = c("001", "002"),
#'     VSDAT = c("2024-01-10", "2024-02-05")
#'   )
#' )
#' result <- check_icf_time_deviation(
#'   data_custom,
#'   ic_dataset = "ICF",
#'   ic_date_var = "ICFDAT"
#' )
#' }
#'
#' @family deviation checks
#' @importFrom dplyr filter mutate select left_join all_of arrange group_by slice ungroup bind_rows
#' @importFrom rlang .data
#' @export
check_icf_time_deviation <- function(data,
                                     ic_dataset = "IC",
                                     ic_date_var = "ICDAT",
                                     ignore_vars = "BRTHDAT",
                                     exclude_datasets = NULL) {
  # Validate parameter types
  if (!is.list(data)) {
    stop("'data' must be a list of data frames")
  }
  if (!is.character(ic_dataset) || length(ic_dataset) != 1) {
    stop("'ic_dataset' must be a single character string")
  }
  if (!is.character(ic_date_var) || length(ic_date_var) != 1) {
    stop("'ic_date_var' must be a single character string")
  }
  if (!is.character(ignore_vars)) {
    stop("'ignore_vars' must be a character vector")
  }
  if (!is.null(exclude_datasets) && !is.character(exclude_datasets)) {
    stop("'exclude_datasets' must be NULL or a character vector")
  }

  # Validate required datasets
  if (!ic_dataset %in% names(data)) {
    stop(paste0("Missing IC dataset: ", ic_dataset))
  }

  # Validate that ic_date_var exists in ic_dataset
  if (!ic_date_var %in% names(data[[ic_dataset]])) {
    stop(sprintf("Variable '%s' not found in dataset '%s'", ic_date_var, ic_dataset))
  }

  # Validate that SUBJID exists in ic_dataset
  if (!"SUBJID" %in% names(data[[ic_dataset]])) {
    stop(sprintf("'SUBJID' column not found in dataset '%s'", ic_dataset))
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

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
