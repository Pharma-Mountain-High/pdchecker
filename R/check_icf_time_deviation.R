#' Check if any time variables occur before informed consent time
#'
#' @param data List of data frames containing study data
#' @param ic_dataset Character string specifying the name of the IC dataset (default: "IC")
#' @param ic_date_var Character string specifying the name of the IC date variable (default: "ICDAT")
#' @param ignore_vars Character vector of variables to ignore in the check (default: "BRTHDAT").
#' Can specify multiple variables, e.g., c("BRTHDAT", "MHSTDAT")
#' @param exclude_datasets Character vector of dataset names to exclude from the check (default: NULL).
#' Can specify multiple datasets, e.g., c("DM", "DS")
#' @param tb_name_var Character string specifying the variable name to use as TBNAME in the output (default: NULL).
#' If NULL, the TBNAME column in the output will be empty.
#' @param pdno Character string specifying the protocol deviation number for this check (default: "2.1.1")
#' @return A list of class "icf_time_deviation" with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if any time deviations were found, FALSE otherwise}
#'     \item{messages}{Character vector. Summary message describing the deviation}
#'     \item{details}{Data frame. Contains detailed deviation records with columns:
#'       \itemize{
#'         \item PDNO: Protocol deviation number specified by \code{pdno} parameter
#'         \item SUBJID: Subject identifier
#'         \item action: Dataset and variable name (format: "dataset.variable")
#'         \item event_datetime: Date when the event occurred
#'         \item icf_datetime: Date when informed consent was obtained
#'         \item diff_date: Numeric. Difference in days (negative values indicate event occurred before IC)
#'         \item TBNAME: Table name from the variable specified by \code{tb_name_var}, empty if \code{tb_name_var} is NULL
#'         \item DESCRIPTION: Description of the deviation for each record
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
                                     ic_dataset = getOption("pdchecker.ic_dataset", "IC"),
                                     ic_date_var = getOption("pdchecker.ic_date_var", "ICDAT"),
                                     ignore_vars = "BRTHDAT",
                                     exclude_datasets = NULL,
                                     tb_name_var = getOption("pdchecker.tb_name_var", NULL),
                                     pdno = "2.1.1") {
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
  if (!is.null(tb_name_var) && (!is.character(tb_name_var) || length(tb_name_var) != 1)) {
    stop("'tb_name_var' must be NULL or a single character string")
  }
  if (!is.character(pdno) || length(pdno) != 1) {
    stop("'pdno' must be a single character string")
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
    details = data.frame(
      PDNO = character(),
      SUBJID = character(),
      action = character(),
      event_datetime = as.Date(character()),
      icf_datetime = as.Date(character()),
      diff_date = numeric(),
      TBNAME = character(),
      DESCRIPTION = character(),
      stringsAsFactors = FALSE
    )
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
        # Determine columns to select based on tb_name_var
        select_cols <- c("SUBJID", col)
        if (!is.null(tb_name_var) && tb_name_var %in% names(df)) {
          select_cols <- c(select_cols, tb_name_var)
        }

        df_times <- df %>%
          select(all_of(select_cols)) %>%
          mutate(
            dataset = dataset_name,
            variable = col,
            action = paste(as.character(dataset_name), as.character(col), sep = "."),
            event_datetime = tryCatch(
              as.Date(.data[[col]]),
              error = function(e) as.Date(NA),
              warning = function(w) as.Date(NA)
            ),
            TBNAME = if (!is.null(tb_name_var) && tb_name_var %in% names(df)) {
              as.character(.data[[tb_name_var]])
            } else {
              ""
            }
          ) %>%
          left_join(icf_times, by = "SUBJID") %>%
          filter(!is.na(event_datetime) & !is.na(icf_datetime) & event_datetime < icf_datetime) %>%
          select(SUBJID, action, event_datetime, icf_datetime, TBNAME) %>%
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
      group_by(SUBJID, action, TBNAME) %>%
      slice(1) %>%
      ungroup()
  } else {
    time_deviations <- data.frame()
  }

  # Compile results
  if (nrow(time_deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "执行任何临床研究程序前未事先获得书面知情同意书"
    results$details <- data.frame(
      PDNO = pdno,
      SUBJID = time_deviations$SUBJID,
      action = time_deviations$action,
      event_datetime = time_deviations$event_datetime,
      icf_datetime = time_deviations$icf_datetime,
      diff_date = time_deviations$diff_date,
      TBNAME = time_deviations$TBNAME,
      DESCRIPTION = sprintf(
        "受试者%s，首次知情同意书在%s签署，但在%s进行了操作[%s]，早于知情同意时间%s天",
        time_deviations$SUBJID,
        time_deviations$icf_datetime,
        time_deviations$event_datetime,
        time_deviations$TBNAME,
        as.character(abs(time_deviations$diff_date))
      ),
      stringsAsFactors = FALSE
    )
  }

  class(results) <- c("icf_time_deviation", "list")
  return(results)
}

#' Print method for ICF time deviation check results
#' @param x Object of class icf_time_deviation
#' @param ... Additional arguments
#' @export
print.icf_time_deviation <- function(x, ...) {
  pdno_display <- if (nrow(x$details) > 0) x$details$PDNO[1] else "2.1.1"
  cat(sprintf("%s 获得ICF前进行了试验相关操作\n", pdno_display))
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
