#' Expand date variable names to match dataset count
#'
#' @description
#' Internal helper function to expand date variable names to match the number of datasets.
#' If a single variable name is provided, it is replicated for all datasets.
#'
#' @param date_var Character vector, date variable names
#' @param datasets Character vector, dataset names
#' @param var_name Character, name of the variable for error messages
#'
#' @return Character vector of date variable names matching the length of datasets
#'
#' @keywords internal
#' @noRd
expand_date_vars <- function(date_var, datasets, var_name = "date_var") {
  if (length(date_var) == 1) {
    return(rep(date_var, length(datasets)))
  } else if (length(date_var) == length(datasets)) {
    return(date_var)
  } else {
    stop(
      var_name, " length must be 1 or equal to ex_datasets length. ex_datasets length: ",
      length(datasets), ", ", var_name, " length: ", length(date_var)
    )
  }
}

#' Get First Dose Date for Each Subject
#'
#' @description
#' Extract the earliest dosing start date from exposure datasets for each subject.
#'
#' @details
#' ## Calculation Rules
#'
#' - Extracts **earliest** dosing start date from all specified exposure datasets
#' - Always uses dosing **start date** variable (\code{ex_date_var})
#' - Supports multiple datasets, automatically merges and takes minimum date per subject
#' - SAS missing values (NA, ".", "") are automatically excluded
#'
#' ## Multiple Datasets Support
#'
#' When multiple datasets are specified:
#' - All datasets are combined
#' - For each subject, the minimum date across all datasets is returned
#' - Missing datasets are silently skipped
#'
#' @param data List containing clinical trial datasets
#' @param ex_datasets Character vector, exposure dataset names (default: "EX").
#'   Multiple datasets can be specified, e.g., c("EX1", "EX2")
#' @param ex_date_var Character vector, dosing start date variable names (default: "EXSTDAT").
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{SUBJID}{Character. Subject ID}
#'     \item{first_dose_date}{Date. First dose date}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with single dataset
#' data <- list(
#'   EX = data.frame(
#'     SUBJID = c("001", "001", "002"),
#'     EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' first_dates <- get_first_dose_date(data)
#' # Subject 001: 2024-01-01 (min of 2024-01-01 and 2024-01-29)
#' # Subject 002: 2024-01-05
#'
#' # Multiple datasets with same column name
#' data <- list(
#'   EX1 = data.frame(
#'     SUBJID = c("001", "001"),
#'     EXSTDAT = c("2024-01-15", "2024-02-01"),
#'     stringsAsFactors = FALSE
#'   ),
#'   EX2 = data.frame(
#'     SUBJID = c("001", "002"),
#'     EXSTDAT = c("2024-01-10", "2024-01-20"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' first_dates <- get_first_dose_date(
#'   data,
#'   ex_datasets = c("EX1", "EX2"),
#'   ex_date_var = "EXSTDAT"
#' )
#' # Subject 001: 2024-01-10 (min across EX1 and EX2)
#' # Subject 002: 2024-01-20
#'
#' # Multiple datasets with different column names
#' data <- list(
#'   EX1 = data.frame(
#'     SUBJID = "001",
#'     STDAT1 = "2024-01-15",
#'     stringsAsFactors = FALSE
#'   ),
#'   EX2 = data.frame(
#'     SUBJID = "001",
#'     STDAT2 = "2024-01-10",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' first_dates <- get_first_dose_date(
#'   data,
#'   ex_datasets = c("EX1", "EX2"),
#'   ex_date_var = c("STDAT1", "STDAT2")
#' )
#' # Subject 001: 2024-01-10
#' }
#'
#' @seealso \code{\link{get_last_dose_date}} for extracting last dose dates
#'
#' @family date extraction
#'
#' @importFrom dplyr filter mutate select group_by ungroup slice sym bind_rows
#' @importFrom magrittr %>%
#' @export
get_first_dose_date <- function(data,
                                ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
                                ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT")) {
  # Parameter validation
  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  if (!is.character(ex_datasets) || length(ex_datasets) == 0) {
    stop("'ex_datasets' must be a non-empty character vector")
  }

  if (!is.character(ex_date_var) || length(ex_date_var) == 0) {
    stop("'ex_date_var' must be a non-empty character vector")
  }

  # Expand date variable names to match dataset count
  ex_date_vars <- expand_date_vars(ex_date_var, ex_datasets, "ex_date_var")

  # Collect first dose dates
  first_dose_dates_list <- list()

  for (i in seq_along(ex_datasets)) {
    ex_ds <- ex_datasets[i]
    current_date_var <- ex_date_vars[i]

    if (ex_ds %in% names(data)) {
      ex_df <- data[[ex_ds]]
      if (current_date_var %in% names(ex_df) && "SUBJID" %in% names(ex_df)) {
        ex_dates <- ex_df %>%
          filter(!is_sas_na(!!sym(current_date_var))) %>%
          mutate(dose_date = as.Date(!!sym(current_date_var))) %>%
          filter(!is.na(dose_date)) %>%
          select(SUBJID, dose_date)

        if (nrow(ex_dates) > 0) {
          first_dose_dates_list[[length(first_dose_dates_list) + 1]] <- ex_dates
        }
      }
    }
  }

  # Calculate first dose date
  if (length(first_dose_dates_list) > 0) {
    first_dose_dates <- bind_rows(first_dose_dates_list) %>%
      group_by(SUBJID) %>%
      filter(dose_date == min(dose_date)) %>%
      slice(1) %>%
      ungroup() %>%
      select(SUBJID, first_dose_date = dose_date)
  } else {
    first_dose_dates <- data.frame(
      SUBJID = character(),
      first_dose_date = as.Date(character()),
      stringsAsFactors = FALSE
    )
  }

  return(first_dose_dates)
}

#' Get Last Dose Date for Each Subject
#'
#' @description
#' Extract the latest dosing date from exposure datasets for each subject.
#' Uses end date if specified, otherwise uses start date.
#'
#' @details
#' ## Calculation Rules
#'
#' The function uses different logic based on whether \code{ex_end_date_var} is specified:
#'
#' ### When ex_end_date_var is NOT specified (default)
#' - Uses dosing **start date** (\code{ex_date_var})
#' - Takes the **latest** dosing start date from all records
#'
#' ### When ex_end_date_var IS specified
#' - Uses dosing **end date** (\code{ex_end_date_var})
#' - Takes the **latest** dosing end date from all records
#' - Suitable for studies with explicit start and end dates per dose
#'
#' ## Multiple Datasets Support
#'
#' When multiple datasets are specified:
#' - All datasets are combined
#' - For each subject, the maximum date across all datasets is returned
#' - Missing datasets are silently skipped
#'
#' @param data List containing clinical trial datasets
#' @param ex_datasets Character vector, exposure dataset names (default: "EX").
#'   Multiple datasets can be specified, e.g., c("EX1", "EX2")
#' @param ex_date_var Character vector, dosing start date variable names (default: "EXSTDAT").
#'   Used when \code{ex_end_date_var} is not specified.
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#' @param ex_end_date_var Character vector, dosing end date variable names (default: NULL).
#'   If specified, uses end date for last dose calculation.
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{SUBJID}{Character. Subject ID}
#'     \item{last_dose_date}{Date. Last dose date}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage - using start date as last dose date
#' data <- list(
#'   EX = data.frame(
#'     SUBJID = c("001", "001", "002"),
#'     EXSTDAT = c("2024-01-01", "2024-02-15", "2024-01-05"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' last_dates <- get_last_dose_date(data)
#' # Subject 001: 2024-02-15 (max of start dates)
#' # Subject 002: 2024-01-05
#'
#' # Using end date for last dose
#' data <- list(
#'   EX = data.frame(
#'     SUBJID = c("001", "001"),
#'     EXSTDAT = c("2024-01-01", "2024-02-01"),
#'     EXENDAT = c("2024-01-28", "2024-02-28"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' last_dates <- get_last_dose_date(
#'   data,
#'   ex_end_date_var = "EXENDAT"
#' )
#' # Subject 001: 2024-02-28 (max of end dates)
#'
#' # Multiple datasets with same column name
#' data <- list(
#'   EX1 = data.frame(
#'     SUBJID = c("001", "001"),
#'     EXSTDAT = c("2024-01-01", "2024-02-01"),
#'     stringsAsFactors = FALSE
#'   ),
#'   EX2 = data.frame(
#'     SUBJID = c("001", "002"),
#'     EXSTDAT = c("2024-03-01", "2024-01-15"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' last_dates <- get_last_dose_date(
#'   data,
#'   ex_datasets = c("EX1", "EX2"),
#'   ex_date_var = "EXSTDAT"
#' )
#' # Subject 001: 2024-03-01 (max across EX1 and EX2)
#' # Subject 002: 2024-01-15
#'
#' # Multiple datasets with different column names
#' data <- list(
#'   EX1 = data.frame(
#'     SUBJID = "001",
#'     STDAT1 = "2024-01-15",
#'     stringsAsFactors = FALSE
#'   ),
#'   EX2 = data.frame(
#'     SUBJID = "001",
#'     STDAT2 = "2024-03-20",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' last_dates <- get_last_dose_date(
#'   data,
#'   ex_datasets = c("EX1", "EX2"),
#'   ex_date_var = c("STDAT1", "STDAT2")
#' )
#' # Subject 001: 2024-03-20
#' }
#'
#' @seealso \code{\link{get_first_dose_date}} for extracting first dose dates
#'
#' @family date extraction
#'
#' @importFrom dplyr filter mutate select group_by ungroup slice sym bind_rows
#' @importFrom magrittr %>%
#' @export
get_last_dose_date <- function(data,
                               ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
                               ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT"),
                               ex_end_date_var = getOption("pdchecker.ex_end_date_var", NULL)) {
  # Parameter validation
  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  if (!is.character(ex_datasets) || length(ex_datasets) == 0) {
    stop("'ex_datasets' must be a non-empty character vector")
  }

  if (!is.character(ex_date_var) || length(ex_date_var) == 0) {
    stop("'ex_date_var' must be a non-empty character vector")
  }

  # Expand date variable names
  ex_date_vars <- expand_date_vars(ex_date_var, ex_datasets, "ex_date_var")

  # Expand end date variable names (if specified)
  use_end_date <- !is.null(ex_end_date_var) && length(ex_end_date_var) > 0
  if (use_end_date) {
    ex_end_date_vars <- expand_date_vars(ex_end_date_var, ex_datasets, "ex_end_date_var")
  }

  # Collect last dose dates
  last_dose_dates_list <- list()

  for (i in seq_along(ex_datasets)) {
    ex_ds <- ex_datasets[i]
    # Use end date if specified; otherwise use start date
    current_date_var <- if (use_end_date) ex_end_date_vars[i] else ex_date_vars[i]

    if (ex_ds %in% names(data)) {
      ex_df <- data[[ex_ds]]
      if (current_date_var %in% names(ex_df) && "SUBJID" %in% names(ex_df)) {
        ex_dates <- ex_df %>%
          filter(!is_sas_na(!!sym(current_date_var))) %>%
          mutate(dose_date = as.Date(!!sym(current_date_var))) %>%
          filter(!is.na(dose_date)) %>%
          select(SUBJID, dose_date)

        if (nrow(ex_dates) > 0) {
          last_dose_dates_list[[length(last_dose_dates_list) + 1]] <- ex_dates
        }
      }
    }
  }

  # Calculate last dose date
  if (length(last_dose_dates_list) > 0) {
    last_dose_dates <- bind_rows(last_dose_dates_list) %>%
      group_by(SUBJID) %>%
      filter(dose_date == max(dose_date)) %>%
      slice(1) %>%
      ungroup() %>%
      select(SUBJID, last_dose_date = dose_date)
  } else {
    last_dose_dates <- data.frame(
      SUBJID = character(),
      last_dose_date = as.Date(character()),
      stringsAsFactors = FALSE
    )
  }

  return(last_dose_dates)
}

#' Get End of Treatment Date for Each Subject
#'
#' @description
#' Extract the end of treatment date from the EOT dataset for each subject.
#'
#' @details
#' ## Calculation Rules
#'
#' - Extracts the EOT date from the specified dataset and variable
#' - If the dataset or variable doesn't exist, returns empty data frame
#' - SAS missing values (NA, ".", "") are automatically excluded
#' - If multiple records exist for a subject, returns distinct values
#'
#' @param data List containing clinical trial datasets
#' @param eot_dataset Character string, EOT dataset name (default: "EOT")
#' @param eot_date_var Character string, EOT date variable name (default: "EOTDAT")
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{SUBJID}{Character. Subject ID}
#'     \item{eot_date}{Date. End of treatment date}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- list(
#'   EOT = data.frame(
#'     SUBJID = c("001", "002"),
#'     EOTDAT = c("2024-06-01", "2024-06-15"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' eot_dates <- get_eot_date(data)
#' }
#'
#' @seealso \code{\link{get_eos_date}} for extracting end of study dates
#'
#' @family date extraction
#'
#' @importFrom dplyr filter mutate select distinct sym
#' @importFrom magrittr %>%
#' @export
get_eot_date <- function(data,
                         eot_dataset = getOption("pdchecker.eot_dataset", "EOT"),
                         eot_date_var = getOption("pdchecker.eot_date_var", "EOTDAT")) {
  # Parameter validation
  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  if (!is.character(eot_dataset) || length(eot_dataset) != 1) {
    stop("'eot_dataset' must be a single character string")
  }

  if (!is.character(eot_date_var) || length(eot_date_var) != 1) {
    stop("'eot_date_var' must be a single character string")
  }

  # Initialize empty result
  eot_dates <- data.frame(
    SUBJID = character(),
    eot_date = as.Date(character()),
    stringsAsFactors = FALSE
  )

  # Get end of treatment date
  if (eot_dataset %in% names(data)) {
    eot_df <- data[[eot_dataset]]
    if (eot_date_var %in% names(eot_df) && "SUBJID" %in% names(eot_df)) {
      eot_dates <- eot_df %>%
        filter(!is_sas_na(!!sym(eot_date_var))) %>%
        mutate(eot_date = as.Date(!!sym(eot_date_var))) %>%
        select(SUBJID, eot_date) %>%
        distinct()
    }
  }

  return(eot_dates)
}

#' Get End of Study Date for Each Subject
#'
#' @description
#' Extract the end of study date from the DS dataset for each subject.
#'
#' @details
#' ## Calculation Rules
#'
#' - Extracts the EOS date from the specified dataset and variable
#' - If the dataset or variable doesn't exist, returns empty data frame
#' - SAS missing values (NA, ".", "") are automatically excluded
#' - If multiple records exist for a subject, returns distinct values
#'
#' @param data List containing clinical trial datasets
#' @param ds_dataset Character string, DS dataset name (default: "DS")
#' @param ds_date_var Character string, EOS date variable name (default: "DSDAT")
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{SUBJID}{Character. Subject ID}
#'     \item{eos_date}{Date. End of study date}
#'   }
#'
#' @examples
#' \dontrun{
#' data <- list(
#'   DS = data.frame(
#'     SUBJID = c("001", "002"),
#'     DSDAT = c("2024-09-01", "2024-09-15"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' eos_dates <- get_eos_date(data)
#' }
#'
#' @seealso \code{\link{get_eot_date}} for extracting end of treatment dates
#'
#' @family date extraction
#'
#' @importFrom dplyr filter mutate select distinct sym
#' @importFrom magrittr %>%
#' @export
get_eos_date <- function(data,
                         ds_dataset = getOption("pdchecker.ds_dataset", "DS"),
                         ds_date_var = getOption("pdchecker.ds_date_var", "DSDAT")) {
  # Parameter validation
  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  if (!is.character(ds_dataset) || length(ds_dataset) != 1) {
    stop("'ds_dataset' must be a single character string")
  }

  if (!is.character(ds_date_var) || length(ds_date_var) != 1) {
    stop("'ds_date_var' must be a single character string")
  }

  # Initialize empty result
  eos_dates <- data.frame(
    SUBJID = character(),
    eos_date = as.Date(character()),
    stringsAsFactors = FALSE
  )

  # Get end of study date
  if (ds_dataset %in% names(data)) {
    ds_df <- data[[ds_dataset]]
    if (ds_date_var %in% names(ds_df) && "SUBJID" %in% names(ds_df)) {
      eos_dates <- ds_df %>%
        filter(!is_sas_na(!!sym(ds_date_var))) %>%
        mutate(eos_date = as.Date(!!sym(ds_date_var))) %>%
        select(SUBJID, eos_date) %>%
        distinct()
    }
  }

  return(eos_dates)
}
