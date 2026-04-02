#' Generate Planned Visit Dates
#'
#' @description
#' Calculate planned visit dates and visit window ranges for each subject based on
#' visit schedule data and clinical trial data. Supports screening, treatment,
#' end of treatment, and follow-up visit date calculations.
#'
#' @details
#' ## Visit Categories
#'
#' The function handles four types of visits:
#'
#' | Category | Description |
#' |----------|-------------|
#' | Screening | Based on first dose date |
#' | Treatment | D1 visits use iterative cycle calculation; non-D1 based on cycle D1 date |
#' | End of Treatment | Based on EOT date or EOS date |
#' | Follow-up | Based on EOT date or last dose date |
#'
#' For detailed calculation rules of each visit type, see the internal calculation
#' functions in `planned_date_calculation.R`.
#'
#' ## Visit Window Calculation
#'
#' Window ranges are calculated based on the window type specified in visit schedule:
#'
#' | Window Type | Window Range |
#' |-------------|--------------|
#' | +/-Nd | `[planned - N, planned + N]` |
#' | +Nd | `[planned, planned + N]` |
#' | -Nd | `[planned - N, planned]` |
#' @param data List containing all clinical trial datasets
#' @param visitcode Data frame, visit schedule data from \code{\link{read_visitcode_file}}
#'   (default: NULL). If NULL, the function will look for a variable named \code{visitcode}
#'   in the calling environment. If not found, an error will be raised.
#'   Must contain columns: VISIT, VISITNUM (numeric), WP, CYCLE, VISITDAY, type, wpvalue (numeric)
#' @param ex_datasets Character vector, exposure dataset names (default: "EX").
#'   Multiple datasets can be specified, e.g., c("EX1", "EX2")
#' @param ex_date_var Character vector, dosing start date variable names (default: "EXSTDAT").
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#'   - Example: c("EXSTDAT1", "EXSTDAT2") corresponds to c("EX1", "EX2")
#' @param ex_end_date_var Character vector, dosing end date variable names (default: NULL).
#'   - If NULL or empty, last dose date uses \code{ex_date_var} (dosing start date)
#'   - If specified, last dose date uses dosing end date
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#' @param sv_dataset Character string, visit dataset name (default: "SV")
#' @param sv_visit_var Character string, visit name variable in visit dataset (default: "VISIT")
#' @param sv_visitnum_var Character string, visit number variable in visit dataset (default: "VISITNUM")
#' @param sv_date_var Character string, visit date variable in visit dataset (default: "SVDAT")
#' @param eot_dataset Character string, end of treatment dataset name (default: "EOT").
#'   If dataset doesn't exist, EOT-related planned dates will be NA
#' @param eot_date_var Character string, end of treatment date variable (default: "EOTDAT")
#' @param ds_dataset Character string, disposition dataset name (default: "DS").
#'   If dataset doesn't exist, EOS-related planned dates will be NA
#' @param ds_date_var Character string, end of study date variable (default: "DSDAT")
#' @param tb_name_var Character string specifying the variable name to use as TBNAME in the output (default: NULL).
#'   If NULL or the column does not exist in the visit dataset, the TBNAME column in the output will be empty.
#' @param cycle_days Numeric, treatment cycle length in days (default: 28).
#'   Used to calculate subsequent cycle D1 planned dates
#'
#' @return Data frame with the following columns:
#'   \describe{
#'     \item{SUBJID}{Character. Subject ID}
#'     \item{VISIT}{Character. Visit name}
#'     \item{VISITNUM}{Numeric. Visit number}
#'     \item{visittype}{Character. Visit type (cycle information)}
#'     \item{visitday}{Character. Visit day}
#'     \item{visit_category}{Character. Visit category (screening, treatment, end_of_treatment, follow_up, or unknown)}
#'     \item{planned_date}{Date. Planned visit date}
#'     \item{wp_start}{Date. Visit window start date}
#'     \item{wp_end}{Date. Visit window end date}
#'     \item{wp_type}{Character. Window type (+/-, +, -, etc.)}
#'     \item{wp_value}{Numeric. Window value in days}
#'     \item{actual_date}{Date. Actual visit date (NA if not available)}
#'     \item{status}{Character. Visit status ("completed" or "missing")}
#'     \item{first_dose_date}{Date. Subject's first dose date}
#'     \item{last_dose_date}{Date. Subject's last dose date}
#'     \item{eot_date}{Date. Subject's end of treatment date}
#'     \item{eos_date}{Date. Subject's end of study date}
#'     \item{TBNAME}{Character. Table name from the variable specified by \code{tb_name_var}, empty if \code{tb_name_var} is NULL}
#'   }
#'
#' @examples
#' \dontrun{
#' # Prepare visit schedule data
#' visit_schedule <- data.frame(
#'   VISIT = c("Screening", "C1D1", "C1D8", "C2D1", "EOT"),
#'   VISITNUM = c(0, 1, 2, 3, 99),
#'   WP = c("", "+/-3d", "+/-2d", "+/-3d", "+7d"),
#'   CYCLE = c("Screening", "Cycle 1", "Cycle 1", "Cycle 2", "End of Treatment"),
#'   VISITDAY = c("0", "1", "8", "1", "EOT"),
#'   type = c(NA, "+/-", "+/-", "+/-", "+"),
#'   wpvalue = c(NA, 3, 2, 3, 7),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Prepare clinical trial data
#' data <- list(
#'   EX = data.frame(
#'     SUBJID = c("001", "001", "002"),
#'     EXSTDAT = c("2024-01-01", "2024-01-29", "2024-01-05"),
#'     stringsAsFactors = FALSE
#'   ),
#'   SV = data.frame(
#'     SUBJID = c("001", "001", "002"),
#'     VISIT = c("C1D1", "C1D8", "C1D1"),
#'     VISITNUM = c(1, 2, 1),
#'     SVDAT = c("2024-01-01", "2024-01-08", "2024-01-05"),
#'     stringsAsFactors = FALSE
#'   ),
#'   EOT = data.frame(
#'     SUBJID = c("001", "002"),
#'     EOTDAT = c("2024-03-15", "2024-03-20"),
#'     stringsAsFactors = FALSE
#'   ),
#'   DS = data.frame(
#'     SUBJID = c("001", "002"),
#'     DSDAT = c("2024-04-15", "2024-04-20"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#'
#' # Generate planned visit dates (using default visitcode variable)
#' visitcode <- visit_schedule
#' result <- generate_planned_visit_dates(data = data)
#'
#' # Or explicitly pass visitcode
#' result <- generate_planned_visit_dates(
#'   data = data,
#'   visitcode = visit_schedule
#' )
#'
#' # With custom cycle days (21-day cycle)
#' result <- generate_planned_visit_dates(
#'   data = data,
#'   visitcode = visit_schedule,
#'   cycle_days = 21
#' )
#'
#' # With multiple exposure datasets
#' result <- generate_planned_visit_dates(
#'   data = data,
#'   visitcode = visit_schedule,
#'   ex_datasets = c("EX1", "EX2"),
#'   ex_date_var = c("EXSTDAT1", "EXSTDAT2")
#' )
#' }
#'
#' @seealso
#' \code{\link{read_visitcode_file}} for reading visit schedule files
#'
#' \code{\link{check_visit_window}} for checking visit window deviations
#'
#' \code{\link{check_missing_visit}} for checking missing visits
#'
#' \code{\link{get_first_dose_date}} for extracting first dose dates
#'
#' \code{\link{get_last_dose_date}} for extracting last dose dates
#'
#' @family visit planning
#'
#' @importFrom dplyr filter select mutate arrange group_by ungroup left_join
#' @importFrom dplyr case_when if_else sym distinct bind_rows slice
#' @importFrom purrr map_chr map2_lgl map_dfr
#' @importFrom magrittr %>%
#' @export
generate_planned_visit_dates <- function(data,
                                         visitcode = NULL,
                                         ex_datasets = getOption("pdchecker.ex_datasets", "EX"),
                                         ex_date_var = getOption("pdchecker.ex_date_var", "EXSTDAT"),
                                         ex_end_date_var = getOption("pdchecker.ex_end_date_var", NULL),
                                         sv_dataset = getOption("pdchecker.sv_dataset", "SV"),
                                         sv_visit_var = getOption("pdchecker.sv_visit_var", "VISIT"),
                                         sv_visitnum_var = getOption("pdchecker.sv_visitnum_var", "VISITNUM"),
                                         sv_date_var = getOption("pdchecker.sv_date_var", "SVDAT"),
                                         eot_dataset = getOption("pdchecker.eot_dataset", "EOT"),
                                         eot_date_var = getOption("pdchecker.eot_date_var", "EOTDAT"),
                                         ds_dataset = getOption("pdchecker.ds_dataset", "DS"),
                                         ds_date_var = getOption("pdchecker.ds_date_var", "DSDAT"),
                                         tb_name_var = getOption("pdchecker.tb_name_var", NULL),
                                         cycle_days = 28) {
  # ============================================================================
  # Parameter validation
  # ============================================================================

  # Validate data parameter
  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  # Validate visitcode parameter
  # If NULL, try to get from caller's environment
  if (is.null(visitcode)) {
    if (exists("visitcode", envir = parent.frame(), inherits = TRUE)) {
      visitcode <- get("visitcode", envir = parent.frame(), inherits = TRUE)
    } else {
      stop(
        "'visitcode' is required. Use read_visitcode_file() to create it, ",
        "or pass it explicitly via the visitcode parameter."
      )
    }
  }
  if (!is.data.frame(visitcode)) {
    stop("'visitcode' must be a data frame")
  }

  # Validate visitcode required columns
  required_visit_cols <- c("VISIT", "VISITNUM", "WP", "CYCLE", "VISITDAY", "type", "wpvalue")
  missing_visit_cols <- setdiff(required_visit_cols, names(visitcode))
  if (length(missing_visit_cols) > 0) {
    stop("visitcode is missing required columns: ", paste(missing_visit_cols, collapse = ", "))
  }

  # Validate character string parameters
  char_params <- list(
    sv_dataset = sv_dataset,
    sv_visit_var = sv_visit_var,
    sv_visitnum_var = sv_visitnum_var,
    sv_date_var = sv_date_var,
    eot_dataset = eot_dataset,
    eot_date_var = eot_date_var,
    ds_dataset = ds_dataset,
    ds_date_var = ds_date_var
  )

  for (param_name in names(char_params)) {
    param_value <- char_params[[param_name]]
    if (!is.character(param_value) || length(param_value) != 1 || nchar(param_value) == 0) {
      stop("'", param_name, "' must be a non-empty character string")
    }
  }

  # Validate ex_datasets parameter
  if (!is.character(ex_datasets) || length(ex_datasets) == 0) {
    stop("'ex_datasets' must be a non-empty character vector")
  }

  # Validate ex_date_var parameter
  if (!is.character(ex_date_var) || length(ex_date_var) == 0) {
    stop("'ex_date_var' must be a non-empty character vector")
  }

  # Validate ex_date_var length matches ex_datasets
  if (length(ex_date_var) != 1 && length(ex_date_var) != length(ex_datasets)) {
    stop(
      "'ex_date_var' length must be 1 or equal to 'ex_datasets' length. ",
      "ex_datasets length: ", length(ex_datasets),
      ", ex_date_var length: ", length(ex_date_var)
    )
  }

  # Validate ex_end_date_var parameter (if provided)
  if (!is.null(ex_end_date_var) && length(ex_end_date_var) > 0) {
    if (!is.character(ex_end_date_var)) {
      stop("'ex_end_date_var' must be a character vector or NULL")
    }
    if (length(ex_end_date_var) != 1 && length(ex_end_date_var) != length(ex_datasets)) {
      stop(
        "'ex_end_date_var' length must be 1 or equal to 'ex_datasets' length. ",
        "ex_datasets length: ", length(ex_datasets),
        ", ex_end_date_var length: ", length(ex_end_date_var)
      )
    }
  }

  # Validate tb_name_var parameter
  if (!is.null(tb_name_var) && (!is.character(tb_name_var) || length(tb_name_var) != 1)) {
    stop("'tb_name_var' must be NULL or a single character string")
  }

  # Validate sv_dataset exists
  if (!sv_dataset %in% names(data)) {
    stop("Missing visit dataset: ", sv_dataset)
  }

  # Validate visit dataset required columns
  sv_data <- data[[sv_dataset]]
  sv_required_cols <- c("SUBJID", sv_visit_var, sv_date_var)
  missing_sv_cols <- setdiff(sv_required_cols, names(sv_data))
  if (length(missing_sv_cols) > 0) {
    stop("Visit dataset '", sv_dataset, "' is missing required columns: ", paste(missing_sv_cols, collapse = ", "))
  }

  # Validate cycle_days parameter
  if (!is.numeric(cycle_days) || length(cycle_days) != 1 || is.na(cycle_days) || cycle_days <= 0) {
    stop("'cycle_days' must be a positive number")
  }

  # ============================================================================
  # Get date data
  # ============================================================================

  # First dose date
  first_dose_dates <- get_first_dose_date(
    data = data,
    ex_datasets = ex_datasets,
    ex_date_var = ex_date_var
  )

  # Last dose date
  last_dose_dates <- get_last_dose_date(
    data = data,
    ex_datasets = ex_datasets,
    ex_date_var = ex_date_var,
    ex_end_date_var = ex_end_date_var
  )

  # End of treatment date
  eot_dates <- get_eot_date(
    data = data,
    eot_dataset = eot_dataset,
    eot_date_var = eot_date_var
  )

  # End of study date
  eos_dates <- get_eos_date(
    data = data,
    ds_dataset = ds_dataset,
    ds_date_var = ds_date_var
  )

  # ============================================================================
  # Prepare visit information data frame
  # ============================================================================

  # Use existing visit_category if available (from read_visitcode_file),
  # otherwise generate it from CYCLE column
  if (!"visit_category" %in% names(visitcode)) {
    visitcode$visit_category <- map_chr(visitcode$CYCLE, match_visit_type)
  }

  visit_info <- visitcode %>%
    mutate(
      visit = VISIT,
      visitnum = VISITNUM,
      visitday = VISITDAY,
      visittype = CYCLE,
      wp = WP,
      wp_type = type,
      wp_value = as.numeric(wpvalue),
      is_d1 = map2_lgl(VISIT, VISITDAY, is_d1_visit)
    ) %>%
    arrange(seq_len(nrow(visitcode)))

  # ============================================================================
  # Get actual visit data
  # ============================================================================

  actual_visits <- sv_data %>%
    filter(!is_sas_na(!!sym(sv_date_var))) %>%
    mutate(
      actual_date = as.Date(!!sym(sv_date_var)),
      VISIT = !!sym(sv_visit_var),
      VISITNUM = !!sym(sv_visitnum_var)
    ) %>%
    select(SUBJID, VISIT, VISITNUM, actual_date) %>%
    arrange(SUBJID, actual_date)

  # ============================================================================
  # Merge subject dates
  # ============================================================================

  subjects <- unique(sv_data$SUBJID)

  subject_dates <- data.frame(SUBJID = subjects, stringsAsFactors = FALSE) %>%
    left_join(first_dose_dates, by = "SUBJID") %>%
    left_join(last_dose_dates, by = "SUBJID") %>%
    left_join(eot_dates, by = "SUBJID") %>%
    left_join(eos_dates, by = "SUBJID")

  # ============================================================================
  # Calculate planned visit dates for each subject
  # ============================================================================

  planned_dates <- map_dfr(subjects, function(subj_id) {
    calc_planned_dates(
      subj_id = subj_id,
      visit_info = visit_info,
      actual_visits = actual_visits,
      subject_dates = subject_dates,
      cycle_days = cycle_days
    )
  })

  # Add TBNAME from visit dataset
  has_tb_name <- !is.null(tb_name_var) && tb_name_var %in% names(sv_data)

  if (nrow(planned_dates) > 0) {
    if (has_tb_name) {
      tb_values <- sv_data[[tb_name_var]]
      tb_values <- tb_values[!is.na(tb_values) & tb_values != ""]
      tbname_value <- if (length(tb_values) > 0) as.character(tb_values[1]) else ""
      planned_dates <- planned_dates %>% mutate(TBNAME = tbname_value)
    } else {
      planned_dates <- planned_dates %>% mutate(TBNAME = "")
    }

    result <- planned_dates %>%
      arrange(SUBJID, actual_date)
    return(result)
  } else {
    return(data.frame())
  }
}
