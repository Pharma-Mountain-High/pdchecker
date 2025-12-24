#' Calculate Planned Visit Dates for a Single Subject
#'
#' @description
#' Internal function to calculate planned visit dates for a single subject.
#' This is the main orchestration function that coordinates all visit date calculations.
#'
#' @details
#' ## Processing Flow
#'
#' 1. **First pass**: Process all D1 visits to establish cycle reference dates
#' 2. **Second pass**: Calculate planned dates for all visits based on their category
#' 3. **Final step**: Determine visit status (completed/missing) and calculate window ranges
#'
#' @param subj_id Character, subject ID
#' @param visit_info Data frame, processed visit schedule information
#' @param actual_visits Data frame, actual visit records
#' @param subject_dates Data frame, subject-level date information
#' @param cycle_days Numeric, treatment cycle length in days
#'
#' @return Data frame with planned visit dates for the subject
#'
#' @importFrom dplyr filter arrange
#' @importFrom magrittr %>%
#'
#' @keywords internal
#' @noRd
calc_planned_dates <- function(subj_id,
                                            visit_info,
                                            actual_visits,
                                            subject_dates,
                                            cycle_days) {
  # Get actual visit records for this subject

  subj_actual <- actual_visits %>%
    filter(SUBJID == subj_id) %>%
    arrange(actual_date)

  # Get relevant dates for this subject
  subj_dates <- subject_dates %>%
    filter(SUBJID == subj_id)

  first_dose_date <- subj_dates$first_dose_date[1]
  last_dose_date <- subj_dates$last_dose_date[1]
  eot_date <- subj_dates$eot_date[1]
  eos_date <- subj_dates$eos_date[1]

  # Calculate planned dates for each visit
  visit_results <- list()
  cycle_d1_dates <- list() # Store planned D1 dates for each cycle
  cycle_d1_actual_dates <- list() # Store actual D1 dates for each cycle
  last_cycle_d1_date <- NULL

  # ============================================================================
  # First process all D1 visits
  # ============================================================================

  d1_visits <- visit_info[visit_info$is_d1, ]
  if (nrow(d1_visits) > 0) {
    for (i in seq_len(nrow(d1_visits))) {
      visit_row <- d1_visits[i, ]
      visit_name <- visit_row$visit
      visit_type <- visit_row$visittype
      visit_day <- visit_row$visitday
      visit_category <- visit_row$visit_category

      if (visit_category == "treatment" && !is.na(visit_day) && is.numeric(as.numeric(visit_day))) {
        # Extract cycle information
        cycle_match <- regmatches(visit_type, regexpr("\\d+", visit_type))
        current_cycle <- if (length(cycle_match) > 0) as.numeric(cycle_match[1]) else 1

        # Check for actual visit record
        actual_d1_visit <- subj_actual[subj_actual$VISIT == visit_name, ]
        actual_d1_date <- if (nrow(actual_d1_visit) > 0) actual_d1_visit$actual_date[1] else as.Date(NA)

        # Calculate planned date
        planned_date <- calculate_d1_planned_date(
          current_cycle = current_cycle,
          actual_d1_date = actual_d1_date,
          first_dose_date = first_dose_date,
          cycle_d1_actual_dates = cycle_d1_actual_dates,
          cycle_d1_dates = cycle_d1_dates,
          last_cycle_d1_date = last_cycle_d1_date,
          cycle_days = cycle_days
        )

        # Store current cycle D1 planned and actual dates
        cycle_key <- paste0("C", current_cycle, "D1")
        cycle_d1_dates[[cycle_key]] <- planned_date
        if (!is.na(actual_d1_date)) {
          cycle_d1_actual_dates[[cycle_key]] <- actual_d1_date
        }
        last_cycle_d1_date <- planned_date
      }
    }
  }

  # ============================================================================
  # Then process all visits
  # ============================================================================

  for (i in seq_len(nrow(visit_info))) {
    visit_row <- visit_info[i, ]
    visit_name <- visit_row$visit
    visit_num <- visit_row$visitnum
    visit_type <- visit_row$visittype
    visit_day <- visit_row$visitday
    visit_category <- visit_row$visit_category
    is_d1 <- visit_row$is_d1

    # Calculate planned date
    planned_date <- calculate_visit_planned_date(
      visit_category = visit_category,
      visit_day = visit_day,
      visit_type = visit_type,
      is_d1 = is_d1,
      first_dose_date = first_dose_date,
      last_dose_date = last_dose_date,
      eot_date = eot_date,
      eos_date = eos_date,
      cycle_d1_dates = cycle_d1_dates,
      cycle_d1_actual_dates = cycle_d1_actual_dates
    )

    # Check actual visit status
    actual_visit <- subj_actual[subj_actual$VISITNUM == visit_num, ]

    if (nrow(actual_visit) > 0) {
      visit_status <- "completed"
      actual_date <- actual_visit$actual_date[1]
    } else {
      visit_status <- "missing"
      actual_date <- as.Date(NA)
    }

    # Calculate window start and end dates
    window_result <- calculate_window_range(
      planned_date = planned_date,
      window_type = visit_row$wp_type,
      window_value = visit_row$wp_value
    )

    visit_results[[length(visit_results) + 1]] <- data.frame(
      SUBJID = subj_id,
      VISIT = visit_name,
      VISITNUM = visit_num,
      visittype = visit_type,
      visitday = visit_day,
      planned_date = planned_date,
      wp_start = window_result$window_start,
      wp_end = window_result$window_end,
      wp_type = visit_row$wp_type,
      wp_value = visit_row$wp_value,
      actual_date = actual_date,
      status = visit_status,
      first_dose_date = first_dose_date,
      last_dose_date = last_dose_date,
      eot_date = eot_date,
      eos_date = eos_date,
      stringsAsFactors = FALSE
    )
  }

  # Combine all visit results
  if (length(visit_results) > 0) {
    return(do.call(rbind, visit_results))
  } else {
    return(data.frame())
  }
}

#' Calculate D1 Visit Planned Date
#'
#' @description
#' Internal function to calculate planned date for D1 (Day 1) visits.
#'
#' @details
#' ## Calculation Rules
#'
#' ### C1D1 (Cycle 1 Day 1)
#' - If actual visit exists: Planned date = Actual visit date
#' - If no actual visit: Planned date = First dose date
#'
#' ### CnD1 (Subsequent cycles, n > 1)
#' - Planned date = Previous cycle D1 reference date + cycle_days
#' - Reference date selection priority:
#'   1. Previous cycle D1 **actual visit date** (if exists)
#'   2. Previous cycle D1 **planned date**
#'   3. Last known cycle D1 date
#'   4. Fallback: first_dose_date + (n-1) * cycle_days
#'
#' ### Key Features
#' - C2D1, C3D1 planned dates are calculated iteratively
#' - Each cycle's D1 does NOT use its own actual date for planned calculation
#' - If C2D1 has an actual visit, that date affects C3D1 planned date
#'
#' ### Example
#' ```
#' First dose date: 2024-01-01, cycle_days: 28
#' C1D1 planned: 2024-01-01 (no actual visit)
#' C2D1 planned: 2024-01-01 + 28 = 2024-01-29
#' C2D1 actual: 2024-02-02 (delayed visit)
#' C3D1 planned: 2024-02-02 + 28 = 2024-03-01 (based on C2D1 actual)
#' ```
#'
#' @param current_cycle Numeric, current cycle number
#' @param actual_d1_date Date, actual D1 visit date (if exists)
#' @param first_dose_date Date, first dose date
#' @param cycle_d1_actual_dates List, actual D1 dates by cycle
#' @param cycle_d1_dates List, planned D1 dates by cycle
#' @param last_cycle_d1_date Date, last known cycle D1 date
#' @param cycle_days Numeric, treatment cycle length in days
#'
#' @return Date, planned date for the D1 visit
#'
#' @keywords internal
#' @noRd
calculate_d1_planned_date <- function(current_cycle,
                                      actual_d1_date,
                                      first_dose_date,
                                      cycle_d1_actual_dates,
                                      cycle_d1_dates,
                                      last_cycle_d1_date,
                                      cycle_days) {
  planned_date <- NA

  if (current_cycle == 1) {
    # C1D1: Use actual date if available; otherwise use first dose date
    if (!is.na(actual_d1_date)) {
      planned_date <- actual_d1_date
    } else {
      planned_date <- first_dose_date
    }
  } else {
    # CnD1 (n>1): Based on previous cycle D1 date + cycle days
    prev_cycle_key <- paste0("C", current_cycle - 1, "D1")

    # Select previous cycle D1 base date: actual date (if available) > planned date
    prev_d1_base_date <- if (!is.null(cycle_d1_actual_dates[[prev_cycle_key]])) {
      cycle_d1_actual_dates[[prev_cycle_key]]
    } else if (!is.null(cycle_d1_dates[[prev_cycle_key]])) {
      cycle_d1_dates[[prev_cycle_key]]
    } else if (!is.null(last_cycle_d1_date)) {
      last_cycle_d1_date
    } else {
      NULL
    }

    if (!is.null(prev_d1_base_date) && !is.na(prev_d1_base_date)) {
      planned_date <- prev_d1_base_date + cycle_days
    } else {
      # Fallback: if previous cycle not found, use first dose date
      planned_date <- first_dose_date + (current_cycle - 1) * cycle_days
    }
  }

  return(planned_date)
}

#' Calculate Visit Planned Date Based on Visit Category
#'
#' @description
#' Internal function to calculate planned date based on visit category.
#' Routes to appropriate calculation logic based on visit type.
#'
#' @details
#' ## Visit Category Routing
#'
#' | Category | Calculation Logic |
#' |----------|-------------------|
#' | screening | Planned date = first_dose_date |
#' | treatment | See treatment visit rules below |
#' | end_of_treatment | Routes to calculate_eot_visit_date() |
#' | follow_up | Routes to calculate_followup_visit_date() |
#'
#' ## Treatment Visit Rules
#'
#' ### D1 Visits (visit day = 1)
#' - Uses pre-calculated D1 planned dates from cycle_d1_dates
#' - See calculate_d1_planned_date() for calculation details
#'
#' ### Non-D1 Visits (e.g., D8, D15, D21)
#' - Planned date = Current cycle D1 reference date + (visit_day - 1)
#' - Reference date selection priority:
#'   1. Current cycle D1 **actual visit date** (if exists)
#'   2. Current cycle D1 **planned date**
#'
#' ### Example (Non-D1)
#' ```
#' C2D1 planned: 2024-01-29
#' C2D1 actual: 2024-02-02
#'
#' C2D8 planned: 2024-02-02 + 7 = 2024-02-09 (based on D1 actual)
#' C2D15 planned: 2024-02-02 + 14 = 2024-02-16 (based on D1 actual)
#' ```
#'
#' @param visit_category Character, visit category (screening, treatment, etc.)
#' @param visit_day Character, visit day value
#' @param visit_type Character, visit type (cycle information)
#' @param is_d1 Logical, whether this is a D1 visit
#' @param first_dose_date Date, first dose date
#' @param last_dose_date Date, last dose date
#' @param eot_date Date, end of treatment date
#' @param eos_date Date, end of study date
#' @param cycle_d1_dates List, planned D1 dates by cycle
#' @param cycle_d1_actual_dates List, actual D1 dates by cycle
#'
#' @return Date, planned date for the visit
#'
#' @keywords internal
#' @noRd
calculate_visit_planned_date <- function(visit_category,
                                         visit_day,
                                         visit_type,
                                         is_d1,
                                         first_dose_date,
                                         last_dose_date,
                                         eot_date,
                                         eos_date,
                                         cycle_d1_dates,
                                         cycle_d1_actual_dates) {
  planned_date <- NA

  if (visit_category == "screening") {
    # Screening visit: planned date = first dose date
    planned_date <- first_dose_date
  } else if (visit_category == "treatment") {
    # Treatment visit
    if (!is.na(visit_day) && is.numeric(as.numeric(visit_day))) {
      current_day <- as.numeric(visit_day)

      # Extract cycle information
      cycle_match <- regmatches(visit_type, regexpr("\\d+", visit_type))
      current_cycle <- if (length(cycle_match) > 0) as.numeric(cycle_match[1]) else 1

      if (is_d1) {
        # D1 visit planned date was calculated in preprocessing step
        cycle_key <- paste0("C", current_cycle, "D1")
        planned_date <- cycle_d1_dates[[cycle_key]]
      } else {
        # Non-D1 visit, calculate based on current cycle D1 date
        cycle_key <- paste0("C", current_cycle, "D1")

        # Prefer actual D1 visit date (if available); otherwise use planned date
        base_d1_date <- if (!is.null(cycle_d1_actual_dates[[cycle_key]])) {
          cycle_d1_actual_dates[[cycle_key]]
        } else {
          cycle_d1_dates[[cycle_key]]
        }

        if (!is.null(base_d1_date) && !is.na(base_d1_date)) {
          # Planned date = D1 date (actual or planned) + (visit day - 1)
          planned_date <- base_d1_date + (current_day - 1)
        } else {
          planned_date <- as.Date(NA)
        }
      }
    }
  } else if (visit_category == "end_of_treatment") {
    # End of treatment visit
    planned_date <- calculate_eot_visit_date(visit_day, eot_date, eos_date)
  } else if (visit_category == "follow_up") {
    # Follow-up visit
    planned_date <- calculate_followup_visit_date(visit_day, eot_date, last_dose_date)
  }

  return(planned_date)
}

#' Calculate End of Treatment Visit Planned Date
#'
#' @description
#' Internal function to calculate planned date for end of treatment visits.
#'
#' @details
#' ## Supported Formats
#'
#' | Format | Calculation | Example |
#' |--------|-------------|---------|
#' | EOT | eot_date | eot_date = 2024-03-15 -> 2024-03-15 |
#' | EOT+N | eot_date + N days | EOT+7 = 2024-03-15 + 7 = 2024-03-22 |
#' | EOS | eos_date | eos_date = 2024-04-15 -> 2024-04-15 |
#'
#' ## Notes
#' - Returns NA if required date (eot_date or eos_date) is missing
#' - EOT+N format extracts numeric value after "EOT+"
#'
#' @param visit_day Character, visit day value (e.g., "EOT", "EOT+7", "EOS")
#' @param eot_date Date, end of treatment date
#' @param eos_date Date, end of study date
#'
#' @return Date, planned date for the EOT visit
#'
#' @keywords internal
#' @noRd
calculate_eot_visit_date <- function(visit_day, eot_date, eos_date) {
  planned_date <- NA

  if (visit_day == "EOT" && !is.na(eot_date)) {
    planned_date <- eot_date
  } else if (grepl("^EOT\\+", visit_day) && !is.na(eot_date)) {
    days_add <- as.numeric(gsub("^EOT\\+", "", visit_day))
    if (!is.na(days_add)) {
      planned_date <- eot_date + days_add
    }
  } else if (visit_day == "EOS" && !is.na(eos_date)) {
    planned_date <- eos_date
  }

  return(planned_date)
}

#' Calculate Follow-up Visit Planned Date
#'
#' @description
#' Internal function to calculate planned date for follow-up visits.
#'
#' @details
#' ## Supported Formats
#'
#' | Format | Calculation | Example |
#' |--------|-------------|---------|
#' | EOT+N | eot_date + N days | EOT+30 = 2024-03-15 + 30 = 2024-04-14 |
#' | LD+N | last_dose_date + N days | LD+30 = 2024-03-01 + 30 = 2024-03-31 |
#' | Last Dose+N | last_dose_date + N days | Last Dose+60 = 2024-03-01 + 60 = 2024-04-30 |
#'
#' ## Notes
#' - Returns NA if required date (eot_date or last_dose_date) is missing
#' - Both "LD+" and "Last Dose+" formats are supported for last dose calculation
#'
#' @param visit_day Character, visit day value (e.g., "EOT+30", "LD+30", "Last Dose+60")
#' @param eot_date Date, end of treatment date
#' @param last_dose_date Date, last dose date
#'
#' @return Date, planned date for the follow-up visit
#'
#' @keywords internal
#' @noRd
calculate_followup_visit_date <- function(visit_day, eot_date, last_dose_date) {
  planned_date <- NA

  if (grepl("^EOT\\+", visit_day) && !is.na(eot_date)) {
    # Based on EOT date
    days_add <- as.numeric(gsub("^EOT\\+", "", visit_day))
    if (!is.na(days_add)) {
      planned_date <- eot_date + days_add
    }
  } else if (grepl("^(Last Dose|LD)\\+", visit_day) && !is.na(last_dose_date)) {
    # Based on last dose date: supports "Last Dose+30" or "LD+30" format
    days_add <- as.numeric(gsub("^(Last Dose|LD)\\+", "", visit_day))
    if (!is.na(days_add)) {
      planned_date <- last_dose_date + days_add
    }
  }

  return(planned_date)
}
