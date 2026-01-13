#' Check Missing Visits
#'
#' @description
#' Check for missing visits based on planned visit dates. Receives planned visit data
#' from \code{\link{generate_planned_visit_dates}} and uses different cutoff date
#' criteria based on visit type to determine if a visit should have been completed.
#'
#' @details
#' ## Usage Workflow
#'
#' This function is designed to work with \code{generate_planned_visit_dates}:
#'
#' ```r
#' # Step 1: Generate planned visit dates
#' planned_dates <- generate_planned_visit_dates(
#'   data = study_data,
#'   visit_schedule_data = visit_codes
#' )
#'
#' # Step 2: Check missing visits
#' result <- check_missing_visit(
#'   planned_dates = planned_dates,
#'   cutoffdt = as.Date("2024-12-31")
#' )
#' ```
#'
#' ## Visit Completion Criteria
#'
#' Different visit types use different termination date criteria:
#'
#' ### Screening and Treatment Visits
#' Planned date must be < min(end of treatment date, end of study date, cutoff date)
#'
#' ### End of Treatment and Follow-up Visits
#' Planned date must be <= min(end of study date, cutoff date)
#'
#' ## Visit Type Classification
#'
#' Visit types are automatically identified by \code{generate_planned_visit_dates}:
#' - screening: Screening visits
#' - treatment: Treatment visits
#' - end_of_treatment: End of treatment visits
#' - follow_up: Follow-up visits
#'
#' @param planned_dates Data frame from \code{\link{generate_planned_visit_dates}}.
#'   Must contain: SUBJID, VISIT, VISITNUM, visittype, planned_date, status, eot_date, eos_date
#' @param cutoffdt Date, data cutoff date (default: current date). Used to determine if visit should be completed
#' @param pdno Character string specifying the protocol deviation number for this check (default: "8.2.1")
#'
#' @return List with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if there are missing visits}
#'     \item{messages}{Character vector. Deviation description messages}
#'     \item{details}{Data frame. Missing visit details with columns:
#'       \itemize{
#'         \item PDNO: Protocol deviation number specified by \code{pdno} parameter
#'         \item SUBJID: Subject ID
#'         \item first_dose_date: Subject's first dose date
#'         \item VISIT: Missing visit name
#'         \item VISITNUM: Visit number
#'         \item planned_date: Planned visit date
#'         \item visittype: Visit type
#'         \item eot_date: Subject's end of treatment date
#'         \item eos_date: Subject's end of study date
#'         \item cutoffdt: Data cutoff date
#'         \item valid_visits_count: Total visits that should be completed
#'         \item completed_visits_count: Actual completed visits
#'         \item DESCRIPTION: Description of the deviation for each record
#'       }
#'     }
#'     \item{planned_visits}{Data frame. Complete input planned visit information}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load data and visit schedule
#' data <- read_raw_data("path/to/data")
#' visit_codes <- read_visitcode_file("path/to/visitcode.xlsx")
#'
#' # Generate planned visit dates
#' planned_dates <- generate_planned_visit_dates(
#'   data = data,
#'   visit_schedule_data = visit_codes
#' )
#'
#' # Check missing visits with default cutoff (current date)
#' result <- check_missing_visit(planned_dates)
#' print(result)
#'
#' # Check missing visits with specific cutoff date
#' result <- check_missing_visit(
#'   planned_dates = planned_dates,
#'   cutoffdt = as.Date("2024-06-30")
#' )
#'
#' # Access deviation details
#' if (result$has_deviation) {
#'   missing_details <- result$details
#' }
#' }
#'
#' @seealso
#' \code{\link{generate_planned_visit_dates}} for generating planned visit data
#' \code{\link{check_visit_window}} for checking visit window deviations
#'
#' @family visit checks
#'
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom purrr map_chr map_dfr
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
check_missing_visit <- function(planned_dates,
                                cutoffdt = Sys.Date(),
                                pdno = "8.2.1") {
  # Validate input parameters
  if (!is.data.frame(planned_dates)) {
    stop("'planned_dates' must be a data frame")
  }

  if (!is.character(pdno) || length(pdno) != 1) {
    stop("'pdno' must be a single character string")
  }

  # Check required columns
  required_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "visittype", "planned_date",
    "status", "eot_date", "eos_date"
  )

  missing_cols <- setdiff(required_cols, names(planned_dates))
  if (length(missing_cols) > 0) {
    stop("'planned_dates' is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame(),
    planned_visits = planned_dates
  )

  # Ensure cutoff date is Date type
  if (!inherits(cutoffdt, "Date")) {
    cutoffdt <- as.Date(cutoffdt)
  }

  # Check missing visits for each subject
  subjects <- unique(planned_dates$SUBJID)

  subject_visits <- map_dfr(subjects, function(subj_id) {
    # Get planned visits for this subject
    subj_planned <- filter(planned_dates, .data$SUBJID == subj_id)

    # Calculate different termination dates for different visit types
    # Screening and treatment: min(eotdate, eosdate, cutoffdt)
    # End of treatment, withdrawal, follow-up: min(eosdate, cutoffdt)

    # Get dates for this subject
    first_dose_date <- unique(subj_planned$first_dose_date)[1]
    eot_date <- unique(subj_planned$eot_date)[1]
    eos_date <- unique(subj_planned$eos_date)[1]

    # Calculate termination date for screening and treatment visits
    trtmax_dates <- c(cutoffdt)
    if (!is.na(eot_date)) {
      trtmax_dates <- c(trtmax_dates, eot_date)
    }
    if (!is.na(eos_date)) {
      trtmax_dates <- c(trtmax_dates, eos_date)
    }
    cutoffdt_trt <- min(trtmax_dates, na.rm = TRUE)

    # Calculate termination date for follow-up visits
    followup_dates <- c(cutoffdt)
    if (!is.na(eos_date)) {
      followup_dates <- c(followup_dates, eos_date)
    }
    cutoffdt_fu <- min(followup_dates, na.rm = TRUE)

    # Filter visits that should be checked based on visit type
    # Determine the cutoff date for each visit category
    valid_planned_visits <- subj_planned %>%
      filter(!is.na(.data$planned_date)) %>%
      mutate(
        should_check = case_when(
          # Screening: planned date must be < cutoffdt_trt
          .data$visit_category == "screening" ~
            .data$planned_date < cutoffdt_trt,
          # Treatment: subject must have first dose, and planned date must be < cutoffdt_trt
          .data$visit_category == "treatment" ~
            !is.na(first_dose_date) & .data$planned_date < cutoffdt_trt,
          # End of treatment: planned date must be <= cutoffdt_fu
          .data$visit_category == "end_of_treatment" ~
            .data$planned_date <= cutoffdt_fu,
          # End of study: planned date must be <= cutoffdt
          .data$visit_category == "end_of_study" ~
            .data$planned_date <= cutoffdt_fu,
          # Follow-up: planned date must be <= cutoffdt_fu
          .data$visit_category == "follow_up" ~
            .data$planned_date <= cutoffdt_fu,
          # Other categories: do not check
          TRUE ~ FALSE
        )
      ) %>%
      filter(.data$should_check) %>%
      select(-"should_check")

    # Find missing visits (should be completed but not completed)
    missing_visit_subset <- filter(valid_planned_visits, .data$status == "missing")

    # Build result - report visits that should be completed but were not
    if (nrow(missing_visit_subset) > 0) {
      # Calculate completed visits count (only within valid planned visits)
      completed_subset <- filter(valid_planned_visits, .data$status == "completed")
      completed_count <- nrow(completed_subset)

      # Create a record for each missing visit
      return(map_dfr(seq_len(nrow(missing_visit_subset)), function(i) {
        visit_name <- missing_visit_subset$VISIT[i]
        visit_planned_date <- missing_visit_subset$planned_date[i]
        first_dose_str <- if (!is.na(first_dose_date)) {
          as.character(first_dose_date)
        } else {
          "未记录"
        }
        visit_info <- sprintf("%s(%s)", visit_name, visit_planned_date)

        data.frame(
          PDNO = pdno,
          SUBJID = subj_id,
          first_dose_date = first_dose_date,
          VISIT = visit_name,
          VISITNUM = missing_visit_subset$VISITNUM[i],
          planned_date = visit_planned_date,
          visittype = missing_visit_subset$visittype[i],
          eot_date = eot_date,
          eos_date = eos_date,
          cutoffdt = cutoffdt,
          valid_visits_count = nrow(valid_planned_visits),
          completed_visits_count = completed_count,
          DESCRIPTION = sprintf(
            "受试者编号%s，首次用药时间为%s，计划进行的%s访视遗漏。",
            subj_id,
            first_dose_str,
            visit_info
          ),
          stringsAsFactors = FALSE
        )
      }))
    } else {
      return(data.frame())
    }
  })

  # Get deviations (subjects with missing visits)
  if (nrow(subject_visits) > 0) {
    deviations <- filter(subject_visits, !is.na(.data$VISIT))
  } else {
    deviations <- data.frame()
  }

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "基于计划访视日期检查，未按计划进行访视"
    results$details <- deviations
  }

  class(results) <- c("missing_visit_check", "list")
  return(results)
}


#' Print method for missing visit check results
#' @param x Object of class missing_visit_check
#' @param ... Additional arguments
#' @export
print.missing_visit_check <- function(x, ...) {
  pdno_display <- if (nrow(x$details) > 0) x$details$PDNO[1] else "8.2.1"
  cat(sprintf("%s 按计划进行现场访视\n", pdno_display))
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
