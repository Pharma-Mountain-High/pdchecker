#' Combine date and optional time into POSIXct
#'
#' Missing / SAS-missing time defaults to 00:00:00. Uses UTC to avoid DST issues.
#'
#' @param date Date or coercible to Date
#' @param time Character vector of times (HH:MM or HH:MM:SS), or NULL
#' @return POSIXct vector
#' @noRd
combine_date_time <- function(date, time = NULL) {
  n <- length(date)
  date_chr <- as.character(as.Date(date))
  out <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  ok <- !is.na(date) & !is.na(date_chr) & date_chr != "NA"
  if (!any(ok)) {
    return(out)
  }

  if (is.null(time)) {
    time_chr <- rep("00:00:00", n)
  } else {
    time_chr <- as.character(time)
    missing_time <- is.na(time_chr) | is_sas_na(time_chr) |
      !nzchar(trimws(ifelse(is.na(time_chr), "", time_chr)))
    time_chr[missing_time] <- "00:00:00"
    time_chr <- trimws(time_chr)
    needs_sec <- grepl("^\\d{1,2}:\\d{2}$", time_chr)
    time_chr[needs_sec] <- paste0(time_chr[needs_sec], ":00")
  }

  parsed <- suppressWarnings(
    as.POSIXct(paste(date_chr[ok], time_chr[ok]), tz = "UTC")
  )
  out[ok] <- parsed
  out
}

#' Extract hour number from a window string like "≤24h" or "±12H"
#'
#' @param wp Character vector
#' @return Numeric hours (NA when unparseable)
#' @noRd
parse_hours_from_wp <- function(wp) {
  vapply(wp, function(x) {
    if (is.na(x) || !nzchar(as.character(x))) {
      return(NA_real_)
    }
    x <- trimws(as.character(x))
    x <- gsub("^±|^(≤|<=)|^(≥|>=)|^\\+|-", "", x, perl = TRUE)
    x <- gsub("小时|[hH]", "", x)
    as.numeric(trimws(x))
  }, numeric(1), USE.NAMES = FALSE)
}

#' Generate Test Window Dates
#'
#' @description
#' Resolve the anchor date and derive the test window range for each required
#' test at each visit. Receives test data prepared by
#' \code{\link{prepare_test_data}} with a test-window config (from
#' \code{\link{read_testwp_file}}), and adds \code{anchor_date},
#' \code{window_start}, \code{window_end} and \code{window_status} columns.
#' The counterpart of \code{\link{generate_planned_visit_dates}} for tests.
#'
#' @details
#' ## Usage Workflow
#'
#' ```r
#' # Step 1: Read test-window config
#' testwp <- read_testwp_file("test_wp.xlsx", sheet_name = "QL0911-302")
#'
#' # Step 2: Prepare test data with testwp as config
#' #         (window rule cols + rd_date + first_dose date/time + cyc_dose date/time)
#' prepared_lb <- prepare_test_data(
#'   data = data,   # should include RAND and EX when RD/FD/EX rules are used
#'   test_dataset = "LB",
#'   config = testwp,
#'   ex_time_var = "EXSTTIM",
#'   test_time_var = "LBTIM"
#' )
#'
#' # Step 3: Generate anchor dates and window ranges
#' testwp_dates <- generate_test_window_dates(prepared_lb)
#'
#' # Step 4: Check test window deviations
#' result <- check_test_window(testwp_dates)
#' ```
#'
#' ## Anchor Dates (ref column)
#'
#' Each rule's window is anchored to a reference date decided by \code{ref}:
#'
#' | ref | Anchor date | Source column on \code{data} |
#' |-----|-------------|------------------------------|
#' | SV | Actual visit date at this visit | SVDAT |
#' | RD | Randomization date | rd_date (from \code{\link{prepare_test_data}}) |
#' | FD | First dose date | first_dose_date (from \code{\link{prepare_test_data}}) |
#' | EX | Actual dosing date at this visit | cyc_dose_date (from \code{\link{prepare_test_data}}) |
#'
#' ## Window Calculation
#'
#' The window range is derived from the anchor with \code{type}/\code{wpvalue}:
#' - \code{0}: test must be performed on the anchor day (or exact datetime for hours)
#' - \code{±}: anchor - wpvalue to anchor + wpvalue
#' - \code{+} / \code{≥}: anchor to anchor + wpvalue
#' - \code{-} / \code{≤}: anchor - wpvalue to anchor
#'
#' When the rule uses hour units (\code{h}/\code{H}/\code{小时}, see \code{wp_unit}),
#' windows are computed on POSIXct datetimes (date + time; missing time = 00:00:00)
#' and stored in \code{anchor_datetime} / \code{window_start_dt} / \code{window_end_dt}.
#' Day-based rules keep Date \code{window_start} / \code{window_end}.
#'
#' ## Window Status
#'
#' All input rows are kept. Rows whose window cannot be derived have NA
#' window dates, with the reason recorded in \code{window_status}:
#'
#' | window_status | Meaning |
#' |---------------|---------|
#' | ok | Window derived successfully |
#' | no_rule | ref is NA (no window rule for this row) |
#' | no_anchor_data | Anchor date column missing for RD/FD/EX rules (with warning) |
#' | missing_anchor_date | Anchor date not found for this subject/visit |
#' | unsupported_rule | Window type cannot be derived (e.g. range/other) |
#'
#' @param data Data frame prepared by \code{\link{prepare_test_data}} using
#'   the output of \code{\link{read_testwp_file}} as config.
#'   Must contain: SUBJID, VISIT, VISITNUM, SVDAT, TESTCAT, TESTDAT,
#'   wp_rule, ref, type, wpvalue.
#'   Prefer also \code{rd_date}, \code{first_dose_date}/\code{first_dose_time},
#'   \code{cyc_dose_date}/\code{cyc_dose_time}, and \code{TESTTIM} for hour rules.
#'
#' @return Data frame with all columns from \code{data}, plus:
#'   \describe{
#'     \item{wp_unit}{\code{"h"} or \code{"d"} (inferred from \code{wp} if missing)}
#'     \item{anchor_date}{Date. Anchor date the window is based on}
#'     \item{anchor_datetime}{POSIXct. Anchor datetime (hour rules; NA for day rules)}
#'     \item{window_start}{Date. Window start date (calendar date of bound)}
#'     \item{window_end}{Date. Window end date (calendar date of bound)}
#'     \item{window_start_dt}{POSIXct. Window start datetime (hour rules only)}
#'     \item{window_end_dt}{POSIXct. Window end datetime (hour rules only)}
#'     \item{window_status}{Character. "ok" or the reason the window is NA}
#'   }
#'
#' @examples
#' \dontrun{
#' testwp <- read_testwp_file("test_wp.xlsx", sheet_name = "QL0911-302")
#' prepared_lb <- prepare_test_data(
#'   data = data, test_dataset = "LB", config = testwp,
#'   ex_time_var = "EXSTTIM", test_time_var = "LBTIM"
#' )
#'
#' testwp_dates <- generate_test_window_dates(prepared_lb)
#'
#' # Inspect derived windows before checking
#' table(testwp_dates$window_status)
#'
#' result <- check_test_window(testwp_dates)
#' }
#'
#' @seealso
#' \code{\link{prepare_test_data}} for preparing test data
#' \code{\link{check_test_window}} for checking test window deviations
#' \code{\link{generate_planned_visit_dates}} for the visit counterpart
#' \code{\link{get_rand_date}} / \code{\link{get_first_dose_date}} /
#' \code{\link{get_cyc_dose_date}} for date extraction
#'
#' @family data preparation
#'
#' @importFrom dplyr filter mutate select case_when
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#'
generate_test_window_dates <- function(data) {
  # ============================================================================
  # Part 1: Validate input parameters
  # ============================================================================
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame prepared by prepare_test_data()")
  }

  required_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TESTCAT", "TESTDAT",
    "wp_rule", "ref", "type", "wpvalue"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "'data' is missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nPlease use prepare_test_data() with config from read_testwp_file()"
    )
  }

  # ============================================================================
  # Part 2: Initialize anchor and status columns
  # ============================================================================
  visitnum_orig <- data$VISITNUM
  result <- data %>%
    mutate(VISITNUM = as.character(.data$VISITNUM))

  # Ensure wp_unit: prefer existing column, else infer from wp
  if (!"wp_unit" %in% names(result)) {
    if ("wp" %in% names(result)) {
      result$wp_unit <- ifelse(
        vapply(result$wp, is_hour_wp, logical(1)),
        "h", "d"
      )
    } else {
      result$wp_unit <- "d"
    }
  } else {
    miss_unit <- is.na(result$wp_unit) | !nzchar(as.character(result$wp_unit))
    if (any(miss_unit) && "wp" %in% names(result)) {
      result$wp_unit[miss_unit] <- ifelse(
        vapply(result$wp[miss_unit], is_hour_wp, logical(1)),
        "h", "d"
      )
    }
    bad_unit <- is.na(result$wp_unit) | !(result$wp_unit %in% c("h", "d"))
    result$wp_unit[bad_unit] <- "d"
  }

  result$anchor_date <- as.Date(NA)
  result$window_status <- ifelse(is.na(result$ref), "no_rule", "ok")

  # ============================================================================
  # Part 3: Resolve anchor dates by ref type
  # ============================================================================
  # SV: actual visit date at this visit
  is_sv <- !is.na(result$ref) & result$ref == "SV"
  sv_anchor <- as.Date(ifelse(
    !is_sas_na(result$SVDAT),
    as.character(as.Date(result$SVDAT)),
    NA_character_
  ))
  result$anchor_date[is_sv] <- sv_anchor[is_sv]

  # RD: randomization date prepared by prepare_test_data()
  is_rd <- !is.na(result$ref) & result$ref == "RD"
  if (any(is_rd)) {
    if (!"rd_date" %in% names(result)) {
      warning(
        sum(is_rd),
        " record(s) with RD rules have no window: 'rd_date' column missing. ",
        "Use prepare_test_data() so rd_date is attached."
      )
      result$window_status[is_rd] <- "no_anchor_data"
    } else {
      result$anchor_date[is_rd] <- as.Date(result$rd_date[is_rd])
    }
  }

  # FD: first dose date prepared by prepare_test_data()
  is_fd <- !is.na(result$ref) & result$ref == "FD"
  if (any(is_fd)) {
    if (!"first_dose_date" %in% names(result)) {
      warning(
        sum(is_fd),
        " record(s) with FD rules have no window: 'first_dose_date' column missing. ",
        "Use prepare_test_data() so first_dose_date is attached."
      )
      result$window_status[is_fd] <- "no_anchor_data"
    } else {
      result$anchor_date[is_fd] <- as.Date(result$first_dose_date[is_fd])
    }
  }

  # EX: per-visit dosing date prepared by prepare_test_data()
  is_ex <- !is.na(result$ref) & result$ref == "EX"
  if (any(is_ex)) {
    if (!"cyc_dose_date" %in% names(result)) {
      warning(
        sum(is_ex),
        " record(s) with EX rules have no window: 'cyc_dose_date' column missing. ",
        "Use prepare_test_data() so cyc_dose_date is attached."
      )
      result$window_status[is_ex] <- "no_anchor_data"
    } else {
      result$anchor_date[is_ex] <- as.Date(result$cyc_dose_date[is_ex])
    }
  }

  # Mark rows whose anchor date could not be resolved
  anchor_missing <- result$window_status == "ok" & is.na(result$anchor_date)
  result$window_status[anchor_missing] <- "missing_anchor_date"

  # ============================================================================
  # Part 4: Resolve anchor datetimes for hour-level rules
  # ============================================================================
  n <- nrow(result)
  result$anchor_datetime <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  result$window_start_dt <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  result$window_end_dt <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")

  is_hour <- result$window_status == "ok" & !is.na(result$wp_unit) & result$wp_unit == "h"
  if (any(is_hour)) {
    anchor_time <- rep(NA_character_, n)

    if (any(is_hour & is_ex)) {
      if ("cyc_dose_time" %in% names(result)) {
        anchor_time[is_hour & is_ex] <- as.character(result$cyc_dose_time[is_hour & is_ex])
      }
    }
    if (any(is_hour & is_fd)) {
      if ("first_dose_time" %in% names(result)) {
        anchor_time[is_hour & is_fd] <- as.character(result$first_dose_time[is_hour & is_fd])
      }
    }
    # SV / RD hour rules: use date at 00:00 (no dedicated time columns)

    result$anchor_datetime[is_hour] <- combine_date_time(
      result$anchor_date[is_hour],
      anchor_time[is_hour]
    )
  }

  # ============================================================================
  # Part 5: Derive window ranges (day-level and hour-level)
  # ============================================================================
  result$window_start <- as.Date(NA)
  result$window_end <- as.Date(NA)

  is_day <- result$window_status == "ok" &
    (is.na(result$wp_unit) | result$wp_unit != "h")

  # Day-level windows (wpvalue in days)
  if (any(is_day)) {
    day_idx <- which(is_day)
    type_d <- result$type[day_idx]
    anchor_d <- result$anchor_date[day_idx]
    val_d <- result$wpvalue[day_idx]

    start_d <- as.Date(rep(NA, length(day_idx)))
    end_d <- as.Date(rep(NA, length(day_idx)))

    is0 <- type_d == "0"
    is_pm <- type_d %in% c("±", "-", "≤")
    is_p <- type_d %in% c("+", "≥")
    is_pm_end <- type_d %in% c("±", "+", "≥")
    is_m_end <- type_d %in% c("-", "≤")

    start_d[is0] <- anchor_d[is0]
    end_d[is0] <- anchor_d[is0]
    start_d[is_pm] <- anchor_d[is_pm] - val_d[is_pm]
    start_d[is_p] <- anchor_d[is_p]
    end_d[is_pm_end] <- anchor_d[is_pm_end] + val_d[is_pm_end]
    end_d[is_m_end] <- anchor_d[is_m_end]

    result$window_start[day_idx] <- start_d
    result$window_end[day_idx] <- end_d
  }

  # Hour-level windows (hours parsed from wp string)
  if (any(is_hour)) {
    hour_idx <- which(is_hour)
    type_h <- result$type[hour_idx]
    anchor_dt <- result$anchor_datetime[hour_idx]
    hours_h <- if ("wp" %in% names(result)) {
      parse_hours_from_wp(result$wp[hour_idx])
    } else {
      result$wpvalue[hour_idx]
    }
    # Prefer parse_hours_from_wp; fall back to wpvalue when wp missing
    hours_h[is.na(hours_h)] <- result$wpvalue[hour_idx][is.na(hours_h)]

    start_dt <- as.POSIXct(rep(NA_real_, length(hour_idx)), origin = "1970-01-01", tz = "UTC")
    end_dt <- as.POSIXct(rep(NA_real_, length(hour_idx)), origin = "1970-01-01", tz = "UTC")

    delta <- as.difftime(hours_h, units = "hours")
    is0 <- type_h == "0"
    is_pm <- type_h %in% c("±", "-", "≤")
    is_p <- type_h %in% c("+", "≥")
    is_pm_end <- type_h %in% c("±", "+", "≥")
    is_m_end <- type_h %in% c("-", "≤")

    start_dt[is0] <- anchor_dt[is0]
    end_dt[is0] <- anchor_dt[is0]
    start_dt[is_pm] <- anchor_dt[is_pm] - delta[is_pm]
    start_dt[is_p] <- anchor_dt[is_p]
    end_dt[is_pm_end] <- anchor_dt[is_pm_end] + delta[is_pm_end]
    end_dt[is_m_end] <- anchor_dt[is_m_end]

    result$window_start_dt[hour_idx] <- start_dt
    result$window_end_dt[hour_idx] <- end_dt
    # Calendar date bounds for display / day fallback
    result$window_start[hour_idx] <- as.Date(start_dt)
    result$window_end[hour_idx] <- as.Date(end_dt)
  }

  # Rules whose type/wpvalue cannot produce a window (e.g. range/other)
  unsupported <- result$window_status == "ok" &
    (is.na(result$window_start) | is.na(result$window_end))
  result$window_status[unsupported] <- "unsupported_rule"

  # Restore original VISITNUM values
  result$VISITNUM <- visitnum_orig

  return(result)
}
