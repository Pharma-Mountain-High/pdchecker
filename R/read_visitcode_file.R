#' Parse Time Unit String to Days
#'
#' @description
#' Convert time unit string to number of days.
#' Supports hours (h), days (d), and weeks (w).
#'
#' @param value_str Time value string (e.g., "3d", "24h", "2w")
#' @return Numeric value in days
#' @noRd
parse_time_unit <- function(value_str) {
  if (grepl("h$|小时$", value_str)) {
    # Hours to days
    num <- as.numeric(gsub("h$|小时$", "", value_str))
    return(num / 24)
  } else if (grepl("w$|周$", value_str)) {
    # Weeks to days
    num <- as.numeric(gsub("w$|周$", "", value_str))
    return(num * 7)
  } else if (grepl("d$|天$|日$", value_str)) {
    # Days
    num <- as.numeric(gsub("d$|天$|日$", "", value_str))
    return(num)
  } else {
    # Default: treat as days
    return(as.numeric(value_str))
  }
}

#' Parse Visit Window Period String
#'
#' @param window_str Window period string (e.g., "+/-3d", "<=24h", "+2d", "-1d")
#' @return List containing window type and value
#' @noRd
parse_window_period <- function(window_str) {
  # Handle missing values
  if (is.na(window_str) || window_str == "" || is.null(window_str)) {
    return(list(type = NA, value = NA))
  }

  # Convert to string and trim whitespace
  window_str <- trimws(as.character(window_str))

  # Define window type pattern table: pattern, type, gsub_pattern
  window_patterns <- list(
    list(pattern = "^±", type = "±", gsub_pattern = "^±"),
    list(pattern = "^(≤|<=)", type = "≤", gsub_pattern = "^(≤|<=)"),
    list(pattern = "^(≥|>=)", type = "≥", gsub_pattern = "^(≥|>=)"),
    list(pattern = "^\\+", type = "+", gsub_pattern = "^\\+"),
    list(pattern = "^-(?!.*(到|至))", type = "-", gsub_pattern = "^-")
  )

  # Iterate through pattern table for matching
  for (wp in window_patterns) {
    if (grepl(wp$pattern, window_str, perl = TRUE)) {
      value_part <- gsub(wp$gsub_pattern, "", window_str)
      value <- parse_time_unit(value_part)
      return(list(type = wp$type, value = value))
    }
  }

  # Range type (e.g., -2到+4, 1至3天)
  if (grepl("到|至", window_str)) {
    return(list(type = "范围", value = window_str))
  }

  # Numeric without prefix (e.g., 2d) -> default to +
  if (grepl("^[0-9]", window_str)) {
    value <- parse_time_unit(window_str)
    return(list(type = "+", value = value))
  }

  # Other formats
  return(list(type = "其他", value = window_str))
}

#' Read Visit Schedule Data and Parse Window Periods
#'
#' @description
#' Read visit schedule Excel or CSV file and parse window period column (WP)
#' into two new variables:
#' - type: Distinguishes window types (<=, +/-, +, -, range, etc.)
#' - wpvalue: Specific window time value (automatically converts hours to days)
#'
#' Supported window period format examples:
#' - "+/-3d" -> type: +/-, value: 3
#' - "<=24h" -> type: <=, value: 1
#' - "+2d" -> type: +, value: 2
#' - "-1d" -> type: -, value: 1
#' - "1w" -> type: +, value: 7 (1 week = 7 days)
#' - "+/-2w" -> type: +/-, value: 14 (2 weeks = 14 days)
#'
#' @details
#' ## Window Period Column
#' The input file must contain a column named "WP" (case-sensitive).
#'
#' ## Time Unit Conversion
#' All time units are converted to days:
#' - Hours (h): divided by 24
#' - Days (d): unchanged
#' - Weeks (w): multiplied by 7
#'
#' ## Encoding Support
#' CSV files default to UTF-8 encoding, with fallback to system default.
#'
#' ## Column Name Conflicts
#' If type or wpvalue columns already exist, they will be overwritten with a message.
#'
#' @param file_path Character. File path (.xlsx, .xls, or .csv).
#'   Must be a non-empty string pointing to an existing file.
#' @param sheet_name Character. Excel sheet name (default: "Sheet1").
#'   Ignored for CSV files.
#' @return A tibble with all columns from input file, plus two new (or overwritten) columns:
#'   \item{type}{Window type (character): +/-, <=, >=, +, -, range, other, or NA}
#'   \item{wpvalue}{Window value (numeric) in days. NA if unparseable (e.g., range or other types)}
#'
#' @examples
#' \dontrun{
#' # Read from Excel file
#' data <- read_visitcode_file("visit_schedule.xlsx")
#'
#' # Read from specific sheet
#' data <- read_visitcode_file("visit_schedule.xlsx", sheet_name = "Schedule")
#'
#' # Read from CSV file
#' data <- read_visitcode_file("visit_schedule.csv")
#'
#' # Example output structure:
#' #   visit  WP      type   wpvalue
#' #   V1     ±3d     ±      3
#' #   V2     <=24h   ≤      1
#' #   V3     +2d     +      2
#' #   V4     -1d     -      1
#' #   V5     1w      +      7
#' }
#'
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#' @export
read_visitcode_file <- function(file_path, sheet_name = "Sheet1") {
  # ---- Parameter validation ----
  # Validate file_path
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.", call. = FALSE)
  }
  if (is.na(file_path) || nchar(trimws(file_path)) == 0) {
    stop("'file_path' cannot be NA or empty.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }

  # Validate sheet_name
  if (!is.character(sheet_name) || length(sheet_name) != 1) {
    stop("'sheet_name' must be a single character string.", call. = FALSE)
  }
  if (is.na(sheet_name) || nchar(trimws(sheet_name)) == 0) {
    stop("'sheet_name' cannot be NA or empty.", call. = FALSE)
  }

  # ---- Read file ----
  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext %in% c("xlsx", "xls")) {
    # Read Excel file
    data <- readxl::read_excel(file_path, sheet = sheet_name)
  } else if (file_ext == "csv") {
    # Read CSV file with UTF-8 encoding support
    data <- tryCatch(
      {
        # Try UTF-8 encoding first
        read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      },
      error = function(e) {
        # Fallback to system default encoding
        read.csv(file_path, stringsAsFactors = FALSE)
      }
    )
    # Convert to tibble for consistency
    data <- tibble::as_tibble(data)
  } else {
    stop(
      "Unsupported file format. Please use .xlsx, .xls, or .csv files.",
      call. = FALSE
    )
  }

  # ---- Validate data structure ----
  if (nrow(data) == 0) {
    warning("The input file contains no data rows.")
  }

  # Check for required WP column
  if (!"WP" %in% names(data)) {
    stop(
      "Missing required column 'WP' in the input file. ",
      "Available columns: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Handle column name conflicts ----
  if ("type" %in% names(data)) {
    message("Note: Existing 'type' column will be overwritten.")
    data <- data[, !names(data) %in% "type"]
  }
  if ("wpvalue" %in% names(data)) {
    message("Note: Existing 'wpvalue' column will be overwritten.")
    data <- data[, !names(data) %in% "wpvalue"]
  }

  # ---- Parse window periods (vectorized) ----
  parsed_results <- lapply(data[["WP"]], parse_window_period)

  # Extract type and wpvalue
  data$type <- vapply(parsed_results, function(x) {
    if (is.na(x$type)) NA_character_ else as.character(x$type)
  }, FUN.VALUE = character(1))

  data$wpvalue <- vapply(parsed_results, function(x) {
    if (is.numeric(x$value)) {
      x$value
    } else {
      NA_real_
    }
  }, FUN.VALUE = numeric(1))

  return(data)
}
