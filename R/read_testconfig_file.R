#' Read Test Configuration File
#'
#' @description
#' Read test-visit configuration from Excel or CSV file and expand VISITNUM
#' column into multiple rows for subsequent use in \code{\link{prepare_test_data}}.
#'
#' @details
#' ## Purpose
#' This function reads external configuration files that define which tests
#' should be performed at which visits. The output can be reused multiple times
#' with \code{\link{prepare_test_data}} without re-reading the file.
#'
#' ## Usage Workflow
#'
#' ```r
#' # Step 1: Read config file once
#' testconfig <- read_testconfig_file("test_config.xlsx")
#'
#' # Step 2: Use config multiple times
#' lb_data <- prepare_test_data(data, "LB", config = testconfig)
#' eg_data <- prepare_test_data(data, "EG", config = testconfig)
#' ```
#'
#' ## VISITNUM Expansion
#' The VISITNUM column in config file can contain comma-separated visit numbers
#' (e.g., "1,2,3" or with fullwidth commas). This function expands them into separate rows:
#'
#' Input:
#' | TESTCAT | VISITNUM |
#' |---------|----------|
#' | CBC     | 1,2,3    |
#'
#' Output:
#' | TESTCAT | VISITNUM |
#' |---------|----------|
#' | CBC     | 1        |
#' | CBC     | 2        |
#' | CBC     | 3        |
#'
#' ## Required Columns
#' The input file must contain:
#' - TESTCAT: Test category name
#' - VISITNUM: Visit numbers (comma-separated string or single number)
#'
#' ## Joining with Visit Code
#' If \code{visitcode} parameter is provided (from \code{\link{read_visitcode_file}}),
#' the VISIT column will be joined to the result by VISITNUM:
#'
#' ```r
#' # Read visit code first
#' visitcode <- read_visitcode_file("visit_schedule.xlsx")
#'
#' # Read test config with visit names
#' testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
#' # Result will contain: TESTCAT, VISITNUM, VISIT, ...
#' ```
#'
#' @param file_path Character. File path (.xlsx, .xls, or .csv).
#'   Must be a non-empty string pointing to an existing file.
#' @param sheet_name Character. Excel sheet name (default: "Sheet1").
#'   Ignored for CSV files.
#' @param visitcode Data frame. Result from \code{\link{read_visitcode_file}} (default: NULL).
#'   If NULL, the function will look for a variable named \code{visitcode} in the
#'   calling environment. If found, VISIT column will be joined by VISITNUM.
#'   The visitcode data frame must contain VISITNUM and VISIT columns.
#'
#' @return A data frame with expanded rows:
#'   \describe{
#'     \item{TESTCAT}{Test category (character)}
#'     \item{VISITNUM}{Visit number (character), one per row}
#'     \item{VISIT}{Visit name (character), if visitcode is provided}
#'     \item{...}{All other columns from input file}
#'   }
#'
#' @examples
#' \dontrun{
#' # Read from Excel file
#' testconfig <- read_testconfig_file("test_config.xlsx")
#'
#' # Read from specific sheet
#' testconfig <- read_testconfig_file("test_config.xlsx", sheet_name = "Config")
#'
#' # Read from CSV file
#' testconfig <- read_testconfig_file("test_config.csv")
#'
#' # Read with visit names (recommended workflow)
#' visitcode <- read_visitcode_file("visit_schedule.xlsx")
#' testconfig <- read_testconfig_file("test_config.xlsx", visitcode = visitcode)
#' # testconfig now contains: TESTCAT, VISITNUM, VISIT, ...
#'
#' # Use in prepare_test_data
#' prepared_data <- prepare_test_data(
#'   data = all_data,
#'   test_dataset = "LB",
#'   config = testconfig
#' )
#' }
#'
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#' @importFrom dplyr bind_rows left_join select
#' @export
read_testconfig_file <- function(file_path, sheet_name = "Sheet1", visitcode = NULL) {
  # ============================================================================
  # Parameter validation
  # ============================================================================

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

  # ============================================================================
  # Read file
  # ============================================================================

  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext %in% c("xlsx", "xls")) {
    # Read Excel file
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel config files", call. = FALSE)
    }
    config_df <- readxl::read_excel(file_path, sheet = sheet_name)
  } else if (file_ext == "csv") {
    # Read CSV file with UTF-8 encoding support
    config_df <- tryCatch(
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
    config_df <- tibble::as_tibble(config_df)
  } else {
    stop(
      "Unsupported file format. Please use .xlsx, .xls, or .csv files.",
      call. = FALSE
    )
  }

  # ============================================================================
  # Validate data structure
  # ============================================================================

  if (nrow(config_df) == 0) {
    warning("The input file contains no data rows.")
    return(config_df)
  }

  # Check for required columns
  if (!"TESTCAT" %in% names(config_df)) {
    stop(
      "Missing required column 'TESTCAT' in the input file. ",
      "Available columns: ", paste(names(config_df), collapse = ", "),
      call. = FALSE
    )
  }

  if (!"VISITNUM" %in% names(config_df)) {
    stop(
      "Missing required column 'VISITNUM' in the input file. ",
      "Available columns: ", paste(names(config_df), collapse = ", "),
      call. = FALSE
    )
  }

  # ============================================================================
  # Expand VISITNUM column
  # ============================================================================

  # Get other columns (excluding TESTCAT and VISITNUM)
  other_cols <- setdiff(names(config_df), c("TESTCAT", "VISITNUM"))

  config_list <- lapply(seq_len(nrow(config_df)), function(i) {
    testcat <- config_df$TESTCAT[i]
    visitnum_str <- as.character(config_df$VISITNUM[i])

    # Split by comma (both English and Chinese comma)
    visitnums <- as.numeric(unlist(strsplit(visitnum_str, "[,，]")))
    visitnums <- visitnums[!is.na(visitnums)]

    # Create base data frame
    result <- data.frame(
      TESTCAT = testcat,
      VISITNUM = as.character(visitnums),
      stringsAsFactors = FALSE
    )

    # Add other columns if they exist
    if (length(other_cols) > 0) {
      for (col in other_cols) {
        result[[col]] <- config_df[[col]][i]
      }
    }

    result
  })

  config_expanded <- dplyr::bind_rows(config_list)

  # ============================================================================
  # Join with visitcode to get VISIT column
  # ============================================================================

  # Try to get visitcode from caller's environment if NULL
  if (is.null(visitcode)) {
    if (exists("visitcode", envir = parent.frame(), inherits = TRUE)) {
      visitcode <- get("visitcode", envir = parent.frame(), inherits = TRUE)
    }
  }

  if (!is.null(visitcode)) {
    # Validate visitcode
    if (!is.data.frame(visitcode)) {
      stop("'visitcode' must be a data frame (from read_visitcode_file()).", call. = FALSE)
    }

    if (!"VISITNUM" %in% names(visitcode)) {
      stop("'visitcode' must contain VISITNUM column.", call. = FALSE)
    }

    if (!"VISIT" %in% names(visitcode)) {
      stop("'visitcode' must contain VISIT column.", call. = FALSE)
    }

    # Prepare visitcode for join (only keep VISITNUM and VISIT)
    visitcode_subset <- visitcode %>%
      dplyr::select(VISITNUM, VISIT) %>%
      dplyr::mutate(VISITNUM = as.character(VISITNUM))

    # Ensure config_expanded VISITNUM is character
    config_expanded$VISITNUM <- as.character(config_expanded$VISITNUM)

    # Left join to add VISIT column
    config_expanded <- config_expanded %>%
      dplyr::left_join(visitcode_subset, by = "VISITNUM")

    # Reorder columns: TESTCAT, VISITNUM, VISIT, then others
    key_cols <- c("TESTCAT", "VISITNUM", "VISIT")
    other_cols <- setdiff(names(config_expanded), key_cols)
    config_expanded <- config_expanded %>%
      dplyr::select(dplyr::all_of(key_cols), dplyr::all_of(other_cols))
  }

  return(config_expanded)
}
