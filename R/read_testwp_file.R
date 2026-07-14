#' Parse a test window rule cell (e.g. "EX(≤24h)", "RD(0)", "SV(±3d)")
#'
#' @param cell_value Character scalar from the test-window matrix cell.
#' @return List with ref, wp_rule, wp, type, wpvalue; or NULL if cell is empty.
#' @noRd
parse_testwp_cell <- function(cell_value) {
  if (is.na(cell_value) || is.null(cell_value)) {
    return(NULL)
  }

  cell_value <- trimws(as.character(cell_value))
  if (!nzchar(cell_value)) {
    return(NULL)
  }

  matched <- regexec("^(RD|SV|EX)\\((.+)\\)$", cell_value, perl = TRUE)
  parts <- regmatches(cell_value, matched)[[1]]
  if (length(parts) == 0) {
    stop(
      "Invalid test window rule '", cell_value, "'. ",
      "Expected format REF(WP), e.g. RD(-7d), SV(±3d), EX(≤24h), EX(0).",
      call. = FALSE
    )
  }

  ref <- toupper(parts[2])
  wp_str <- trimws(parts[3])

  if (wp_str == "0") {
    return(list(
      ref = ref,
      wp_rule = cell_value,
      wp = wp_str,
      type = "0",
      wpvalue = 0
    ))
  }

  parsed <- parse_window_period(wp_str)
  # Keep wpvalue numeric so bind_rows() can combine all rows.
  # Non-numeric values (range / other formats) become NA_real_.
  wpvalue <- if (is.numeric(parsed$value)) {
    as.numeric(parsed$value)
  } else {
    NA_real_
  }

  list(
    ref = ref,
    wp_rule = cell_value,
    wp = wp_str,
    type = parsed$type,
    wpvalue = wpvalue
  )
}

#' Read Test Window Configuration File
#'
#' @description
#' Read a test-window matrix Excel file and expand it into one row per
#' required test at each visit, with parsed window rules.
#'
#' @details
#' ## File layout
#'
#' The input file uses a **matrix** layout (unlike \code{\link{read_testconfig_file}}):
#'
#' - Row 1: headers — column 1 \code{VISIT}, column 2 \code{VISITNUM},
#'   remaining columns are \code{TESTCAT} names.
#' - Following rows: one visit per row; non-empty cells indicate that the test
#'   is required at that visit and contain the window rule.
#'
#' ## Window rule format
#'
#' Each non-empty cell uses \code{REF(WP)}:
#'
#' | REF | Anchor date |
#' |-----|-------------|
#' | RD | Randomization date |
#' | SV | Actual visit date at this visit |
#' | EX | Actual dosing date at this visit |
#'
#' \code{WP} uses the same syntax as the \code{WP} column in
#' \code{\link{read_visitcode_file}} (\code{±3d}, \code{≤24h}, \code{+3d}, etc.).
#' \code{0} inside parentheses means the test must be performed on the anchor day.
#'
#' Examples: \code{RD(-7d)}, \code{RD(0)}, \code{EX(≤24h)}, \code{SV(±3d)}.
#'
#' @param file_path Character. File path (`.xlsx` or `.xls`).
#' @param sheet_name Character. Excel sheet name (default: `"Sheet1"`).
#' @param visitcode Data frame from \code{\link{read_visitcode_file}} (default: `NULL`).
#'   If provided (or found as `visitcode` in the calling environment), visit names
#'   from visitcode are joined by \code{VISITNUM} for validation.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{TESTCAT}{Test category (character)}
#'     \item{VISITNUM}{Visit number (character)}
#'     \item{VISIT}{Visit name from the file (character)}
#'     \item{wp_rule}{Original cell value, e.g. \code{EX(≤24h)}}
#'     \item{ref}{Reference date type: \code{RD}, \code{SV}, or \code{EX}}
#'     \item{wp}{Window period string inside parentheses}
#'     \item{type}{Parsed window type (\code{±}, \code{≤}, \code{0}, etc.)}
#'     \item{wpvalue}{Parsed window value in days (numeric; may be \code{NA})}
#'   }
#'
#' @examples
#' \dontrun{
#' testwp <- read_testwp_file(
#'   system.file("extdata", "example_test_wp.xlsx", package = "pdchecker"),
#'   sheet_name = "QL0911-302"
#' )
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom tools file_ext
#' @importFrom dplyr bind_rows left_join select mutate all_of
#' @export
read_testwp_file <- function(file_path,
                             sheet_name = "Sheet1",
                             visitcode = NULL) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.", call. = FALSE)
  }
  if (is.na(file_path) || nchar(trimws(file_path)) == 0) {
    stop("'file_path' cannot be NA or empty.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }

  if (!is.character(sheet_name) || length(sheet_name) != 1) {
    stop("'sheet_name' must be a single character string.", call. = FALSE)
  }
  if (is.na(sheet_name) || nchar(trimws(sheet_name)) == 0) {
    stop("'sheet_name' cannot be NA or empty.", call. = FALSE)
  }

  file_ext <- tolower(tools::file_ext(file_path))
  if (!file_ext %in% c("xlsx", "xls")) {
    stop(
      "Unsupported file format. Please use .xlsx or .xls files.",
      call. = FALSE
    )
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read Excel files.", call. = FALSE)
  }

  raw_df <- readxl::read_excel(file_path, sheet = sheet_name, col_names = FALSE)
  raw_df <- tibble::as_tibble(raw_df)

  if (nrow(raw_df) < 2 || ncol(raw_df) < 3) {
    stop(
      "The input file must have a header row and at least one visit row ",
      "with test columns.",
      call. = FALSE
    )
  }

  header_visit <- toupper(trimws(as.character(raw_df[[1]][1])))
  header_visitnum <- toupper(trimws(as.character(raw_df[[2]][1])))

  if (header_visit != "VISIT" || header_visitnum != "VISITNUM") {
    stop(
      "The first row must start with 'VISIT' and 'VISITNUM' in columns 1 and 2. ",
      "Found: '", raw_df[[1]][1], "', '", raw_df[[2]][1], "'.",
      call. = FALSE
    )
  }

  testcats <- trimws(as.character(unlist(raw_df[1, 3:ncol(raw_df), drop = TRUE])))
  if (any(!nzchar(testcats))) {
    stop("All test category column names in row 1 must be non-empty.", call. = FALSE)
  }

  result_list <- list()

  for (i in 2:nrow(raw_df)) {
    visit_name <- trimws(as.character(raw_df[[1]][i]))
    visitnum <- trimws(as.character(raw_df[[2]][i]))

    if (!nzchar(visitnum)) {
      next
    }

    for (j in seq_along(testcats)) {
      col_idx <- j + 2L
      cell_value <- raw_df[[col_idx]][i]
      parsed <- parse_testwp_cell(cell_value)

      if (is.null(parsed)) {
        next
      }

      result_list[[length(result_list) + 1L]] <- data.frame(
        TESTCAT = as.character(testcats[j]),
        VISITNUM = as.character(visitnum),
        VISIT = as.character(visit_name),
        wp_rule = as.character(parsed$wp_rule),
        ref = as.character(parsed$ref),
        wp = as.character(parsed$wp),
        type = as.character(parsed$type),
        wpvalue = as.numeric(parsed$wpvalue),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(result_list) == 0) {
    warning("No required test-window rules found in the input file.")
    return(data.frame(
      TESTCAT = character(),
      VISITNUM = character(),
      VISIT = character(),
      wp_rule = character(),
      ref = character(),
      wp = character(),
      type = character(),
      wpvalue = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  testwp_expanded <- dplyr::bind_rows(result_list)
  testwp_expanded$VISITNUM <- as.character(testwp_expanded$VISITNUM)

  if (is.null(visitcode)) {
    if (exists("visitcode", envir = parent.frame(), inherits = TRUE)) {
      visitcode <- get("visitcode", envir = parent.frame(), inherits = TRUE)
    }
  }

  if (!is.null(visitcode)) {
    if (!is.data.frame(visitcode)) {
      stop("'visitcode' must be a data frame (from read_visitcode_file()).", call. = FALSE)
    }
    if (!"VISITNUM" %in% names(visitcode)) {
      stop("'visitcode' must contain VISITNUM column.", call. = FALSE)
    }
    if (!"VISIT" %in% names(visitcode)) {
      stop("'visitcode' must contain VISIT column.", call. = FALSE)
    }

    visitcode_subset <- visitcode %>%
      dplyr::select(VISITNUM, VISIT) %>%
      dplyr::mutate(VISITNUM = as.character(VISITNUM)) %>%
      dplyr::distinct(VISITNUM, .keep_all = TRUE)

    testwp_expanded <- testwp_expanded %>%
      dplyr::left_join(
        visitcode_subset %>% dplyr::rename(VISIT_visitcode = VISIT),
        by = "VISITNUM"
      )

    mismatched <- !is.na(testwp_expanded$VISIT_visitcode) &
      testwp_expanded$VISIT != testwp_expanded$VISIT_visitcode
    if (any(mismatched, na.rm = TRUE)) {
      warning(
        sum(mismatched),
        " row(s) have VISIT names that differ from visitcode for the same VISITNUM.",
        call. = FALSE
      )
    }

    testwp_expanded <- testwp_expanded %>%
      dplyr::select(-VISIT_visitcode)
  }

  key_cols <- c("TESTCAT", "VISITNUM", "VISIT", "wp_rule", "ref", "wp", "type", "wpvalue")
  testwp_expanded <- testwp_expanded %>%
    dplyr::select(dplyr::all_of(key_cols))

  testwp_expanded
}
