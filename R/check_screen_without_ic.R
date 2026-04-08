#' Check for screened subjects without informed consent
#'
#' @param data List of data frames containing study data
#' @param sv_dataset Character string specifying which dataset to use for visit data (default: "SV")
#' @param ic_dataset Character string specifying which dataset to use for informed consent data (default: "IC")
#' @param sv_visit_var Character string specifying the variable name for visit in visit dataset (default: "VISIT")
#' @param visit_pattern Character string or vector specifying the pattern(s) to
#'   identify screening visits (default: "Screening|screening")
#' @param ic_date_var Character string specifying the variable name for informed
#'   consent date in IC dataset (default: "ICDAT")
#' @param tb_name_var Character string specifying the variable name to use as TBNAME in the output (default: NULL).
#' If NULL, the TBNAME column in the output will be empty.
#' @param pdno Character string specifying the protocol deviation number for this check (default: "2.4.1")
#' @return A list of class "screen_ic_check" containing:
#' \describe{
#'   \item{has_deviation}{Logical value indicating whether any deviation was found.
#'     \code{TRUE} if subjects with screening visits but without informed consent were found,
#'     \code{FALSE} otherwise.}
#'   \item{messages}{Character vector of deviation messages.}
#'   \item{details}{Data frame containing the following columns:
#'     \describe{
#'       \item{PDNO}{Protocol deviation number specified by \code{pdno} parameter.}
#'       \item{SUBJID}{Subject IDs who have screening visits but no informed consent date.}
#'       \item{VISIT}{Visit name from the visit dataset.}
#'       \item{TBNAME}{Table name from the variable specified by \code{tb_name_var}, empty if \code{tb_name_var} is NULL.}
#'       \item{DESCRIPTION}{Description of the deviation for each subject.}
#'     }
#'     Returns empty data frame with these columns if no deviations found.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic usage with default parameters
#' data <- list(
#'   SV = data.frame(
#'     SUBJID = c("001", "002", "003"),
#'     VISIT = c("Screening", "Screening", "C1D1")
#'   ),
#'   IC = data.frame(
#'     SUBJID = c("001", "002"),
#'     ICDAT = c("2024-01-01", "2024-01-02")
#'   )
#' )
#' result <- check_screen_without_ic(data)
#' print(result)
#'
#' # Example 2: Using custom dataset names
#' data2 <- list(
#'   VIS = data.frame(
#'     SUBJID = c("001", "002"),
#'     VISIT = c("Screening", "Screening")
#'   ),
#'   ICF = data.frame(
#'     SUBJID = c("001"),
#'     ICDAT = c("2024-01-01")
#'   )
#' )
#' result2 <- check_screen_without_ic(
#'   data2,
#'   sv_dataset = "VIS",
#'   ic_dataset = "ICF"
#' )
#'
#' # Example 3: Using custom variable names and visit pattern
#' data3 <- list(
#'   SV = data.frame(
#'     SUBJID = c("001", "002", "003"),
#'     VISITNAME = c("Scr", "Scr", "Treatment")
#'   ),
#'   IC = data.frame(
#'     SUBJID = c("001", "002"),
#'     ICFDAT = c("2024-01-01", "2024-01-02")
#'   )
#' )
#' result3 <- check_screen_without_ic(
#'   data3,
#'   sv_visit_var = "VISITNAME",
#'   visit_pattern = "Scr",
#'   ic_date_var = "ICFDAT"
#' )
#' }
#'
#' @family deviation checks
#' @seealso [check_icf_time_deviation()] for checking events before informed consent
#'
#' @importFrom dplyr anti_join select distinct filter mutate %>%
#' @importFrom rlang .data
#' @export
check_screen_without_ic <- function(data,
                                    sv_dataset = getOption("pdchecker.sv_dataset", "SV"),
                                    ic_dataset = getOption("pdchecker.ic_dataset", "IC"),
                                    sv_visit_var = getOption("pdchecker.sv_visit_var", "VISIT"),
                                    visit_pattern = "Screening|screening",
                                    ic_date_var = getOption("pdchecker.ic_date_var", "ICDAT"),
                                    tb_name_var = getOption("pdchecker.tb_name_var", NULL),
                                    pdno = "2.4.1") {
  # Validate parameter types
  if (!is.list(data)) {
    stop("'data' must be a list of data frames")
  }
  if (!is.character(sv_dataset) || length(sv_dataset) != 1) {
    stop("'sv_dataset' must be a single character string")
  }
  if (!is.character(ic_dataset) || length(ic_dataset) != 1) {
    stop("'ic_dataset' must be a single character string")
  }
  if (!is.character(sv_visit_var) || length(sv_visit_var) != 1) {
    stop("'sv_visit_var' must be a single character string")
  }
  if (!is.character(visit_pattern) || length(visit_pattern) < 1) {
    stop("'visit_pattern' must be a character string")
  }
  if (!is.character(ic_date_var) || length(ic_date_var) != 1) {
    stop("'ic_date_var' must be a single character string")
  }
  if (!is.null(tb_name_var) && (!is.character(tb_name_var) || length(tb_name_var) != 1)) {
    stop("'tb_name_var' must be NULL or a single character string")
  }
  if (!is.character(pdno) || length(pdno) != 1) {
    stop("'pdno' must be a single character string")
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame(
      PDNO = character(),
      SUBJID = character(),
      VISIT = character(),
      TBNAME = character(),
      DESCRIPTION = character(),
      stringsAsFactors = FALSE
    )
  )

  # Validate required datasets
  required_datasets <- c(sv_dataset, ic_dataset)
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  if (!sv_visit_var %in% names(data[[sv_dataset]])) {
    stop(sprintf("Variable '%s' not found in dataset '%s'", sv_visit_var, sv_dataset))
  }

  # Validate that ic_date_var exists in ic_dataset
  if (!ic_date_var %in% names(data[[ic_dataset]])) {
    stop(sprintf("Variable '%s' not found in dataset '%s'", ic_date_var, ic_dataset))
  }

  # Get screening visit records with visit info
  screening_records <- data[[sv_dataset]] %>%
    filter(grepl(visit_pattern, .data[[sv_visit_var]], ignore.case = TRUE)) %>%
    select(SUBJID, VISIT = .data[[sv_visit_var]]) %>%
    distinct()


  # Resolve TBNAME from IC dataset
  ic_df <- data[[ic_dataset]]
  has_tb_name <- !is.null(tb_name_var) && tb_name_var %in% names(ic_df)

  # Get unique subjects from IC dataset with non-empty consent date
  ic_subjects <- ic_df %>%
    filter(!is.na(.data[[ic_date_var]])) %>%
    distinct(SUBJID)

  # Find screening records for subjects not in IC
  missing_ic <- screening_records %>%
    anti_join(ic_subjects, by = "SUBJID")

  # Add TBNAME from IC dataset
  if (has_tb_name) {
    tb_values <- ic_df[[tb_name_var]]
    tb_values <- tb_values[!is.na(tb_values) & tb_values != ""]
    tbname_value <- if (length(tb_values) > 0) as.character(tb_values[1]) else ""
    missing_ic <- missing_ic %>% mutate(TBNAME = tbname_value)
  } else {
    missing_ic <- missing_ic %>% mutate(TBNAME = "")
  }

  # Compile results
  if (nrow(missing_ic) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "未签署知情同意书"
    results$details <- data.frame(
      PDNO = pdno,
      SUBJID = missing_ic$SUBJID,
      VISIT = missing_ic$VISIT,
      TBNAME = missing_ic$TBNAME,
      DESCRIPTION = sprintf(
        "受试者编号%s，在未签署知情同意书的情况下进行了%s访视。",
        missing_ic$SUBJID,
        missing_ic$VISIT
      ),
      stringsAsFactors = FALSE
    )
  }

  class(results) <- c("screen_ic_check", "list")
  return(results)
}

#' Print method for screen without IC check results
#' @param x Object of class screen_ic_check
#' @param ... Additional arguments
#' @export
print.screen_ic_check <- function(x, ...) {
  pdno_display <- if (nrow(x$details) > 0) x$details$PDNO[1] else "2.4.1"
  cat(sprintf("%s 未签署知情同意书\n", pdno_display))
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
