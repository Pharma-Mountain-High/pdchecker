#' Check for screened subjects without informed consent
#'
#' @param data List of data frames containing study data
#' @param sv_dataset Character string specifying which dataset to use for visit data (default: "SV")
#' @param ic_dataset Character string specifying which dataset to use for informed consent data (default: "IC")
#' @param visit_var Character string specifying the variable name for visit in visit dataset (default: "VISIT")
#' @param visit_pattern Character string or vector specifying the pattern(s) to
#'   identify screening visits (default: "Screening|screening")
#' @param ic_date_var Character string specifying the variable name for informed
#'   consent date in IC dataset (default: "ICDAT")
#' @return A list of class "screen_ic_check" containing:
#' \describe{
#'   \item{has_deviation}{Logical value indicating whether any deviation was found.
#'     \code{TRUE} if subjects with screening visits but without informed consent were found,
#'     \code{FALSE} otherwise.}
#'   \item{messages}{Character vector of deviation messages.}
#'   \item{details}{Data frame containing SUBJID column with subject IDs who have screening visits
#'     but no informed consent date (or date is NA).
#'     Returns empty data frame if no deviations found.}
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
#'   visit_var = "VISITNAME",
#'   visit_pattern = "Scr",
#'   ic_date_var = "ICFDAT"
#' )
#' }
#'
#' @family deviation checks
#' @seealso [check_icf_time_deviation()] for checking events before informed consent
#'
#' @importFrom dplyr anti_join select distinct filter %>%
#' @importFrom rlang .data
#' @export
check_screen_without_ic <- function(data,
                                    sv_dataset = "SV",
                                    ic_dataset = "IC",
                                    visit_var = "VISIT",
                                    visit_pattern = "Screening|screening",
                                    ic_date_var = "ICDAT") {
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
  if (!is.character(visit_var) || length(visit_var) != 1) {
    stop("'visit_var' must be a single character string")
  }
  if (!is.character(visit_pattern) || length(visit_pattern) < 1) {
    stop("'visit_pattern' must be a character string")
  }
  if (!is.character(ic_date_var) || length(ic_date_var) != 1) {
    stop("'ic_date_var' must be a single character string")
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c(sv_dataset, ic_dataset)
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Validate that visit_var exists in sv_dataset
  if (!visit_var %in% names(data[[sv_dataset]])) {
    stop(sprintf("Variable '%s' not found in dataset '%s'", visit_var, sv_dataset))
  }

  # Validate that ic_date_var exists in ic_dataset
  if (!ic_date_var %in% names(data[[ic_dataset]])) {
    stop(sprintf("Variable '%s' not found in dataset '%s'", ic_date_var, ic_dataset))
  }

  # Get unique subjects from screening dataset with screening visits
  screening_subjects <- data[[sv_dataset]] %>%
    filter(grepl(visit_pattern, .data[[visit_var]], ignore.case = TRUE)) %>%
    distinct(SUBJID)

  # Get unique subjects from IC dataset with non-empty consent date
  ic_subjects <- data[[ic_dataset]] %>%
    filter(!is.na(.data[[ic_date_var]])) %>%
    distinct(SUBJID)

  # Find subjects with screening visit but not in IC
  missing_ic <- screening_subjects %>%
    anti_join(ic_subjects, by = "SUBJID")

  # Compile results
  if (nrow(missing_ic) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "未签署知情同意书"
    results$details <- missing_ic
  }

  class(results) <- c("screen_ic_check", "list")
  return(results)
}

#' Print method for screen without IC check results
#' @param x Object of class screen_ic_check
#' @param ... Additional arguments
#' @export
print.screen_ic_check <- function(x, ...) {
  cat("2.4 未签署知情同意书\n")
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
    formatted_details <- apply(x$details, 1, function(row) {
      sprintf(
        "受试者%s在未签署知情同意书的情况下进行了筛选期访视。",
        row["SUBJID"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
