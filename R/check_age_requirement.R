#' Check for patients under 18 years old
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select
#' @export
check_age_requirement <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  if (!"DM" %in% names(data)) {
    stop("Missing DM dataset")
  }

  # Find subjects under 18
  underage_subjects <- data$DM %>%
    filter(AGE < 18) %>%
    select(SUBJID, AGE)

  # Compile results
  if (nrow(underage_subjects) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "不符合入组标准1，＜18岁的男性或女性受试者"
    results$details <- underage_subjects
  }

  class(results) <- c("age_check", "list")
  return(results)
}

#' Print method for age requirement check results
#' @param x Object of class age_check
#' @param ... Additional arguments
#' @export
print.age_check <- function(x, ...) {
  cat("3.1不符合入选标准却入组\n")
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
        "受试者%s的年龄为%s岁，不满足入组年龄要求（≥18岁）。",
        row["SUBJID"],
        row["AGE"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
