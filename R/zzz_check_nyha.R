#' Check NYHA class exclusion criteria
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate
#' @export
check_nyha_requirement <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("NYHA", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomization information
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get NYHA classification
  nyha_data <- data$NYHA %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    select(SUBJID, NYHAYN, NYHASCO) %>%
    filter(!is_sas_na(NYHAYN)) %>%
    mutate(
      has_nyha = NYHAYN == "是" & NYHASCO != "I级",
      nyha_class = NYHASCO
    )

  # Check NYHA requirement (II, III, IV class)
  deviations <- nyha_data %>%
    filter(
      has_nyha
    )

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "符合排除标准1.1的任意一项, 纽约心脏协会（NYHA）II、III、IV级心力衰竭或已知最后一次检查的左心室射血分数<30%。"
    results$details <- deviations
  }

  class(results) <- c("nyha_check", "list")
  return(results)
}

#' Print method for NYHA requirement check results
#' @param x Object of class nyha_check
#' @param ... Additional arguments
#' @export
print.nyha_check <- function(x, ...) {
  cat("4.1 符合排除标准存在以下心血管疾病或治疗史却入组\n")
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
        "受试者%s的NYHA分级为%s",
        row["SUBJID"],
        row["nyha_class"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
