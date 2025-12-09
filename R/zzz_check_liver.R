#' Check liver function test exclusion criteria
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when
#' @export
check_liver_exclusion <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("LB", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomization information
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get liver function tests at screening
  liver_tests <- data$LB %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    filter(
      VISITNAME == "筛选期V1",
      LBTEST %in% c("天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶", "总胆红素")
    ) %>%
    filter(!is_sas_na(LBORRES) & !is_sas_na(LBORNRHI)) %>%
    mutate(
      TEST_VALUE = as.numeric(LBORRES),
      ULN = as.numeric(LBORNRHI),
      RATIO = TEST_VALUE / ULN,
      CRITERION = case_when(
        LBTEST %in% c("天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶") & RATIO > 2 ~
          sprintf("%s > 2倍正常值上限", LBTEST),
        LBTEST == "总胆红素" & RATIO > 1.5 ~
          "总胆红素 > 1.5倍正常值上限",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(CRITERION))

  # Group by subject to combine multiple abnormalities
  deviations <- liver_tests %>%
    select(SUBJID, LBTEST, TEST_VALUE, ULN, RATIO, CRITERION)

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "符合排除标准4.1，ALT或AST > 2倍正常值上限或总胆红素 > 1.5倍正常值上限"
    results$details <- deviations
  }

  class(results) <- c("liver_check", "list")
  return(results)
}

#' Print method for liver function check results
#' @param x Object of class liver_check
#' @param ... Additional arguments
#' @export
print.liver_check <- function(x, ...) {
  cat("4.4符合排除标准实验室检查要求却入组\n")
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
        "受试者%s的%s为%s（正常值上限：%s，%s倍ULN）",
        row["SUBJID"],
        row["LBTEST"],
        row["TEST_VALUE"],
        row["ULN"],
        row["RATIO"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
