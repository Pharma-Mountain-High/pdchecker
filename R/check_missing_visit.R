#' Check missing visits
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when group_by summarise ungroup left_join
#' @importFrom purrr pmap_chr
#' @export
check_missing_visit <- function(test_data, dataset_name) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Get date column name
  date_col <- paste0(dataset_name, "DAT")

  # Get subject visits and V3 dates
  subject_visits <- test_data %>%
    filter(!is_sas_na(!!sym(date_col))) %>%
    mutate(
      VISDAT = as.Date(!!sym(date_col)),
      VISITNAME = if_else(is_sas_na(VISITNAME), NA_character_, VISITNAME)
    ) %>%
    # Group by subject
    group_by(SUBJID) %>%
    summarise(
      visit_dates = paste(format(VISDAT, "%Y-%m-%d"), collapse = ", "),
      visit_types = paste(unique(VISITNAME), collapse = ", "),
      has_v1 = any(VISITNAME == "筛选期V1"),
      has_v2 = any(VISITNAME == "筛选期V2"),
      has_v3 = any(VISITNAME == "V3"),
      has_v4 = any(VISITNAME == "V4"),
      has_v5 = any(VISITNAME == "V5"),
      has_v6 = any(VISITNAME == "V6"),
      has_v7 = any(VISITNAME == "V7"),
      has_v8 = any(VISITNAME == "V8"),
      has_v9 = any(VISITNAME == "V9"),
      has_v10 = any(VISITNAME == "V10"),
      has_v11 = any(VISITNAME == "V11"),
      has_v12 = any(VISITNAME == "V12"),
      has_v13 = any(VISITNAME == "V13"),
      has_v14 = any(VISITNAME == "V14"),
      v3_date = min(VISDAT[VISITNAME == "V3"]),
      last_visit_date = max(VISDAT),
      .groups = "drop"
    ) %>%
    mutate(
      # Get missing visits before last visit
      missing_visits = pmap_chr(list(
        has_v1, has_v2, has_v3, has_v4, has_v5, has_v6, has_v7,
        has_v8, has_v9, has_v10, has_v11, has_v12, has_v13, has_v14,
        v3_date,
        last_visit_date
      ), function(...) {
        args <- list(...)
        visit_flags <- args[1:14]
        v3_date <- args[[15]]
        last_date <- args[[16]]

        if (is.na(v3_date)) {
          return(NA_character_)
        }

        # Define visit schedule (days from V3)
        visit_schedule <- c(
          V1 = -14, V2 = -7, V3 = 0,
          V4 = 14, V5 = 30, V6 = 60, V7 = 90,
          V8 = 104, V9 = 120, V10 = 150, V11 = 180,
          V12 = 210, V13 = 240, V14 = 270
        )

        # Get missing visits that should have occurred
        missing <- character()
        for (i in seq_along(visit_flags)) {
          visit_name <- names(visit_schedule)[i]
          target_date <- v3_date + visit_schedule[i]
          if (!visit_flags[[i]] && target_date <= last_date) {
            missing <- c(missing, visit_name)
          }
        }

        if (length(missing) > 0) {
          sprintf("缺失以下访视：%s", paste(missing, collapse = "、"))
        } else {
          NA_character_
        }
      })
    )

  # Get deviations
  deviations <- subject_visits

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "未按计划进行访视"
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
  cat("8.2 按计划进行现场访视\n")
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
        "受试者%s：%s",
        row["SUBJID"],
        row["CRITERION"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
