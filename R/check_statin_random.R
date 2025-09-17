#' Check prior statin therapy random criteria
#'
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when left_join if_else bind_rows
#' @export
check_statin_random <- function(data) {
  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame()
  )

  # Validate required datasets
  required_datasets <- c("EX1", "EX2", "STATIN", "RAND")
  missing_datasets <- setdiff(required_datasets, names(data))
  if (length(missing_datasets) > 0) {
    stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
  }

  # Get randomization information
  rand_subjects <- data$RAND %>%
    select(SUBJID, RANDID)

  # Get subjects with prior statin use from STATIN dataset
  statin_subjects <- data$STATIN %>%
    inner_join(rand_subjects, by = "SUBJID") %>%
    filter(
      !is_sas_na(STATINYN),
      STATINYN %in% c(
        "未应用本方案中规定的稳定背景降脂治疗的受试者（进入导入期）",
        "既往使用他汀种类与本方案规定的他汀和剂量一致且治疗稳定，但治疗持续时间不足4周的，需要补足至≥4周（缩短导入期）"
      )
    ) %>%
    select(SUBJID, STATINYN)

  # Return early if no subjects meet initial criteria
  if (nrow(statin_subjects) == 0) {
    class(results) <- c("statin_check", "list")
    return(results)
  }

  # Process 阿托伐他汀 data from EX1
  atorvastatin_usage <- if (nrow(data$EX1) > 0) {
    data$EX1 %>%
      inner_join(rand_subjects, by = "SUBJID") %>%
      mutate(
        statin_name = "阿托伐他汀",
        dose_value = suppressWarnings(as.numeric(EXDOS)),
        EXSTDAT = as.Date(EXSTDAT),
        EXENDAT = as.Date(EXENDAT),
        EXTRT = "阿托伐他汀"
      ) %>%
      filter(!is.na(dose_value)) # Remove rows where dose conversion failed
  } else {
    data.frame() # Return empty data frame if EX1 is empty
  }

  # Process 瑞舒伐他汀 data from EX2
  rosuvastatin_usage <- if (nrow(data$EX2) > 0) {
    data$EX2 %>%
      inner_join(rand_subjects, by = "SUBJID") %>%
      mutate(
        statin_name = "瑞舒伐他汀",
        dose_value = suppressWarnings(as.numeric(EXDOS)),
        EXSTDAT = as.Date(EXSTDAT),
        EXENDAT = as.Date(EXENDAT),
        EXTRT = "瑞舒伐他汀"
      ) %>%
      filter(!is.na(dose_value)) # Remove rows where dose conversion failed
  } else {
    data.frame() # Return empty data frame if EX2 is empty
  }

  # Combine both statin datasets
  statin_usage <- bind_rows(atorvastatin_usage, rosuvastatin_usage)

  # Return early if no statin usage data
  if (nrow(statin_usage) == 0) {
    class(results) <- c("statin_check", "list")
    return(results)
  }

  statin_usage <- statin_usage %>%
    right_join(statin_subjects, by = "SUBJID") %>%
    mutate(
      # Calculate treatment duration in weeks
      duration_days = case_when(
        !is.na(EXSTDAT) & !is.na(EXENDAT) ~ as.numeric(difftime(EXENDAT, EXSTDAT, units = "days")),
        TRUE ~ NA_real_
      ),
      duration_weeks = duration_days / 7,

      # Check dose criteria
      meets_dose_criteria = case_when(
        is.na(dose_value) ~ FALSE,
        statin_name == "阿托伐他汀" ~ dose_value >= 10,
        statin_name == "瑞舒伐他汀" ~ dose_value >= 5,
        TRUE ~ FALSE
      ),

      # Check duration criteria
      meets_duration_criteria = case_when(
        is.na(duration_weeks) ~ FALSE,
        STATINYN == "未应用本方案中规定的稳定背景降脂治疗的受试者（进入导入期）" ~ duration_weeks >= 4,
        TRUE ~ !is.na(duration_weeks)
      ),

      # Combine criteria
      meets_all_criteria = meets_dose_criteria & meets_duration_criteria,

      # Create criterion description with safe handling of NA values
      CRITERION = case_when(
        !meets_all_criteria & !is.na(dose_value) & !is.na(duration_weeks) ~
          sprintf(
            "%s %.1fmg，持续%.1f周",
            EXTRT, dose_value, duration_weeks
          ),
        !meets_all_criteria ~
          sprintf("%s 剂量或持续时间记录不完整", EXTRT),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(CRITERION))

  # Get deviations
  deviations <- statin_usage %>%
    select(
      SUBJID, EXTRT, dose_value, EXSTDAT, EXENDAT, CRITERION,
      meets_duration_criteria, meets_dose_criteria
    )

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- paste(
      "不符合随机前已接受本方案规定的他汀（阿托伐他汀钙片≥10 mg qd，或瑞舒伐他汀钙片≥5 mg qd），伴或不伴其他降脂药，均稳定种类和剂量治疗≥4周的任意一项却随机。",
      sep = "\n"
    )
    results$details <- deviations
  }

  class(results) <- c("statin_check", "list")
  return(results)
}

#' Print method for statin therapy check results
#' @param x Object of class statin_check
#' @param ... Additional arguments
#' @export
print.statin_check <- function(x, ...) {
  cat("5.3不符合随机标准却随机\n")
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
        "受试者%s服用%s，用药时间：%s至%s，%s",
        row["SUBJID"],
        row["EXTRT"],
        row["EXSTDAT"],
        row["EXENDAT"],
        row["CRITERION"]
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
