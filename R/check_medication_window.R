#' Check study medication administration time window
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when group_by summarise ungroup rename
#' @export
check_medication_window <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Validate required datasets
    required_datasets <- c("EX", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }

    # Get randomization information
    rand_subjects <- data$RAND %>%
        select(SUBJID, RANDID)
    
    # Get medication administration records
    med_admin <- data$EX %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        filter(!is_sas_na(EXSTDAT)) %>%
        # Check if subject has all required administrations
        group_by(SUBJID) %>%
        mutate(
            EXSTDAT = as.Date(EXSTDAT),
            # Calculate study day
            STUDY_DAY = as.numeric(difftime(EXSTDAT, min(EXSTDAT, na.rm = TRUE), units = "days")) + 1,
            # Check if administration is within window
            WINDOW = case_when(
                STUDY_DAY == 1 ~ "第1天",
                STUDY_DAY >= 87 & STUDY_DAY <= 93 ~ "第90天±3天",
                TRUE ~ "超出给药时间窗"
            ),
            # Create criterion description
            CRITERION = case_when(
                WINDOW == "超出给药时间窗" ~ 
                    sprintf("给药时间为第%d天，超出方案规定时间窗", STUDY_DAY),
                TRUE ~ NA_character_
            ),
            has_day1 = any(WINDOW == "第1天"),
            has_day90 = any(WINDOW == "第90天±3天"),
            missing_admin = case_when(
                !has_day1 & !has_day90 ~ "缺失第1天和第90天给药",
                !has_day1 ~ "缺失第1天给药",
                !has_day90 ~ "缺失第90天给药",
                TRUE ~ NA_character_
            )
        ) %>%
        ungroup()
    
    # Get deviations for wrong timing
    timing_deviations <- med_admin %>%
        filter(!is.na(CRITERION)) %>%
        select(SUBJID, EXSTDAT, STUDY_DAY, CRITERION)
    
    # Get deviations for missing administrations
    missing_deviations <- med_admin %>%
        filter(!is.na(missing_admin)) %>%
        select(SUBJID, missing_admin) %>%
        distinct() %>%
        rename(CRITERION = missing_admin)
    
    # Combine all deviations
    deviations <- bind_rows(
        timing_deviations,
        missing_deviations
    )
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- "受试者未按时用药或受试者超过时间窗要求用药"
        results$details <- deviations
    }
    
    class(results) <- c("medication_window_check", "list")
    return(results)
}

#' Print method for medication window check results
#' @param x Object of class medication_window_check
#' @param ... Additional arguments
#' @export
print.medication_window_check <- function(x, ...) {
    cat("6.3 受试者未按时用药\n")
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
            if (!is.na(row["STUDY_DAY"])) {
                sprintf("受试者%s：%s（给药日期：%s）",
                        row["SUBJID"],
                        row["CRITERION"],
                        row["EXSTDAT"])
            } else {
                sprintf("受试者%s：%s",
                        row["SUBJID"],
                        row["CRITERION"])
            }
        })
        cat(formatted_details, sep = "\n")
    }
}

