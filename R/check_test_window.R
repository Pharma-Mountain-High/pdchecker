#' Check if tests were performed within visit windows
#' 
#' @param test_data Data frame containing test data
#' @param dataset_name Name of the dataset (e.g., "LB", "VS", "HT")
#' @return List containing check results and descriptions
#' @importFrom dplyr slice_tail case_when group_by summarise ungroup semi_join bind_rows first sym
#' @export
check_test_window <- function(test_data, dataset_name) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Define planned visits and their windows
    planned_visits <- list(
        "V4" = list(target = 14, window = 2),
        "V5" = list(target = 30, window = 3),
        "V6" = list(target = 60, window = 3),
        "V7" = list(target = 90, window = 3),
        "V8" = list(target = 104, window = 3),
        "V9" = list(target = 120, window = 3),
        "V10" = list(target = 150, window = 3),
        "V11" = list(target = 180, window = 3),
        "V12" = list(target = 210, window = 3),
        "V13" = list(target = 240, window = 3),
        "V14" = list(target = 270, window = 3)
    )
    
    # Get date column name
    date_col <- paste0(dataset_name, "DAT")
    
    # Get V3 dates for each subject
    v3_dates <- test_data %>%
        filter(VISITNAME == "V3" & !is_sas_na(!!sym(date_col))) %>%
        mutate(start_date = as.Date(!!sym(date_col))) %>%
        select(SUBJID, start_date, VISITNAME) %>%
        distinct()
    
    # Get all tests and find last visit for each subject
    test_records <- test_data %>%
        filter(!is_sas_na(!!sym(date_col))) %>%
        mutate(
            test_date = as.Date(!!sym(date_col)),
            VISITNAME = if_else(is_sas_na(VISITNAME), NA_character_, VISITNAME)
        )
    
    # Get last visit for each subject
    last_visits <- test_records %>%
        filter(!is.na(VISITNAME)) %>%
        group_by(SUBJID) %>%
        arrange(test_date, .by_group = TRUE) %>%
        slice_tail(n = 1) %>%
        select(SUBJID, last_visit = VISITNAME, last_date = test_date) %>%
        ungroup()
    
    # Check each planned visit for each subject
    window_checks <- lapply(names(planned_visits), function(visit) {
        visit_info <- planned_visits[[visit]]
        target_day <- visit_info$target
        window <- visit_info$window
        
        # Check window compliance for existing visits
        visit_windows <- v3_dates %>%
            # Only check subjects that have the visit
            semi_join(
                test_records %>% 
                    filter(VISITNAME == visit) %>% 
                    select(SUBJID) %>% 
                    distinct(),
                by = "SUBJID"
            ) %>%
            # Join with last visit info to get last_date
            left_join(last_visits, by = "SUBJID") %>%
            # Only check visits before last date
            filter(start_date <= last_date) %>%
            mutate(
                target_date = start_date + target_day - 1,
                window_start = target_date - window,
                window_end = target_date + window
            )
        
        # Check if test is within window
        window_deviations <- visit_windows %>%
            left_join(
                test_records %>% 
                    filter(
                        VISITNAME == visit,
                        !is_sas_na(!!sym(date_col))
                    ),
                by = "SUBJID"
            ) %>%
            group_by(SUBJID, window_start, window_end) %>%
            summarise(
                in_window = any(!is.na(test_date) & test_date >= first(window_start) & test_date <= first(window_end)),
                .groups = "drop"
            ) %>%
            filter(!in_window) %>%
            mutate(
                VISITNAME = visit,
                CRITERION = sprintf(
                    "第%d±%d天的检查未在规定时间窗内完成（%s至%s）",
                    target_day, window,
                    format(window_start, "%Y-%m-%d"),
                    format(window_end, "%Y-%m-%d")
                )
            ) %>%
            select(SUBJID, VISITNAME, CRITERION)
        
        window_deviations
    }) %>%
        bind_rows()
    
    # Get deviations
    deviations <- window_checks %>%
        arrange(SUBJID, VISITNAME)
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- sprintf("%s检查未在规定时间窗内完成", dataset_name)
        results$details <- deviations
    }
    
    class(results) <- c("test_window_check", "list")
    return(results)
}

#' Print method for missing test check results
#' @param x Object of class missing_test_check
#' @param ... Additional arguments
#' @export
print.test_window_check <- function(x, ...) {
    cat("8.1 未做或缺失；收集记录不符合方案规定\n")
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
            sprintf("受试者%s：%s",
                    row["SUBJID"],
                    row["CRITERION"])
        })
        cat(formatted_details, sep = "\n")
    }
}
