#' Check eGFR requirements
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate
#' @export
check_gfr_requirement <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )

     # Validate required datasets
    required_datasets <- c("GFR", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }

    rand_subjects <- data$RAND %>%
        select(SUBJID, RANDID)
    
    # Get baseline eGFR values
    baseline_gfr <- data$GFR %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        filter(VISITNAME == "筛选期V1") %>%
        select(SUBJID, GFR) %>%
        filter(!is_sas_na(GFR)) %>%
        mutate(
            GFR_VALUE = as.numeric(GFR),
        )
    
    # Check eGFR requirement (> 30 ml/min/1.73 m2)
    deviations <- baseline_gfr %>%
        filter(GFR_VALUE <= 30)
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- "不符合入组标准6，筛选时肾小球滤过率估计值（eGFR）≤30 ml/min/1.73 m2，或未采用CKD-EPI公式。"
        results$details <- deviations
    }
    
    class(results) <- c("gfr_check", "list")
    return(results)
}

#' Print method for eGFR requirement check results
#' @param x Object of class gfr_check
#' @param ... Additional arguments
#' @export
print.gfr_check <- function(x, ...) {
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
            sprintf("受试者%s的肾小球滤过率为%s, 单位为：%s",
                    row["SUBJID"],
                    row["GFR_VALUE"],
                    row["GFR_UNIT"])
        })
        cat(formatted_details, sep = "\n")
    }
}

