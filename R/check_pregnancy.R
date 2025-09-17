#' Check pregnancy test exclusion criteria
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate
#' @export
check_pregnancy_exclusion <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Validate required datasets
    required_datasets <- c("RP", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }

    # Get randomization information
    rand_subjects <- data$RAND %>%
        select(SUBJID, RANDID)
    
    # Get pregnancy test results at screening
    pregnancy_tests <- data$RP %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        filter(
            VISITNAME == "筛选期V1",
            RPTYP == "血妊娠"
        ) %>%
        filter(!is_sas_na(RPRES)) %>%
        mutate(
            CRITERION = if_else(
                RPRES == "阳性",
                "血妊娠试验阳性",
                NA_character_
            )
        ) %>%
        filter(!is.na(CRITERION))
    
    # Get deviations
    deviations <- pregnancy_tests %>%
        select(SUBJID, RPTYP, RPRES, CRITERION)
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- "符合排除标准4.5，血妊娠试验阳性"
        results$details <- deviations
    }
    
    class(results) <- c("pregnancy_check", "list")
    return(results)
}

#' Print method for pregnancy test check results
#' @param x Object of class pregnancy_check
#' @param ... Additional arguments
#' @export
print.pregnancy_check <- function(x, ...) {
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
            sprintf("受试者%s的%s结果为%s",
                    row["SUBJID"],
                    row["RPTYP"],
                    row["RPRES"])
        })
        cat(formatted_details, sep = "\n")
    }
} 