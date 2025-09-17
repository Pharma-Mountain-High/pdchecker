#' Check for screened subjects without informed consent
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr anti_join select distinct
#' @export
check_screen_without_ic <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Validate required datasets
    required_datasets <- c("SCR", "IC", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }
    
    # Get unique subjects from SCR dataset
    scr_subjects <- data$SCR %>%
        distinct(SUBJID)
    
    # Get unique subjects from IC dataset
    ic_subjects <- data$IC %>%
        distinct(SUBJID)
    
    # Find subjects in SCR but not in IC
    missing_ic <- scr_subjects %>%
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
            sprintf("受试者%s在未签署知情同意书的情况下进行了筛选。",
                    row["SUBJID"])
        })
        cat(formatted_details, sep = "\n")
    }
}
