#' Check dose and injection site requirements
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when group_by summarise ungroup left_join n_distinct
#' @export
check_dose_site_requirement <- function(data) {
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
    
    # Get dose and site information
    dose_sites <- data$EX %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        filter(!is_sas_na(EXADOS)) %>%
        rowwise() %>%
        mutate(
            total_dose = EXADOS,
            required_sites = case_when(
                total_dose == "100mg" ~ 1,
                total_dose == "300mg" ~ 2,
                total_dose == "500mg" ~ 3,
                TRUE ~ NA_integer_
            ),
            site_count = length(strsplit(EXSITE, ",")[[1]]),
            sites = EXSITE,
            # Check if meets requirements
            meets_requirements = !is.na(required_sites) & site_count >= required_sites,
            # Create criterion description
            CRITERION = case_when(
                !meets_requirements ~ sprintf(
                    "总剂量%s，需要至少%d个给药部位，实际使用%d个部位（%s）",
                    total_dose,
                    required_sites,
                    site_count,
                    sites
                ),
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(CRITERION))
    
    # Get deviations
    deviations <- dose_sites %>%
        select(SUBJID, VISITNAME, total_dose, required_sites, site_count, sites, CRITERION)
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- "药物剂量出现错误"
        results$details <- deviations
    }
    
    class(results) <- c("dose_site_check", "list")
    return(results)
}

#' Print method for dose and site check results
#' @param x Object of class dose_site_check
#' @param ... Additional arguments
#' @export
print.dose_site_check <- function(x, ...) {
    cat("6.4 药物用量出现错误\n")
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
            sprintf("受试者%s访视%s：%s",
                    row["SUBJID"],
                    row["VISITNAME"],
                    row["CRITERION"])
        })
        cat(formatted_details, sep = "\n")
    }
}



