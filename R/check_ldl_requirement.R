#' Check LDL-C requirements
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select left_join mutate case_when row_number n
#' @export
check_ldl_requirement <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Validate required datasets
    required_datasets <- c("LB", "SF", "MH", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }

    rand_subjects <- data$RAND %>%
        select(SUBJID, RANDID)
    
    first_dose_date <- data$EX %>%
        group_by(SUBJID) %>%
        summarise(first_dose_date = min(EXSTDAT))

    # Get baseline LDL-C values
    baseline_ldl <- data$LB %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        left_join(first_dose_date, by = "SUBJID") %>%
        filter(LBTEST == "低密度脂蛋白胆固醇" & LBDAT <= first_dose_date) %>%
        arrange(SUBJID, LBDAT) %>%
        group_by(SUBJID) %>%
        filter(row_number() == n()) %>%
        select(SUBJID, LBTEST, LBORRES) %>%
        mutate(LDL_VALUE = as.numeric(LBORRES))
    
    # Get background therapy information
    background_therapy <- data$SF %>%
        select(SUBJID, RANDLDL, RANDTT)
    
    # Get disease history
    disease_history <- data$MH %>%
        group_by(SUBJID) %>%
        summarise(has_ascvd = any(grepl("ASC|动脉粥样硬化|缺血性心肌病|冠心病|冠状动脉血运重建术后|缺血性卒中|短暂性脑缺血发作|外周动脉疾病|血运重建术|PAD|TIA", MHTERM, ignore.case = TRUE))) %>%
        ungroup()
   
    # Combine data for checking
    check_data <- baseline_ldl %>%
        left_join(background_therapy, by = "SUBJID") %>%
        left_join(disease_history, by = "SUBJID")
    
    # Check conditions
    deviations <- check_data %>%
        mutate(
            deviation_reason = case_when(
                # Condition 1: On statin therapy, LDL-C ≥ 2.6 mmol/L
                RANDTT == "是" & LDL_VALUE < 2.6  & !has_ascvd ~ 
                    "接受他汀类药物治疗的受试者基线LDL-C水平未达到≥2.6 mmol/L",
                
                # Condition 2: Not on statin therapy, LDL-C ≥ 3.4 mmol/L
                RANDTT == "否" & LDL_VALUE < 3.4 ~ 
                    "未接受他汀类药物治疗的受试者基线LDL-C水平未达到≥3.4 mmol/L",
                
                # Condition 3: With ASCVD, LDL-C ≥ 1.8 mmol/L
                has_ascvd & LDL_VALUE < 1.8 ~ 
                    "有ASCVD病史的受试者基线LDL-C水平未达到≥1.8 mmol/L",
                
                TRUE ~ NA_character_
            )
        ) %>%
        filter(!is.na(deviation_reason)) %>%
        select(SUBJID, LDL_VALUE, RANDTT, has_ascvd, deviation_reason) %>%
        distinct()
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- "存在不符合LDL-C入组标准的受试者"
        results$details <- deviations
    }
    
    class(results) <- c("ldl_check", "list")
    return(results)
}

#' Print method for LDL-C requirement check results
#' @param x Object of class ldl_check
#' @param ... Additional arguments
#' @export
print.ldl_check <- function(x, ...) {
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
            sprintf("受试者%s的基线LDL-C水平为%s mmol/L，%s",
                    row["SUBJID"],
                    row["LDL_VALUE"],
                    row["deviation_reason"])
        })
        cat(formatted_details, sep = "\n")
    }
}

