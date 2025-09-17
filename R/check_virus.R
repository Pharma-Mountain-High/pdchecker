#' Check viral infection exclusion criteria
#' 
#' @param data List of data frames containing study data
#' @return List containing check results and descriptions
#' @importFrom dplyr filter select mutate case_when bind_rows
#' @export
check_virus_exclusion <- function(data) {
    # Initialize results
    results <- list(
        has_deviation = FALSE,
        messages = character(),
        details = data.frame()
    )
    
    # Validate required datasets
    required_datasets <- c("VIR", "HBV", "HCV", "RAND")
    missing_datasets <- setdiff(required_datasets, names(data))
    if (length(missing_datasets) > 0) {
        stop("Missing required datasets: ", paste(missing_datasets, collapse = ", "))
    }
    
    # Get randomization information
    rand_subjects <- data$RAND %>%
        select(SUBJID, RANDID)

    # Get viral test results at screening from VIR dataset
    virus_tests <- data$VIR %>%
        inner_join(rand_subjects, by = "SUBJID") %>%
        filter(
            VISITNAME == "筛选期V1",
            VIRTEST %in% c(
                "乙肝表面抗原（HBsAg）",
                "丙型肝炎病毒抗体",
                "人类免疫缺陷病毒抗体",
                "梅毒（特异性抗体）"
            )
        ) %>%
        filter(!is_sas_na(VIRRES)) %>%
        mutate(
            CRITERION = case_when(
                VIRTEST == "乙肝表面抗原（HBsAg）" & VIRRES == "阳性" ~ 
                    "HBsAg阳性",
                VIRTEST == "丙型肝炎病毒抗体" & VIRRES == "阳性" ~ 
                    "HCV-Ab阳性",
                VIRTEST == "人类免疫缺陷病毒抗体" & VIRRES == "阳性" ~ 
                    "Anti-HIV阳性",
                VIRTEST == "梅毒（特异性抗体）" & VIRRES == "阳性" ~ 
                    "TP阳性",
                TRUE ~ NA_character_
            )
        )
    
    # Get HBV-DNA results from HBV dataset
    hbv_dna_tests <- data$HBV %>%
        filter(
            VISITNAME == "筛选期V1",
            HBVPERF == 2,
            !is_sas_na(HBVRES)
        ) %>%
        mutate(
            DNA_VALUE = as.numeric(HBVRES),
            DNA_UNIT = HBVUNT,
            VIRTEST = "HBV-DNA",
            VIRRES = HBVRES,
            CRITERION = if_else(
                DNA_VALUE >= 1000,
                sprintf("HBV-DNA滴度为%s %s", HBVRES, HBVUNT),
                NA_character_
            )
        ) %>%
        select(SUBJID, VIRTEST, VIRRES, CRITERION)
    
    # Get HCV-RNA results from HCV dataset
    hcv_rna_tests <- data$HCV %>%
        filter(
            VISITNAME == "筛选期V1",
            HCVPERF == 2,
            !is_sas_na(HCVRES)
        ) %>%
        mutate(
            VIRTEST = "HCV-RNA",
            VIRRES = HCVRES,
            CRITERION = if_else(
                HCVRES == "阳性",
                "HCV-RNA阳性",
                NA_character_
            )
        ) %>%
        select(SUBJID, VIRTEST, VIRRES, CRITERION)
    
    # Combine all viral test results
    all_tests <- bind_rows(
        virus_tests %>% select(SUBJID, VIRTEST, VIRRES, CRITERION),
        hbv_dna_tests,
        hcv_rna_tests
    ) %>%
    filter(!is.na(CRITERION))
    
    # Check for HBV coinfection (HBsAg positive and HBV-DNA ≥ 1000)
    hbv_subjects <- all_tests %>%
        filter(VIRTEST %in% c("乙肝表面抗原（HBsAg）", "HBV-DNA")) %>%
        group_by(SUBJID) %>%
        summarise(
            has_hbsag = any(VIRTEST == "乙肝表面抗原（HBsAg）" & CRITERION == "HBsAg阳性"),
            has_hbv_dna = any(VIRTEST == "HBV-DNA" & !is.na(CRITERION)),
            .groups = "drop"
        ) %>%
        filter(has_hbsag & has_hbv_dna)
    
    # Check for HCV coinfection (both Ab and RNA positive)
    hcv_subjects <- all_tests %>%
        filter(VIRTEST %in% c("丙型肝炎病毒抗体", "HCV-RNA")) %>%
        group_by(SUBJID) %>%
        summarise(
            has_both = all(c("HCV-Ab阳性", "HCV-RNA阳性") %in% CRITERION),
            .groups = "drop"
        ) %>%
        filter(has_both)
    
    # Get final deviations
    deviations <- all_tests %>%
        filter(
            SUBJID %in% hbv_subjects$SUBJID |
            SUBJID %in% hcv_subjects$SUBJID |
            CRITERION == "Anti-HIV阳性" |
            CRITERION == "TP阳性"
        )
    
    # Compile results
    if (nrow(deviations) > 0) {
        results$has_deviation <- TRUE
        results$messages <- paste(
            "符合排除标准4.2，在筛选期出现以下病毒学检查结果：",
            "1) HBsAg阳性且检测HBV-DNA滴度≥1×10^3拷贝/ml；",
            "2) 丙型肝炎病毒抗体（HCV-Ab）阳性，且丙型肝炎病毒核糖核酸（HCV-RNA）阳性；",
            "3) 人免疫缺陷病毒抗体（Anti-HIV）阳性；",
            "4) 梅毒特异性抗体阳性。",
            sep = "\n"
        )
        results$details <- deviations
    }
    
    class(results) <- c("virus_check", "list")
    return(results)
}

#' Print method for viral infection check results
#' @param x Object of class virus_check
#' @param ... Additional arguments
#' @export
print.virus_check <- function(x, ...) {
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
            sprintf("受试者%s的%s检测结果为%s",
                    row["SUBJID"],
                    row["VIRTEST"],
                    row["VIRRES"])
        })
        cat(formatted_details, sep = "\n")
    }
} 