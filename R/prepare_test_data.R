#' Prepare Test Data for Missing Test Checks
#'
#' @description
#' Extract specified test dataset from a list of all data, merge with visit dataset,
#' and create standardized column names (TBNAME, TESTCAT, TESTDE, TESTDAT, TESTYN, ORRES)
#' for subsequent missing test checks.
#'
#' @details
#' ## Usage Workflow
#'
#' This function is used for data preparation before \code{\link{check_missing_test}}:
#'
#' ```r
#' # Step 1: Prepare test data
#' prepared_data <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB"
#' )
#'
#' # Step 2: Check missing
#' result <- check_missing_test(
#'   data = prepared_data,
#'   sv_data = sv_data,
#'   test_var = "LBTEST",
#'   test = "CBC"
#' )
#' ```
#'
#' ## Data Processing Logic
#'
#' 1. Extract specified test dataset (e.g., "LB") from data list
#' 2. Extract visit dataset (default "SV") from data list, keeping only:
#'    SUBJID, VISIT, VISITNUM, SVDAT
#' 3. Left join test dataset to visit dataset by SUBJID, VISIT, VISITNUM
#' 4. Create standardized column names:
#'    - TBNAME: Dataset name (mapped from original column or uses test_dataset)
#'    - TESTCAT: Test category (mapped from original category column)
#'    - TESTDE: Test name (mapped from original test column), e.g., "RBC Count"
#'    - TESTDAT: Test date (mapped from original date column)
#'    - TESTYN: Test performed flag (mapped from original flag column)
#'    - ORRES: Test result (mapped from original result column)
#' 5. Return dataset with original test columns and derived columns in order:
#'    SUBJID, VISIT, VISITNUM, TBNAME, TESTCAT, TESTDE, TESTYN, TESTDAT, ORRES, other original columns
#'
#' ## Notes
#'
#' - If a mapped column doesn't exist (e.g., test_yn_var), the derived column will be NA
#' - Left join is based on visit dataset, so records follow visit records
#' - SV dataset only keeps SUBJID, VISIT, VISITNUM, SVDAT; other columns excluded
#' - All original columns from test dataset are preserved
#' - Derived columns are placed first for easy viewing
#'
#' @param data List containing all clinical trial datasets
#' @param test_dataset Character string, test dataset name to extract (e.g., "LB")
#' @param test_date_var Character string, original test date variable (default: "LBDAT")
#' @param test_yn_var Character string, original test performed variable (default: "YN")
#' @param test_result_var Character string, original test result variable (default: "ORRES")
#' @param test_cat_var Character string, original test category variable (default: "LBCAT").
#'   If NULL or empty, TESTCAT column will be NA
#' @param test_de_var Character string, original test name variable (default: NULL).
#'   If NULL or empty, TESTDE column will be NA. Used for specific test names
#' @param tb_name_var Character string, original dataset name variable (default: NULL).
#'   If NULL or empty, TBNAME column uses test_dataset value (e.g., "LB")
#' @param sv_dataset Character string, visit dataset name (default: "SV")
#' @param sv_visit_var Character string, visit name variable in visit dataset (default: "VISIT")
#' @param sv_visitnum_var Character string, visit number variable in visit dataset (default: "VISITNUM")
#' @param sv_date_var Character string, visit date variable in visit dataset (default: "SVDAT")
#' @param config Character string or Data frame, test-visit configuration (default: NULL).
#'   If string, represents Excel config file path; if data frame, used directly.
#'   Config must contain: TESTCAT (test category), VISITNUM (comma-separated visit numbers).
#'   If provided, generates expected visit-test combinations as skeleton,
#'   showing empty records (TESTDAT=NA) even when original data has no records
#' @param config_cat Character vector, test categories to filter from config (default: NULL).
#'   If NULL, uses all TESTCAT in config;
#'   If provided (e.g., c("CBC", "Chemistry")), only uses those TESTCAT records.
#'   Note: Only effective when config parameter is provided
#' @param filter_cond Character string, subject filter condition (default: NULL).
#'   multiple conditions separated by semicolons (intersection/AND logic):
#'   - "ENROL|ENRYN=='Y'" - Filter enrolled subjects
#'   - "SUBJECT|SEX=='M'" - Filter males from SUBJECT dataset
#'   - "SUBJECT|AGE>=18" - Filter age >= 18 from SUBJECT
#'   - "ENROL|ENRYN=='Y';SUBJECT|SEX=='M'" - Enrolled males (intersection)
#'   If NULL, no subject filtering is applied
#'
#' @return Data frame with column order:
#'   \describe{
#'     \item{SUBJID}{Subject ID (from SV dataset)}
#'     \item{VISIT}{Visit name (from SV dataset)}
#'     \item{VISITNUM}{Visit number (from SV dataset)}
#'     \item{SVDAT}{Visit date (from SV dataset)}
#'     \item{TBNAME}{Dataset name (derived, mapped from tb_name_var or test_dataset)}
#'     \item{TESTCAT}{Test category (derived, mapped from test_cat_var)}
#'     \item{TESTDE}{Test name (derived, mapped from test_de_var)}
#'     \item{TESTYN}{Test performed flag (derived, mapped from test_yn_var)}
#'     \item{TESTDAT}{Test date (derived, mapped from test_date_var)}
#'     \item{ORRES}{Test result (derived, mapped from test_result_var)}
#'     \item{...}{All other original columns from test dataset}
#'   }
#'   Note: SV dataset only keeps SUBJID, VISIT, VISITNUM, SVDAT in output
#'
#' @importFrom dplyr left_join select mutate sym all_of bind_rows rename inner_join filter pull
#' @importFrom magrittr %>%
#' @export
#'
prepare_test_data <- function(data,
                              test_dataset,
                              test_date_var = "LBDAT",
                              test_yn_var = "YN",
                              test_result_var = "ORRES",
                              test_cat_var = "LBCAT",
                              test_de_var = NULL,
                              tb_name_var = NULL,
                              sv_dataset = "SV",
                              sv_visit_var = "VISIT",
                              sv_visitnum_var = "VISITNUM",
                              sv_date_var = "SVDAT",
                              config = NULL,
                              config_cat = NULL,
                              filter_cond = NULL) {
  # ============================================================================
  # Parameter validation
  # ============================================================================

  if (!is.list(data)) {
    stop("'data' must be a list")
  }

  if (missing(test_dataset) || is.null(test_dataset)) {
    stop("'test_dataset' parameter is required")
  }

  if (!test_dataset %in% names(data)) {
    stop(paste0("Test dataset not found in data: ", test_dataset))
  }

  if (!sv_dataset %in% names(data)) {
    stop(paste0("Visit dataset not found in data: ", sv_dataset))
  }

  # ============================================================================
  # Get base datasets
  # ============================================================================

  test_data <- data[[test_dataset]]
  sv_data <- data[[sv_dataset]]

  # ============================================================================
  # Process filter conditions
  # ============================================================================

  final_subjids <- NULL
  if (!is.null(filter_cond) && filter_cond != "") {
    final_subjids <- subj_filter(data, filter_cond)
  }

  # ============================================================================
  # Validate dataset columns
  # ============================================================================

  sv_required_cols <- c("SUBJID", sv_visit_var, sv_visitnum_var, sv_date_var)
  sv_missing_cols <- setdiff(sv_required_cols, names(sv_data))
  if (length(sv_missing_cols) > 0) {
    stop(paste0(
      "Visit dataset '", sv_dataset, "' is missing required columns: ",
      paste(sv_missing_cols, collapse = ", ")
    ))
  }

  test_required_cols <- c("SUBJID", sv_visit_var, sv_visitnum_var)
  test_missing_cols <- setdiff(test_required_cols, names(test_data))
  if (length(test_missing_cols) > 0) {
    stop(paste0(
      "Test dataset '", test_dataset, "' is missing required columns: ",
      paste(test_missing_cols, collapse = ", ")
    ))
  }

  # ============================================================================
  # Prepare visit data
  # ============================================================================

  sv_data_subset <- sv_data %>%
    select(
      SUBJID = SUBJID,
      VISIT = !!sym(sv_visit_var),
      VISITNUM = !!sym(sv_visitnum_var),
      SVDAT = !!sym(sv_date_var)
    ) %>%
    mutate(VISITNUM = as.character(VISITNUM)) %>%
    filter(!is_sas_na(SVDAT))

  # Apply subject filter
  if (!is.null(final_subjids) && length(final_subjids) > 0) {
    sv_data_subset <- sv_data_subset %>%
      filter(SUBJID %in% final_subjids)

    if (nrow(sv_data_subset) == 0) {
      warning("Visit dataset is empty after subject filtering")
    }
  }

  # ============================================================================
  # Process config and merge data
  # ============================================================================

  if (!is.null(config)) {
    # Read config
    if (is.character(config)) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel config files")
      }
      config_df <- readxl::read_excel(config)
    } else if (is.data.frame(config)) {
      config_df <- config
    } else {
      stop("'config' must be a character string (file path) or a data frame")
    }

    # Validate config columns
    if (!"TESTCAT" %in% names(config_df) || !"VISITNUM" %in% names(config_df)) {
      stop("Config must contain TESTCAT and VISITNUM columns")
    }

    # Parse config: expand VISITNUM string to list
    config_list <- lapply(seq_len(nrow(config_df)), function(i) {
      testcat <- config_df$TESTCAT[i]
      visitnum_str <- as.character(config_df$VISITNUM[i])
      visitnums <- as.numeric(unlist(strsplit(visitnum_str, "[,，]")))
      visitnums <- visitnums[!is.na(visitnums)]

      data.frame(
        TESTCAT = testcat,
        VISITNUM = as.character(visitnums),
        stringsAsFactors = FALSE
      )
    })
    config_expanded <- bind_rows(config_list)

    # Filter by config_cat
    if (!is.null(config_cat) && length(config_cat) > 0) {
      config_expanded <- config_expanded %>%
        filter(TESTCAT %in% config_cat)

      if (nrow(config_expanded) == 0) {
        warning("No matching TESTCAT found after filtering by config_cat")
      }
    }

    # Generate skeleton
    skeleton <- sv_data_subset %>%
      inner_join(config_expanded, by = "VISITNUM") %>%
      select(SUBJID, VISIT, VISITNUM, SVDAT, TESTCAT)

    # Standardize test data columns
    test_data_standard <- test_data %>%
      rename(
        VISIT = !!sym(sv_visit_var),
        VISITNUM = !!sym(sv_visitnum_var)
      ) %>%
      mutate(VISITNUM = as.character(VISITNUM))

    # Handle test_cat_var
    if (!is.null(test_cat_var) && test_cat_var != "" && test_cat_var %in% names(test_data_standard)) {
      test_data_standard <- test_data_standard %>%
        rename(TESTCAT_orig = !!sym(test_cat_var))
    } else {
      test_data_standard$TESTCAT_orig <- NA_character_
    }

    # Merge with skeleton
    merged_data <- skeleton %>%
      left_join(
        test_data_standard,
        by = c("SUBJID", "VISIT", "VISITNUM", "TESTCAT" = "TESTCAT_orig")
      )
  } else {
    # Merge without config
    test_data_for_join <- test_data
    test_data_for_join[[sv_visitnum_var]] <- as.character(test_data_for_join[[sv_visitnum_var]])

    merged_data <- sv_data_subset %>%
      left_join(
        test_data_for_join,
        by = c("SUBJID", "VISIT" = sv_visit_var, "VISITNUM" = sv_visitnum_var)
      )
  }

  # ============================================================================
  # Create derived columns
  # ============================================================================

  # TBNAME
  if (!is.null(tb_name_var) && tb_name_var != "" && tb_name_var %in% names(merged_data)) {
    merged_data$TBNAME <- merged_data[[tb_name_var]]
  } else {
    merged_data$TBNAME <- test_dataset
  }

  # TESTCAT
  if (!"TESTCAT" %in% names(merged_data)) {
    if (!is.null(test_cat_var) && test_cat_var != "" && test_cat_var %in% names(merged_data)) {
      merged_data$TESTCAT <- merged_data[[test_cat_var]]
    } else {
      if (!is.null(test_cat_var) && test_cat_var != "" && !test_cat_var %in% names(merged_data)) {
        warning(paste0("Column not found in test dataset: ", test_cat_var, ", TESTCAT will be set to NA"))
      }
      merged_data$TESTCAT <- NA
    }
  }

  # TESTDE
  if (!is.null(test_de_var) && test_de_var != "" && test_de_var %in% names(merged_data)) {
    merged_data$TESTDE <- merged_data[[test_de_var]]
  } else {
    if (!is.null(test_de_var) && test_de_var != "" && !test_de_var %in% names(merged_data)) {
      warning(paste0("Column not found in test dataset: ", test_de_var, ", TESTDE will be set to NA"))
    }
    merged_data$TESTDE <- NA
  }

  # TESTYN
  if (test_yn_var %in% names(merged_data)) {
    merged_data$TESTYN <- merged_data[[test_yn_var]]
  } else {
    warning(paste0("Column not found in test dataset: ", test_yn_var, ", TESTYN will be set to NA"))
    merged_data$TESTYN <- NA
  }

  # TESTDAT
  if (test_date_var %in% names(merged_data)) {
    merged_data$TESTDAT <- merged_data[[test_date_var]]
  } else {
    warning(paste0("Column not found in test dataset: ", test_date_var, ", TESTDAT will be set to NA"))
    merged_data$TESTDAT <- NA
  }

  # ORRES
  if (test_result_var %in% names(merged_data)) {
    if (test_result_var != "ORRES") {
      merged_data$ORRES <- merged_data[[test_result_var]]
    }
  } else {
    warning(paste0("Column not found in test dataset: ", test_result_var, ", ORRES will be set to NA"))
    merged_data$ORRES <- NA
  }

  # ============================================================================
  # Finalize output
  # ============================================================================

  # Fill NA values in TBNAME
  if (any(!is.na(merged_data$TBNAME))) {
    fill_value <- merged_data$TBNAME[!is.na(merged_data$TBNAME)][1]
  } else {
    fill_value <- test_dataset
  }
  merged_data$TBNAME <- ifelse(is.na(merged_data$TBNAME), fill_value, merged_data$TBNAME)

  # Reorder columns
  key_cols <- c("SUBJID", "VISIT", "VISITNUM", "SVDAT")
  derived_cols <- c("TBNAME", "TESTCAT", "TESTDE", "TESTYN", "TESTDAT", "ORRES")
  other_cols <- setdiff(names(merged_data), c(key_cols, derived_cols))

  result <- merged_data %>%
    select(all_of(key_cols), all_of(derived_cols), all_of(other_cols))

  return(result)
}
