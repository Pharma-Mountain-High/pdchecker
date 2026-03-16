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
#' # Step 1: Read config file once (if using config)
#' testconfig <- read_testconfig_file("test_config.xlsx")
#'
#' # Step 2: Prepare test data (can reuse testconfig multiple times)
#' lb_prepared <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   config = testconfig
#' )
#'
#' eg_prepared <- prepare_test_data(
#'   data = list(EG = eg_data, SV = sv_data),
#'   test_dataset = "EG",
#'   config = testconfig
#' )
#'
#' # Step 3: Check missing
#' result <- check_missing_test(
#'   data = lb_prepared,
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
#' @param config Data frame, test-visit configuration (required, default: NULL).
#'   If NULL, the function will look for a variable named \code{testconfig} in the
#'   calling environment; if not found, an error is raised.
#'   Must be a data frame, typically created by \code{\link{read_testconfig_file}}
#'   which expands VISITNUM to one per row.
#'   Config must contain: TESTCAT (test category), VISITNUM (visit number, one per row).
#'   Generates expected visit-test combinations as skeleton,
#'   showing empty records (TESTDAT=NA) even when original data has no records.
#'   Use \code{\link{read_testconfig_file}} to read config from Excel/CSV files
#' @param config_cat Character vector, test categories to filter from config (default: NULL).
#'   If NULL, uses all TESTCAT in config;
#'   If provided (e.g., c("CBC", "Chemistry")), only uses those TESTCAT records
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

  # Try to get config from caller's environment if NULL
  if (is.null(config)) {
    if (exists("testconfig", envir = parent.frame(), inherits = TRUE)) {
      config <- get("testconfig", envir = parent.frame(), inherits = TRUE)
    } else {
      stop(
        "'config' is required. Use read_testconfig_file() to create testconfig, ",
        "or pass it explicitly via the config parameter."
      )
    }
  }

  # Validate config type
  if (!is.data.frame(config)) {
    stop("'config' must be a data frame. Use read_testconfig_file() to read from Excel/CSV files.")
  }

  # Validate config columns
  if (!"TESTCAT" %in% names(config) || !"VISITNUM" %in% names(config)) {
    stop("Config must contain TESTCAT and VISITNUM columns")
  }

  # Ensure VISITNUM is character
  config_expanded <- config
  config_expanded$VISITNUM <- as.character(config_expanded$VISITNUM)

  # Filter by config_cat
  if (!is.null(config_cat) && length(config_cat) > 0) {
    config_expanded <- config_expanded %>%
      filter(TESTCAT %in% config_cat)

    if (nrow(config_expanded) == 0) {
      warning("No matching TESTCAT found after filtering by config_cat")
    }
  }

  # Generate skeleton
  # Remove VISIT from config if exists (VISIT should come from sv_data_subset)
  if ("VISIT" %in% names(config_expanded)) {
    config_for_join <- config_expanded %>%
      select(-VISIT)
  } else {
    config_for_join <- config_expanded
  }

  skeleton <- sv_data_subset %>%
    inner_join(config_for_join, by = "VISITNUM") %>%
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
