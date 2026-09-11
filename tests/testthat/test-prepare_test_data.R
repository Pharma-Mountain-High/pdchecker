# Test prepare_test_data() function
# =============================================================================

# Create test data
# =============================================================================

# Create default config for tests
# Config defines which TESTCAT should be checked at which VISITNUM
# Each row is one TESTCAT-VISITNUM combination
create_default_config <- function() {
  data.frame(
    TESTCAT = c("CBC", "CBC", "CBC", "Chemistry", "Chemistry", "Chemistry"),
    VISITNUM = c("1", "2", "3", "1", "2", "3"),
    stringsAsFactors = FALSE
  )
}

create_test_data <- function() {
  # Visit data
  sv_data <- data.frame(
    SUBJID = c("001", "001", "001", "002", "002", "002", "003", "003", "003"),
    VISIT = c("V1", "V2", "V3", "V1", "V2", "V3", "V1", "V2", "V3"),
    VISITNUM = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    SVDAT = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-01",
      "2024-01-02", "2024-01-16", "2024-02-02",
      "2024-01-03", "2024-01-17", "2024-02-03"
    )),
    stringsAsFactors = FALSE
  )

  # Lab data
  lb_data <- data.frame(
    SUBJID = c("001", "001", "002", "002", "003", "003"),
    VISIT = c("V1", "V2", "V1", "V2", "V1", "V2"),
    VISITNUM = c(1, 2, 1, 2, 1, 2),
    LBCAT = c("CBC", "CBC", "CBC", "Chemistry", "CBC", "CBC"),
    LBTEST = c("WBC", "WBC", "WBC", "Liver", "WBC", "WBC"),
    LBDAT = as.Date(c(
      "2024-01-01", "2024-01-15",
      "2024-01-02", "2024-01-16",
      "2024-01-03", "2024-01-17"
    )),
    ORRES = c("5.5", "6.2", "5.8", "Normal", "6.0", "5.9"),
    YN = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    stringsAsFactors = FALSE
  )

  # Subject data
  subject_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    SEX = c("M", "F", "M"),
    AGE = c(25, 30, 45),
    stringsAsFactors = FALSE
  )

  # Demographics data
  dm_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    AGE = c(25, 30, 45),
    BMI = c(22.5, 28.0, 25.5),
    stringsAsFactors = FALSE
  )

  # Enrollment data
  enrol_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    ENRYN = c("Yes", "Yes", "No"),
    stringsAsFactors = FALSE
  )

  list(
    SV = sv_data,
    LB = lb_data,
    SUBJECT = subject_data,
    DM = dm_data,
    ENROL = enrol_data
  )
}

create_vs_test_data <- function() {
  # Vital signs in long format (one measurement per row)
  vs_data <- data.frame(
    SUBJID = c("001", "001", "002", "002", "003", "003"),
    VISIT = c("V1", "V2", "V1", "V2", "V1", "V2"),
    VISITNUM = c(1, 2, 1, 2, 1, 2),
    TNAME = c("VS", "VS", "VS", "VS", "VS", "VS"),
    VSTEST = c("Height", "Weight", "Height", "Weight", "Height", "Weight"),
    VSDAT = as.Date(c(
      "2024-01-01", "2024-01-15",
      "2024-01-02", "2024-01-16",
      "2024-01-03", "2024-01-17"
    )),
    VSORRES = c("170", "65", "165", "60", "180", "80"),
    VSYN = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
    stringsAsFactors = FALSE
  )

  sv_data <- data.frame(
    SUBJID = c("001", "001", "001", "002", "002", "002", "003", "003", "003"),
    VISIT = c("V1", "V2", "V3", "V1", "V2", "V3", "V1", "V2", "V3"),
    VISITNUM = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    SVDAT = as.Date(c(
      "2024-01-01", "2024-01-15", "2024-02-01",
      "2024-01-02", "2024-01-16", "2024-02-02",
      "2024-01-03", "2024-01-17", "2024-02-03"
    )),
    stringsAsFactors = FALSE
  )

  subject_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    SEX = c("M", "F", "M"),
    AGE = c(25, 30, 45),
    stringsAsFactors = FALSE
  )

  enrol_data <- data.frame(
    SUBJID = c("001", "002", "003"),
    ENRYN = c("Yes", "Yes", "No"),
    stringsAsFactors = FALSE
  )

  list(
    SV = sv_data,
    VSLONG = vs_data,
    SUBJECT = subject_data,
    ENROL = enrol_data
  )
}

create_two_dataset_data <- function() {
  vs <- create_vs_test_data()
  lb <- create_test_data()
  list(
    SV = lb$SV,
    LB = lb$LB,
    VSLONG = vs$VSLONG,
    SUBJECT = lb$SUBJECT,
    DM = lb$DM,
    ENROL = lb$ENROL
  )
}


# =============================================================================
# Test basic functionality
# =============================================================================

test_that("prepare_test_data() basic functionality works", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # Check returned data frame is not empty

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # Check required columns exist
  expected_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME",
    "TESTCAT", "TESTCAT_ORIG", "TESTDE", "TESTYN", "TESTDAT", "TESTTIM", "ORRES"
  )
  expect_true(all(expected_cols %in% names(result)))

  # Check TBNAME is correctly set
  expect_true(all(result$TBNAME == "LB"))

  # Check TESTCAT is correctly mapped
  expect_true("CBC" %in% result$TESTCAT)
})


test_that("prepare_test_data() correctly merges visit and test data", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # With config, row count = subjects × visits × TESTCAT combinations
  # 3 subjects × 3 visits × 2 TESTCAT = 18 rows
  n_subjects <- length(unique(test_data$SV$SUBJID))
  n_visits <- length(unique(config_df$VISITNUM))
  n_testcat <- length(unique(config_df$TESTCAT))
  expected_rows <- n_subjects * n_visits * n_testcat
  expect_equal(nrow(result), expected_rows)

  # Check all subjects and visits are represented
  expect_setequal(unique(result$SUBJID), unique(test_data$SV$SUBJID))
  expect_setequal(unique(result$VISITNUM), unique(config_df$VISITNUM))
})


# =============================================================================
# Test filter_cond parameter (single dataset filter)
# =============================================================================

test_that("filter_cond single dataset filter: filter male subjects", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "SUBJECT|SEX=='M'"
  )

  # Check only male subjects are kept
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "003"))
  expect_false("002" %in% unique_subjids) # 002 is female
})


test_that("filter_cond single dataset filter: filter subjects with age > 30", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "SUBJECT|AGE>30"
  )

  # Check only subjects with age > 30 are kept
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003") # Only 003 has age 45
})


test_that("filter_cond single dataset filter: combined conditions", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "SUBJECT|SEX=='M' & AGE>=25 & AGE<=30"
  )

  # Check only male subjects with age 25-30 are kept
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "001") # Only 001 matches
})


# =============================================================================
# Test filter_cond parameter (multiple dataset filter)
# =============================================================================

test_that("filter_cond multiple dataset filter: intersection of two conditions", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "SUBJECT|SEX=='M';DM|AGE>=30"
  )

  # SUBJECT males: 001, 003
  # DM AGE>=30: 002, 003
  # Intersection: 003
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003")
})


test_that("filter_cond multiple dataset filter: intersection of three conditions", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "SUBJECT|SEX=='M';DM|AGE>=25;DM|BMI<26"
  )

  # SUBJECT males: 001, 003
  # DM AGE>=25: 001, 002, 003
  # DM BMI<26: 001, 003
  # Intersection: 001, 003
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "003"))
})


test_that("filter_cond multiple dataset filter: conditions with whitespace", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = " SUBJECT | SEX=='M' ; DM | AGE>=30 "
  )

  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003")
})


# =============================================================================
# Test enrollment filter via filter_cond
# =============================================================================

test_that("filter_cond can be used for enrollment filtering", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "ENROL|ENRYN=='Yes'"
  )

  # Only 001 and 002 are enrolled (ENRYN="Yes")
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002"))
  expect_false("003" %in% unique_subjids)
})


test_that("filter_cond enrollment and custom filter combined", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = "ENROL|ENRYN=='Yes';SUBJECT|SEX=='M'"
  )

  # Enrolled: 001, 002
  # Males: 001, 003
  # Intersection: 001
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "001")
})


# =============================================================================
# Test config functionality
# =============================================================================

test_that("config generates visit-test skeleton", {
  test_data <- create_test_data()

  # Create config data frame
  config_df <- data.frame(
    TESTCAT = c("CBC", "CBC", "CBC", "Chemistry", "Chemistry"),
    VISITNUM = c("1", "2", "3", "1", "2"),
    stringsAsFactors = FALSE
  )

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # Check returned data contains configured test categories
  expect_true("CBC" %in% result$TESTCAT)
  expect_true("Chemistry" %in% result$TESTCAT)

  # Check CBC has records for all 3 visits
  blood_routine <- result[result$TESTCAT == "CBC" & result$SUBJID == "001", ]
  expect_equal(nrow(blood_routine), 3) # V1, V2, V3
})


test_that("config_cat parameter filters specific test categories", {
  test_data <- create_test_data()

  config_df <- data.frame(
    TESTCAT = c("CBC", "CBC", "Chemistry", "Chemistry", "Urinalysis", "Urinalysis"),
    VISITNUM = c("1", "2", "1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    config_cat = c("CBC", "Chemistry") # Only use these two
  )

  # Check only specified test categories are included
  unique_testcat <- unique(result$TESTCAT)
  expect_true(all(unique_testcat %in% c("CBC", "Chemistry")))
  expect_false("Urinalysis" %in% unique_testcat)
})


# =============================================================================
# Test error handling
# =============================================================================

test_that("filter_cond format error throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  # Format error: missing | separator
  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      config = config_df,
      filter_cond = "SUBJECT SEX=='M'" # Missing |
    ),
    "filter_cond format error"
  )
})


test_that("filter_cond non-existent dataset throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      config = config_df,
      filter_cond = "NONEXIST|SEX=='M'" # Dataset doesn't exist
    ),
    "Dataset specified in filter_cond does not exist"
  )
})


test_that("filter_cond dataset missing SUBJID throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  # Create a dataset without SUBJID
  test_data$NOSUBJID <- data.frame(
    ID = c("001", "002", "003"),
    VALUE = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      config = config_df,
      filter_cond = "NOSUBJID|VALUE>1"
    ),
    "is missing SUBJID column"
  )
})


test_that("filter_cond syntax error throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      config = config_df,
      filter_cond = "SUBJECT|NONEXIST_COL=='M'" # Column doesn't exist
    ),
    "failed"
  )
})


test_that("missing required parameter throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_error(
    prepare_test_data(
      data = test_data,
      config = config_df
      # Missing test_dataset
    ),
    "'test_dataset' parameter is required"
  )
})


test_that("non-existent test_dataset throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "NONEXIST",
      config = config_df
    ),
    "Test dataset not found in data"
  )
})


test_that("non-existent visit dataset throws error", {
  test_data <- create_test_data()
  config_df <- create_default_config()
  test_data$SV <- NULL # Remove visit dataset

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      config = config_df
    ),
    "Visit dataset not found in data"
  )
})


test_that("missing config throws error", {
  test_data <- create_test_data()

  # config is required, should error if not provided and testconfig not in env
  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB"
    ),
    "'config' is required"
  )
})


# =============================================================================
# Test edge cases
# =============================================================================

test_that("filter_cond NULL does not filter", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = NULL
  )

  # Should contain all subjects
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond empty string does not filter", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    filter_cond = ""
  )

  # Should contain all subjects
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond matching no subjects gives warning", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      config = config_df,
      # No subject has age > 100
      filter_cond = "SUBJECT|AGE>100"
    ),
    "matched no subjects"
  )
})


test_that("filter_cond intersection empty gives warning", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      config = config_df,
      filter_cond = "SUBJECT|SEX=='M';SUBJECT|SEX=='F'" # Intersection is empty
    ),
    "Intersection of all filter conditions is empty"
  )
})


# =============================================================================
# Test column mapping
# =============================================================================

test_that("TBNAME is correctly mapped", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    tb_name_var = NULL # Use test_dataset as TBNAME
  )

  expect_true(all(result$TBNAME == "LB"))
})


test_that("TESTCAT is correctly mapped", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # Check TESTCAT column is correctly created
  expect_true("TESTCAT" %in% names(result))
  # Check there are non-NA TESTCAT values
  expect_true(any(!is.na(result$TESTCAT)))
})


test_that("TESTDE is correctly mapped", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # Check TESTDE column is correctly created
  expect_true("TESTDE" %in% names(result))
  # Check values are correctly mapped
  expect_true("WBC" %in% result$TESTDE[!is.na(result$TESTDE)])
})


test_that("column order is correct", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # Check column order: key columns first, then derived columns, then date cols
  first_cols <- names(result)[1:17]
  expect_equal(
    first_cols,
    c(
      "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME", "TESTCAT",
      "TESTCAT_ORIG", "TESTDE", "TESTYN", "TESTDAT", "TESTTIM", "ORRES",
      "rd_date", "first_dose_date", "first_dose_time",
      "cyc_dose_date", "cyc_dose_time"
    )
  )
})


# =============================================================================
# Test window columns carried from testwp-style config
# =============================================================================

create_testwp_config <- function() {
  data.frame(
    TESTCAT = c("CBC", "CBC", "Chemistry"),
    VISITNUM = c("1", "2", "1"),
    VISIT = c("V1", "V2", "V1"),
    wp_rule = c("RD(-7d)", "EX(\u226424h)", "SV(\u00b13d)"),
    ref = c("RD", "EX", "SV"),
    wp = c("-7d", "\u226424h", "\u00b13d"),
    type = c("-", "\u2264", "\u00b1"),
    wpvalue = c(7, 24, 3),
    wp_unit = c("d", "h", "d"),
    stringsAsFactors = FALSE
  )
}

test_that("testwp-style config carries window columns to output", {
  test_data <- create_test_data()
  config_df <- create_testwp_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  window_cols <- c("wp_rule", "ref", "wp", "type", "wpvalue", "wp_unit")
  expect_true(all(window_cols %in% names(result)))

  cbc_v1 <- result[result$TESTCAT == "CBC" & result$VISITNUM == 1, ]
  expect_true(all(cbc_v1$ref == "RD"))
  expect_true(all(cbc_v1$wpvalue == 7))

  cbc_v2 <- result[result$TESTCAT == "CBC" & result$VISITNUM == 2, ]
  expect_true(all(cbc_v2$ref == "EX"))
  expect_true(all(cbc_v2$type == "\u2264"))
  expect_true(all(cbc_v2$wp_unit == "h"))
  expect_true(all(cbc_v2$wpvalue == 24))

  # Date columns then window columns after derived columns
  expect_equal(
    names(result)[1:23],
    c(
      "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME", "TESTCAT",
      "TESTCAT_ORIG", "TESTDE", "TESTYN", "TESTDAT", "TESTTIM", "ORRES",
      "rd_date", "first_dose_date", "first_dose_time",
      "cyc_dose_date", "cyc_dose_time",
      window_cols
    )
  )
})

test_that("plain testconfig without window columns is unchanged", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  expect_false(any(c("wp_rule", "ref", "wp", "wpvalue") %in% names(result)))
  # date columns always present (NA without RAND/EX)
  expect_true(all(c(
    "rd_date", "first_dose_date", "first_dose_time",
    "cyc_dose_date", "cyc_dose_time"
  ) %in% names(result)))
  expect_true(all(is.na(result$rd_date)))
  expect_true(all(is.na(result$first_dose_date)))
  expect_true(all(is.na(result$first_dose_time)))
  expect_true(all(is.na(result$cyc_dose_date)))
  expect_true(all(is.na(result$cyc_dose_time)))
})

test_that("window columns conflicting with test data are skipped with warning", {
  test_data <- create_test_data()
  # Test dataset already contains a column named "ref"
  test_data$LB$ref <- "from_lb"
  config_df <- create_testwp_config()

  expect_warning(
    result <- prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      config = config_df
    ),
    "were not added"
  )

  # Existing test-data column kept as-is; other window columns still joined
  expect_true(all(result$ref[!is.na(result$ref)] == "from_lb"))
  expect_true("wpvalue" %in% names(result))
})

test_that("prepare_test_data attaches rd_date, first_dose_date and cyc_dose_date", {
  test_data <- create_test_data()
  test_data$RAND <- data.frame(
    SUBJID = c("001", "002", "003"),
    RANDDT = as.Date(c("2024-01-15", "2024-01-16", "2024-01-17")),
    stringsAsFactors = FALSE
  )
  test_data$EX <- data.frame(
    SUBJID = c("001", "001", "002"),
    VISITNUM = c(1, 2, 1),
    EXSTDAT = as.Date(c("2024-01-01", "2024-01-15", "2024-01-02")),
    EXSTTIM = c("09:00", "10:30", "14:00"),
    stringsAsFactors = FALSE
  )

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = create_default_config(),
    ex_time_var = "EXSTTIM"
  )

  expect_true(all(result$rd_date[result$SUBJID == "001"] == as.Date("2024-01-15")))
  # First dose is earliest EX date across visits
  expect_true(all(result$first_dose_date[result$SUBJID == "001"] == as.Date("2024-01-01")))
  expect_true(all(result$first_dose_time[result$SUBJID == "001"] == "09:00"))
  expect_true(all(result$first_dose_date[result$SUBJID == "002"] == as.Date("2024-01-02")))
  expect_true(all(result$first_dose_time[result$SUBJID == "002"] == "14:00"))
  v1_001 <- result[result$SUBJID == "001" & result$VISITNUM == 1, ]
  expect_true(all(v1_001$cyc_dose_date == as.Date("2024-01-01")))
  expect_true(all(v1_001$cyc_dose_time == "09:00"))
  v2_001 <- result[result$SUBJID == "001" & result$VISITNUM == 2, ]
  expect_true(all(v2_001$cyc_dose_date == as.Date("2024-01-15")))
  expect_true(all(v2_001$cyc_dose_time == "10:30"))
  # Visit 3 has no dosing record
  v3_001 <- result[result$SUBJID == "001" & result$VISITNUM == 3, ]
  expect_true(all(is.na(v3_001$cyc_dose_date)))
  expect_true(all(is.na(v3_001$cyc_dose_time)))
})

test_that("完整流程 prepare 挂日期到 generate 再到 check", {
  test_data <- create_test_data()
  test_data$RAND <- data.frame(
    SUBJID = c("001", "002", "003"),
    RANDDT = as.Date(c("2024-01-15", "2024-01-15", "2024-01-15")),
    stringsAsFactors = FALSE
  )
  test_data$EX <- data.frame(
    SUBJID = c("001", "002"),
    VISITNUM = c(2, 2),
    EXSTDAT = as.Date(c("2024-01-15", "2024-01-16")),
    stringsAsFactors = FALSE
  )

  config_df <- data.frame(
    TESTCAT = "CBC",
    VISITNUM = "2",
    wp_rule = "EX(0)",
    ref = "EX",
    wp = "0",
    type = "0",
    wpvalue = 0,
    stringsAsFactors = FALSE
  )

  prepared <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df
  )

  # 001 V2: TESTDAT=2024-01-15, EX=2024-01-15 -> in window
  # Force 002 V2 out of window
  prepared$TESTDAT[prepared$SUBJID == "002"] <- as.Date("2024-01-20")

  testwp_dates <- generate_test_window_dates(prepared)
  result <- check_test_window(testwp_dates)

  expect_true(result$has_deviation)
  expect_true(all(result$details$SUBJID == "002"))
})


# =============================================================================
# Tests for multi-dataset support (B1-B14)
# =============================================================================

create_multi_config <- function() {
  data.frame(
    TESTCAT = c("CBC", "CBC", "CBC", "VS", "VS", "VS"),
    VISITNUM = c("1", "2", "3", "1", "2", "3"),
    stringsAsFactors = FALSE
  )
}

test_that("B1 two datasets are combined with correct TBNAME and row count", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  expect_true(is.data.frame(result))
  expect_setequal(unique(result$TBNAME), c("LB", "VSLONG"))
  # LB: 3 subjects * 1 TESTCAT * 3 visits = 9
  # VS: 3 subjects * 1 TESTCAT * 3 visits = 9
  expect_equal(nrow(result), 18)
})


test_that("B2 different variable names per dataset are mapped correctly", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  lb_rows <- result[result$TBNAME == "LB", ]
  vs_rows <- result[result$TBNAME == "VSLONG", ]

  expect_true(any(!is.na(lb_rows$TESTDAT)))
  expect_true(any(!is.na(vs_rows$TESTDAT)))
  expect_true(any(!is.na(vs_rows$TESTDE)))
  expect_equal(unique(vs_rows$TESTDE[!is.na(vs_rows$TESTDE)]), c("Height", "Weight"))
})


test_that("B3 unnamed single value is reused across datasets", {
  data <- create_two_dataset_data()
  # Add generic ORRES column to VSLONG so a single test_result_var can match both datasets
  data$VSLONG$ORRES <- data$VSLONG$VSORRES
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = "ORRES", # single value, reused
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = c("CBC"), VS = c("VS")),
    config = config_df
  )

  lb_rows <- result[result$TBNAME == "LB", ]
  vs_rows <- result[result$TBNAME == "VSLONG", ]

  expect_true(any(!is.na(lb_rows$ORRES)))
  expect_true(any(!is.na(vs_rows$ORRES)))
})


test_that("B4 named arguments align by name regardless of order", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(VS = "VSDAT", LB = "LBDAT"), # order reversed
    test_yn_var = c(VS = "VSYN", LB = "YN"),
    test_result_var = c(VS = "VSORRES", LB = "ORRES"),
    test_cat_var = c(VS = "TNAME", LB = "LBCAT"),
    test_de_var = c(VS = "VSTEST", LB = "LBTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  lb_rows <- result[result$TBNAME == "LB", ]
  vs_rows <- result[result$TBNAME == "VSLONG", ]

  expect_true(any(!is.na(lb_rows$TESTDAT)))
  expect_true(any(!is.na(vs_rows$TESTDAT)))
  expect_equal(unique(vs_rows$TESTDE[!is.na(vs_rows$TESTDE)]), c("Height", "Weight"))
})


test_that("B5 mismatched names raise an error", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  expect_error(
    prepare_test_data(
      data = data,
      test_dataset = c(LB = "LB", VS = "VSLONG"),
      test_date_var = c(LB = "LBDAT") # missing VS
    ),
    "test_date_var.*Missing: VS"
  )
})


test_that("B6 different config_cat per dataset are applied", {
  data <- create_two_dataset_data()
  config_df <- data.frame(
    TESTCAT = c("CBC", "CBC", "CBC", "CHEM", "CHEM", "CHEM", "VS", "VS", "VS"),
    VISITNUM = c("1", "2", "3", "1", "2", "3", "1", "2", "3"),
    stringsAsFactors = FALSE
  )

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config = config_df,
    config_cat = list(LB = c("CBC", "CHEM"), VS = "VS")
  )

  expect_setequal(unique(result$TESTCAT[result$TBNAME == "LB"]), c("CBC", "CHEM"))
  expect_setequal(unique(result$TESTCAT[result$TBNAME == "VSLONG"]), "VS")
  expect_false("CHEM" %in% result$TESTCAT[result$TBNAME == "VSLONG"])
})


test_that("B7 columns missing in one dataset are filled with NA after bind", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  # LB has original LBCAT, LBTEST columns; VS does not
  expect_true("LBCAT" %in% names(result))
  expect_true("LBTEST" %in% names(result))
  expect_true(all(is.na(result$LBCAT[result$TBNAME == "VSLONG"])))
  expect_true(all(is.na(result$LBTEST[result$TBNAME == "VSLONG"])))
})


test_that("B8 single dataset usage remains unchanged", {
  test_data <- create_test_data()
  config_df <- create_default_config()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    config = config_df,
    config_cat = c("CBC", "Chemistry")
  )

  expect_true(is.data.frame(result))
  expect_true(all(result$TBNAME == "LB"))
  expect_true(all(unique(result$TESTCAT) %in% c("CBC", "Chemistry")))
})


test_that("B9 NULL per-dataset argument is reused and produces NA TESTDE", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = NULL, # reused NULL
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  expect_true(all(is.na(result$TESTDE)))
})


test_that("B10 multi-dataset result can be fed to check_missing_test", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  prepared <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  result <- check_missing_test(prepared)

  expect_true(inherits(result, "missing_test_check"))
  expect_type(result$has_deviation, "logical")
})


test_that("B11 factor and character TESTDE are unified to character", {
  data <- create_two_dataset_data()
  # Make LBTEST a factor in LB
  data$LB$LBTEST <- factor(data$LB$LBTEST)
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  expect_true(is.character(result$TESTDE))
  expect_false(any(vapply(result$TESTDE, is.list, logical(1)), na.rm = TRUE))
})


test_that("B12 shared config window columns are preserved", {
  data <- create_two_dataset_data()
  config_df <- data.frame(
    TESTCAT = c("CBC", "CBC", "VS", "VS"),
    VISITNUM = c("1", "2", "1", "2"),
    wp_rule = c("RD(-7d)", "EX(<=24h)", "SV(+/-3d)", "RD(-7d)"),
    ref = c("RD", "EX", "SV", "RD"),
    wp = c("-7d", "<=24h", "+/-3d", "-7d"),
    type = c("-", "<=", "+/-", "-"),
    wpvalue = c(7, 24, 3, 7),
    wp_unit = c("d", "h", "d", "d"),
    stringsAsFactors = FALSE
  )

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df
  )

  window_cols <- c("wp_rule", "ref", "wp", "type", "wpvalue", "wp_unit")
  expect_true(all(window_cols %in% names(result)))

  cbc_v1 <- result[result$TESTCAT == "CBC" & result$VISITNUM == 1, ]
  expect_true(all(cbc_v1$ref == "RD"))
})


test_that("B13 filter_cond single string is reused across datasets", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c(LB = "LB", VS = "VSLONG"),
    test_date_var = c(LB = "LBDAT", VS = "VSDAT"),
    test_yn_var = c(LB = "YN", VS = "VSYN"),
    test_result_var = c(LB = "ORRES", VS = "VSORRES"),
    test_cat_var = c(LB = "LBCAT", VS = "TNAME"),
    test_de_var = c(LB = "LBTEST", VS = "VSTEST"),
    config_cat = list(LB = "CBC", VS = "VS"),
    config = config_df,
    filter_cond = "SUBJECT|SEX=='M'"
  )

  expect_setequal(unique(result$SUBJID), c("001", "003"))
})


test_that("B14 unnamed test_dataset aligns arguments by position", {
  data <- create_two_dataset_data()
  config_df <- create_multi_config()

  result <- prepare_test_data(
    data = data,
    test_dataset = c("LB", "VSLONG"),
    test_date_var = c("LBDAT", "VSDAT"),
    test_yn_var = c("YN", "VSYN"),
    test_result_var = c("ORRES", "VSORRES"),
    test_cat_var = c("LBCAT", "TNAME"),
    test_de_var = c("LBTEST", "VSTEST"),
    config_cat = list(c("CBC"), c("VS")),
    config = config_df
  )

  expect_setequal(unique(result$TBNAME), c("LB", "VSLONG"))
  expect_equal(nrow(result), 18)
})
