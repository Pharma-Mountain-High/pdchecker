# Test prepare_test_data() function
# =============================================================================

# Create test data
# =============================================================================

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


# =============================================================================
# Test basic functionality
# =============================================================================

test_that("prepare_test_data() basic functionality works", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST"
  )

  # Check returned data frame is not empty

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # Check required columns exist
  expected_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME",
    "TESTCAT", "TESTDE", "TESTYN", "TESTDAT", "ORRES"
  )
  expect_true(all(expected_cols %in% names(result)))

  # Check TBNAME is correctly set
  expect_true(all(result$TBNAME == "LB"))

  # Check TESTCAT is correctly mapped
  expect_true("CBC" %in% result$TESTCAT)
})


test_that("prepare_test_data() correctly merges visit and test data", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST"
  )

  # Check row count equals visit records (left_join based on visits)
  expect_equal(nrow(result), nrow(test_data$SV))

  # Check all visit records are in result
  expect_setequal(
    paste(result$SUBJID, result$VISIT),
    paste(test_data$SV$SUBJID, test_data$SV$VISIT)
  )
})


# =============================================================================
# Test filter_cond parameter (single dataset filter)
# =============================================================================

test_that("filter_cond single dataset filter: filter male subjects", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    filter_cond = "SUBJECT|SEX=='M'"
  )

  # Check only male subjects are kept
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "003"))
  expect_false("002" %in% unique_subjids) # 002 is female
})


test_that("filter_cond single dataset filter: filter subjects with age > 30", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    filter_cond = "SUBJECT|AGE>30"
  )

  # Check only subjects with age > 30 are kept
  unique_subjids <- unique(result$SUBJID)
  expect_equal(unique_subjids, "003") # Only 003 has age 45
})


test_that("filter_cond single dataset filter: combined conditions", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
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

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
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

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
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

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
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

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    filter_cond = "ENROL|ENRYN=='Yes'"
  )

  # Only 001 and 002 are enrolled (ENRYN="Yes")
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002"))
  expect_false("003" %in% unique_subjids)
})


test_that("filter_cond enrollment and custom filter combined", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
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
    TESTCAT = c("CBC", "Chemistry"),
    VISITNUM = c("1,2,3", "1,2"),
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
    TESTCAT = c("CBC", "Chemistry", "Urinalysis"),
    VISITNUM = c("1,2", "1,2", "1,2"),
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

  # Format error: missing | separator
  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "SUBJECT SEX=='M'" # Missing |
    ),
    "filter_cond format error"
  )
})


test_that("filter_cond non-existent dataset throws error", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "NONEXIST|SEX=='M'" # Dataset doesn't exist
    ),
    "Dataset specified in filter_cond does not exist"
  )
})


test_that("filter_cond dataset missing SUBJID throws error", {
  test_data <- create_test_data()

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
      filter_cond = "NOSUBJID|VALUE>1"
    ),
    "is missing SUBJID column"
  )
})


test_that("filter_cond syntax error throws error", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      filter_cond = "SUBJECT|NONEXIST_COL=='M'" # Column doesn't exist
    ),
    "failed"
  )
})


test_that("missing required parameter throws error", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data
      # Missing test_dataset
    ),
    "'test_dataset' parameter is required"
  )
})


test_that("non-existent test_dataset throws error", {
  test_data <- create_test_data()

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "NONEXIST"
    ),
    "Test dataset not found in data"
  )
})


test_that("non-existent visit dataset throws error", {
  test_data <- create_test_data()
  test_data$SV <- NULL # Remove visit dataset

  expect_error(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB"
    ),
    "Visit dataset not found in data"
  )
})


# =============================================================================
# Test edge cases
# =============================================================================

test_that("filter_cond NULL does not filter", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    filter_cond = NULL
  )

  # Should contain all subjects
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond empty string does not filter", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    filter_cond = ""
  )

  # Should contain all subjects
  unique_subjids <- unique(result$SUBJID)
  expect_setequal(unique_subjids, c("001", "002", "003"))
})


test_that("filter_cond matching no subjects gives warning", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
      # No subject has age > 100
      filter_cond = "SUBJECT|AGE>100"
    ),
    "matched no subjects"
  )
})


test_that("filter_cond intersection empty gives warning", {
  test_data <- create_test_data()

  expect_warning(
    prepare_test_data(
      data = test_data,
      test_dataset = "LB",
      test_date_var = "LBDAT",
      test_yn_var = "YN",
      test_result_var = "ORRES",
      test_cat_var = "LBCAT",
      test_de_var = "LBTEST",
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

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST",
    tb_name_var = NULL # Use test_dataset as TBNAME
  )

  expect_true(all(result$TBNAME == "LB"))
})


test_that("TESTCAT is correctly mapped", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST"
  )

  # Check TESTCAT column is correctly created
  expect_true("TESTCAT" %in% names(result))
  # Check there are non-NA TESTCAT values
  expect_true(any(!is.na(result$TESTCAT)))
})


test_that("TESTDE is correctly mapped", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST"
  )

  # Check TESTDE column is correctly created
  expect_true("TESTDE" %in% names(result))
  # Check values are correctly mapped
  expect_true("WBC" %in% result$TESTDE[!is.na(result$TESTDE)])
})


test_that("column order is correct", {
  test_data <- create_test_data()

  result <- prepare_test_data(
    data = test_data,
    test_dataset = "LB",
    test_date_var = "LBDAT",
    test_yn_var = "YN",
    test_result_var = "ORRES",
    test_cat_var = "LBCAT",
    test_de_var = "LBTEST"
  )

  # Check column order: key columns first, then derived columns
  first_cols <- names(result)[1:10]
  expect_equal(
    first_cols,
    c(
      "SUBJID", "VISIT", "VISITNUM", "SVDAT", "TBNAME", "TESTCAT", "TESTDE",
      "TESTYN", "TESTDAT", "ORRES"
    )
  )
})
