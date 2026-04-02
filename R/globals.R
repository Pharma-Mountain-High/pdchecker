# Global variables declaration
# This file declares global variables used in dplyr/tidyverse non-standard evaluation
# to avoid "no visible binding for global variable" notes in R CMD check

utils::globalVariables(c(
  # =========================================================================
  # Subject and Site Information
  # =========================================================================
  "SUBJID", "SITEID", "AGE",

  # =========================================================================
  # Visit Related
  # =========================================================================
  "VISIT", "VISITNUM", "VISITNAME", "SVDAT",
  "visit_date", "planned_date", "actual_date",
  "status", "visit_category", "visittype",
  "first_dose_date", "last_dose_date", "eot_date", "eos_date",
  "dose_date",

  # =========================================================================
  # Test/Lab Related
  # =========================================================================
  "TESTCAT", "TESTDE", "TESTYN", "TESTDAT", "ORRES", "TBNAME",
  "LBTEST", "LBORRES", "LBORRESU", "LBORNRHI", "LBDAT",
  "TEST_VALUE", "ULN", "LLN", "RATIO",
  "LDL_VALUE", "LDL_UNIT",
  "GFR", "GFR_VALUE",
  "TG_VALUE", "TG_UNIT",

  # =========================================================================
  # Randomization Related
  # =========================================================================
  "RANDID", "RANDTT", "RANDLDL",
  "STRAT1", "STRAT2", "STRAT1FL", "STRAT2FL",
  # Chinese column names
  "分层因素",
  "随机号",
  # Derived variables
  "iwrs_value", "actual_value",

  # =========================================================================
  # Medication/Exposure Related
  # =========================================================================
  "EXSTDAT", "EXENDAT", "EXDOS", "EXADOS", "EXSITE", "EXTRT",
  "dose_value", "duration_days", "total_dose",
  "sites", "required_sites", "site_count",
  "WINDOW", "STUDY_DAY", "missing_admin",

  # =========================================================================
  # Medical History Related
  # =========================================================================
  "MHTERM", "has_ascvd",
  "PHDRUG", "PHDRSTAT", "PHSPERF",
  "issue_type",

  # =========================================================================
  # NYHA Related
  # =========================================================================
  "NYHAYN", "NYHASCO", "has_nyha",

  # =========================================================================
  # Statin Related
  # =========================================================================
  "STATINYN",
  "meets_dose_criteria", "meets_duration_criteria",

  # =========================================================================
  # Pregnancy Related
  # =========================================================================
  "RPTYP", "RPRES",

  # =========================================================================
  # Virus Related
  # =========================================================================
  "VIRTEST", "VIRRES",
  "HBVPERF", "HBVRES", "HBVUNT", "DNA_VALUE",
  "HCVPERF", "HCVRES",
  "has_hbsag", "has_hbv_dna", "has_both",

  # =========================================================================
  # Thyroid Related
  # =========================================================================
  "LBORNRLO",

  # =========================================================================
  # ICF Related
  # =========================================================================
  "event_datetime", "icf_datetime", "action",

  # =========================================================================
  # Deviation Related
  # =========================================================================
  "CRITERION",
  "has_deviation", "deviation_reason", "deviation_count",

  # =========================================================================
  # Window Related
  # =========================================================================
  "wp_start", "wp_end", "wp_type", "wp_value",
  "window_start", "window_end",
  "start_date", "last_visit", "last_date", "test_date",
  "target_date", "in_window",
  "type", "wpvalue",

  # =========================================================================
  # Visit Code File Related (Chinese column names)
  # =========================================================================
  "VISIT", "VISITNUM", "VISITDAY", "CYCLE", "WP",

  # =========================================================================
  # Parse Output Related
  # =========================================================================
  "subjid", "check_name", "message", "details",

  # =========================================================================
  # Prepare Test Data Related
  # =========================================================================
  "TESTCAT_orig",

  # =========================================================================
  # dplyr Functions (used without namespace prefix)
  # =========================================================================
  "rowwise"
))
