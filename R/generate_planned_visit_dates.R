#' Generate Planned Visit Dates
#'
#' @description
#' Calculate planned visit dates and visit window ranges for each subject based on
#' visit schedule data and clinical trial data. Supports screening, treatment,
#' end of treatment, and follow-up visit date calculations.
#'
#' @details
#' ## Planned Visit Date Calculation Rules
#'
#' This function uses different calculation logic based on visit type:
#'
#' ### 1. Screening Visits
#' Planned date = First dose date
#'
#' **Example**: If first dose date is 2024-01-01, screening visit planned date is 2024-01-01
#'
#' ### 2. Treatment Visits
#'
#' #### **D1 Visits (visit day = 1)**
#'
#' **C1D1 (Cycle 1 Day 1)**:
#' - If actual visit exists: Planned date = Actual visit date
#' - If no actual visit: Planned date = First dose date
#'
#' **CnD1 (Subsequent cycles, n > 1)**:
#' - Planned date = Previous cycle D1 reference date + cycle_days (default 28 days)
#' - **Reference date selection rule**:
#'   1. Prefer previous cycle D1 **actual visit date** (if exists)
#'   2. Otherwise use previous cycle D1 **planned date**
#'
#' **Key features**:
#' - C2D1, C3D1 planned dates are calculated iteratively, not using their own actual dates
#' - If C2D1 has an actual visit, that date affects C3D1 planned date calculation
#'
#' **Example**:
#' ```
#' First dose date: 2024-01-01
#' C1D1 planned: 2024-01-01 (no actual visit)
#' C2D1 planned: 2024-01-01 + 28 days = 2024-01-29
#' C2D1 actual: 2024-02-02 (delayed visit)
#' C3D1 planned: 2024-02-02 + 28 days = 2024-03-01 (based on C2D1 actual)
#' ```
#'
#' #### **Non-D1 Visits (e.g., D8, D15, D21)**
#'
#' Planned date = Current cycle D1 reference date + (visit day - 1)
#'
#' **Reference date selection rule**:
#' 1. Prefer current cycle D1 **actual visit date** (if exists)
#' 2. Otherwise use current cycle D1 **planned date**
#'
#' **Example**:
#' ```
#' C2D1 planned: 2024-01-29
#' C2D1 actual: 2024-02-02
#'
#' C2D8 planned: 2024-02-02 + 7 days = 2024-02-09 (based on D1 actual)
#' C2D15 planned: 2024-02-02 + 14 days = 2024-02-16 (based on D1 actual)
#' ```
#'
#' ### 3. End of Treatment Visits
#'
#' - **EOT**: Planned date = End of treatment date (eot_date)
#' - **EOT+number**: Planned date = End of treatment date + days
#'   - Example: EOT+7 = eot_date + 7 days
#' - **EOS**: Planned date = End of study date (eos_date)
#'
#' ### 4. Follow-up Visits
#'
#' - **EOT+number**: Planned date = End of treatment date + days
#'   - Example: EOT+30 = eot_date + 30 days
#' - **Last Dose+number** or **LD+number**: Planned date = Last dose date + days
#'   - Example: LD+30 = last_dose_date + 30 days
#'   - Example: Last Dose+60 = last_dose_date + 60 days
#'
#' ## Visit Window Calculation
#'
#' Calculate visit window range based on window type in visit schedule data:
#'
#' | Window Type | Window Start | Window End | Description | Example |
#' |-------------|--------------|------------|-------------|---------|
#' | +/-3d | planned-3 | planned+3 | 3 days before/after | planned 2024-01-10, window 2024-01-07 to 2024-01-13 |
#' | +2d | planned | planned+2 | delay only | planned 2024-01-10, window 2024-01-10 to 2024-01-12 |
#' | -2d | planned-2 | planned | early only | planned 2024-01-10, window 2024-01-08 to 2024-01-10 |
#'
#' **Window purpose**: Used to determine if actual visit is within allowed time range.
#' Visits outside window are considered protocol deviations.
#'
#' ## First and Last Dose Date Calculation
#'
#' ### **First Dose Date**
#' - Extract earliest dosing start date from all specified exposure datasets (ex_datasets)
#' - Always uses **dosing start date** variable (ex_date_var)
#' - Supports multiple datasets, automatically merges and takes minimum
#'
#' **Example**:
#' ```r
#' EX1 data: Subject 001, dose dates 2024-01-01, 2024-01-29
#' EX2 data: Subject 001, dose date 2024-01-03
#' First dose date = min(2024-01-01, 2024-01-29, 2024-01-03) = 2024-01-01
#' ```
#'
#' ### **Last Dose Date**
#' - Uses different logic based on whether ex_end_date_var is specified:
#'
#' **Case 1: ex_end_date_var not specified (default)**
#' - Uses dosing start date (ex_date_var)
#' - Takes **latest** dosing start date from all records
#'
#' **Case 2: ex_end_date_var specified**
#' - Uses dosing end date (ex_end_date_var)
#' - Takes **latest** dosing end date from all records
#' - Suitable for studies with explicit start and end dates
#'
#' @param data List containing all clinical trial datasets
#' @param visit_schedule_data Data frame, visit schedule data from \code{\link{read_visitcode_file}}.
#'   Must contain columns: visit_name, visit_code, window, cycle, visit_day, type, wpvalue
#' @param ex_datasets Character vector, exposure dataset names (default: "EX").
#'   Multiple datasets can be specified, e.g., c("EX1", "EX2")
#' @param ex_date_var Character vector, dosing start date variable names (default: "EXSTDAT").
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#'   - Example: c("EXSTDAT1", "EXSTDAT2") corresponds to c("EX1", "EX2")
#' @param ex_end_date_var Character vector, dosing end date variable names (default: NULL).
#'   - If NULL or empty, last dose date uses \code{ex_date_var} (dosing start date)
#'   - If specified, last dose date uses dosing end date
#'   - If length 1, all datasets use the same column name
#'   - If length equals \code{ex_datasets}, corresponds one-to-one with datasets
#' @param sv_dataset Character string, visit dataset name (default: "SV")
#' @param sv_visit_var Character string, visit name variable in visit dataset (default: "VISIT")
#' @param sv_visitnum_var Character string, visit number variable in visit dataset (default: "VISITNUM")
#' @param sv_date_var Character string, visit date variable in visit dataset (default: "SVDAT")
#' @param eot_dataset Character string, end of treatment dataset name (default: "EOT").
#'   If dataset doesn't exist, EOT-related planned dates will be NA
#' @param eot_date_var Character string, end of treatment date variable (default: "EOTDAT")
#' @param ds_dataset Character string, disposition dataset name (default: "DS").
#'   If dataset doesn't exist, EOS-related planned dates will be NA
#' @param ds_date_var Character string, end of study date variable (default: "DSDAT")
#' @param cycle_days Numeric, treatment cycle length in days (default: 28).
#'   Used to calculate subsequent cycle D1 planned dates
#'
#' @return Data frame with the following columns:
#'   \describe{
#'     \item{SUBJID}{Subject ID}
#'     \item{VISIT}{Visit name}
#'     \item{VISITNUM}{Visit number}
#'     \item{visittype}{Visit type (cycle information)}
#'     \item{visitday}{Visit day}
#'     \item{planned_date}{Planned visit date}
#'     \item{wp_start}{Visit window start date}
#'     \item{wp_end}{Visit window end date}
#'     \item{wp_type}{Window type (+/-, +, -, etc.)}
#'     \item{wp_value}{Window value in days}
#'     \item{actual_date}{Actual visit date (if available)}
#'     \item{status}{Visit status (completed or missing)}
#'     \item{first_dose_date}{Subject's first dose date}
#'     \item{last_dose_date}{Subject's last dose date}
#'     \item{eot_date}{Subject's end of treatment date}
#'     \item{eos_date}{Subject's end of study date}
#'   }
#'
#' @importFrom dplyr filter select mutate arrange group_by ungroup left_join
#' @importFrom dplyr case_when if_else sym distinct bind_rows slice
#' @importFrom magrittr %>%
#' @export
generate_planned_visit_dates <- function(data,
                                         visit_schedule_data,
                                         ex_datasets = "EX",
                                         ex_date_var = "EXSTDAT",
                                         ex_end_date_var = NULL,
                                         sv_dataset = "SV",
                                         sv_visit_var = "VISIT",
                                         sv_visitnum_var = "VISITNUM",
                                         sv_date_var = "SVDAT",
                                         eot_dataset = "EOT",
                                         eot_date_var = "EOTDAT",
                                         ds_dataset = "DS",
                                         ds_date_var = "DSDAT",
                                         cycle_days = 28) {
  # 判断是否为D1访视的辅助函数
  is_d1_visit <- function(visit_name, visit_day) {
    if (is.na(visit_day)) {
      return(FALSE)
    }
    # 只检查访视日是否为1
    return(visit_day == "1")
  }

  # 检查visit_schedule_data必要的列
  required_visit_cols <- c("访视名称", "访视编码", "窗口期", "周期", "访视日", "type", "wpvalue")
  if (!all(required_visit_cols %in% names(visit_schedule_data))) {
    stop("访视编码文件缺少必要的列: ", paste(setdiff(required_visit_cols, names(visit_schedule_data)), collapse = ", "))
  }

  # 计算首次和末次给药日期
  # 首次给药日期：使用给药开始日期（ex_date_var）
  # 末次给药日期：如果指定了给药结束日期（ex_end_date_var），则使用给药结束日期；否则使用给药开始日期

  # 处理给药开始日期变量
  if (length(ex_date_var) == 1) {
    ex_date_vars <- rep(ex_date_var, length(ex_datasets))
  } else if (length(ex_date_var) == length(ex_datasets)) {
    ex_date_vars <- ex_date_var
  } else {
    stop(
      "ex_date_var 的长度必须为1或与 ex_datasets 的长度相同。ex_datasets 长度: ",
      length(ex_datasets), ", ex_date_var 长度: ", length(ex_date_var)
    )
  }

  # 处理给药结束日期变量（如果指定）
  use_end_date <- !is.null(ex_end_date_var) && length(ex_end_date_var) > 0
  if (use_end_date) {
    if (length(ex_end_date_var) == 1) {
      ex_end_date_vars <- rep(ex_end_date_var, length(ex_datasets))
    } else if (length(ex_end_date_var) == length(ex_datasets)) {
      ex_end_date_vars <- ex_end_date_var
    } else {
      stop(
        "ex_end_date_var 的长度必须为1或与 ex_datasets 的长度相同。ex_datasets 长度: ",
        length(ex_datasets), ", ex_end_date_var 长度: ", length(ex_end_date_var)
      )
    }
  }

  # 收集首次给药日期（使用开始日期）
  first_dose_dates_list <- list()
  for (i in seq_along(ex_datasets)) {
    ex_ds <- ex_datasets[i]
    current_date_var <- ex_date_vars[i]

    if (ex_ds %in% names(data)) {
      ex_df <- data[[ex_ds]]
      if (current_date_var %in% names(ex_df) && "SUBJID" %in% names(ex_df)) {
        ex_dates <- ex_df %>%
          filter(!is_sas_na(!!sym(current_date_var))) %>%
          mutate(dose_date = as.Date(!!sym(current_date_var))) %>%
          filter(!is.na(dose_date)) %>%
          select(SUBJID, dose_date)

        if (nrow(ex_dates) > 0) {
          first_dose_dates_list[[length(first_dose_dates_list) + 1]] <- ex_dates
        }
      }
    }
  }

  # 计算首次给药日期
  if (length(first_dose_dates_list) > 0) {
    first_dose_dates <- bind_rows(first_dose_dates_list) %>%
      group_by(SUBJID) %>%
      filter(dose_date == min(dose_date)) %>%
      slice(1) %>%
      ungroup() %>%
      select(SUBJID, first_dose_date = dose_date)
  } else {
    first_dose_dates <- data.frame(SUBJID = character(), first_dose_date = as.Date(character()))
  }

  # 收集末次给药日期（使用结束日期或开始日期）
  last_dose_dates_list <- list()
  for (i in seq_along(ex_datasets)) {
    ex_ds <- ex_datasets[i]
    # 如果指定了给药结束日期，使用结束日期；否则使用开始日期
    current_date_var <- if (use_end_date) ex_end_date_vars[i] else ex_date_vars[i]

    if (ex_ds %in% names(data)) {
      ex_df <- data[[ex_ds]]
      if (current_date_var %in% names(ex_df) && "SUBJID" %in% names(ex_df)) {
        ex_dates <- ex_df %>%
          filter(!is_sas_na(!!sym(current_date_var))) %>%
          mutate(dose_date = as.Date(!!sym(current_date_var))) %>%
          filter(!is.na(dose_date)) %>%
          select(SUBJID, dose_date)

        if (nrow(ex_dates) > 0) {
          last_dose_dates_list[[length(last_dose_dates_list) + 1]] <- ex_dates
        }
      }
    }
  }

  # 计算末次给药日期
  if (length(last_dose_dates_list) > 0) {
    last_dose_dates <- bind_rows(last_dose_dates_list) %>%
      group_by(SUBJID) %>%
      filter(dose_date == max(dose_date)) %>%
      slice(1) %>%
      ungroup() %>%
      select(SUBJID, last_dose_date = dose_date)
  } else {
    last_dose_dates <- data.frame(SUBJID = character(), last_dose_date = as.Date(character()))
  }

  # 获取访视数据
  if (!sv_dataset %in% names(data)) {
    stop(paste0("缺少访视数据集: ", sv_dataset))
  }
  sv_data <- data[[sv_dataset]]
  if (!sv_date_var %in% names(sv_data) || !"SUBJID" %in% names(sv_data) || !sv_visit_var %in% names(sv_data)) {
    stop(paste0("访视数据集 ", sv_dataset, " 缺少必要的列: SUBJID, ", sv_visit_var, ", ", sv_date_var))
  }

  # 获取治疗结束日期
  eot_dates <- data.frame(SUBJID = character(), eot_date = as.Date(character()))
  if (eot_dataset %in% names(data)) {
    eot_df <- data[[eot_dataset]]
    if (eot_date_var %in% names(eot_df) && "SUBJID" %in% names(eot_df)) {
      eot_dates <- eot_df %>%
        filter(!is_sas_na(!!sym(eot_date_var))) %>%
        mutate(eot_date = as.Date(!!sym(eot_date_var))) %>%
        select(SUBJID, eot_date) %>%
        distinct()
    }
  }

  # 获取研究结束日期
  eos_dates <- data.frame(SUBJID = character(), eos_date = as.Date(character()))
  if (ds_dataset %in% names(data)) {
    ds_df <- data[[ds_dataset]]
    if (ds_date_var %in% names(ds_df) && "SUBJID" %in% names(ds_df)) {
      eos_dates <- ds_df %>%
        filter(!is_sas_na(!!sym(ds_date_var))) %>%
        mutate(eos_date = as.Date(!!sym(ds_date_var))) %>%
        select(SUBJID, eos_date) %>%
        distinct()
    }
  }

  # 准备访视信息数据框
  # 使用 utils.R 中的共享函数 match_visit_type 进行访视类型分类
  visit_info <- visit_schedule_data %>%
    mutate(
      visit = 访视名称,
      visitnum = 访视编码,
      visitday = 访视日,
      visittype = 周期,
      wp = 窗口期,
      wp_type = type,
      wp_value = as.numeric(wpvalue),
      visit_category = sapply(周期, match_visit_type),
      is_d1 = mapply(is_d1_visit, 访视名称, 访视日)
    ) %>%
    arrange(seq_len(nrow(visit_schedule_data)))

  # 获取受试者列表（从访视数据集中）
  subjects <- unique(sv_data$SUBJID)

  # 获取实际访视数据
  # 检查是否有VISITNUM列
  has_visitnum <- sv_visitnum_var %in% names(sv_data)

  actual_visits <- sv_data %>%
    filter(!is_sas_na(!!sym(sv_date_var))) %>%
    mutate(actual_date = as.Date(!!sym(sv_date_var))) %>%
    mutate(VISIT = !!sym(sv_visit_var)) %>%
    mutate(VISITNUM = !!sym(sv_visitnum_var)) %>%
    select(SUBJID, VISIT, VISITNUM, actual_date) %>%
    arrange(SUBJID, actual_date)

  # 合并每个受试者的相关日期（首次用药、末次用药、治疗结束、研究结束）
  subject_dates <- data.frame(SUBJID = subjects) %>%
    left_join(first_dose_dates, by = "SUBJID") %>%
    left_join(last_dose_dates, by = "SUBJID") %>%
    left_join(eot_dates, by = "SUBJID") %>%
    left_join(eos_dates, by = "SUBJID")

  # 为每个受试者计算计划访视日期
  planned_dates_list <- lapply(subjects, function(subj_id) {
    # 获取该受试者的实际访视记录
    subj_actual <- actual_visits %>%
      filter(SUBJID == subj_id) %>%
      arrange(actual_date)

    # 获取该受试者的相关日期
    subj_dates <- subject_dates %>%
      filter(SUBJID == subj_id)


    first_dose_date <- subj_dates$first_dose_date[1]
    last_dose_date <- subj_dates$last_dose_date[1]
    eot_date <- subj_dates$eot_date[1]
    eos_date <- subj_dates$eos_date[1]

    # 为每个访视计算计划日期
    visit_results <- list()
    cycle_d1_dates <- list() # 存储每个周期D1的计划日期（按规则计算）
    cycle_d1_actual_dates <- list() # 存储每个周期D1的实际日期（如果有）
    last_cycle_d1_date <- NULL # 追踪最近一个周期D1的日期

    # 首先处理所有D1访视，确保其他访视能正确引用D1的计划日期
    d1_visits <- visit_info[visit_info$is_d1, ]
    if (nrow(d1_visits) > 0) {
      for (i in seq_len(nrow(d1_visits))) {
        visit_row <- d1_visits[i, ]
        visit_name <- visit_row$visit
        visit_type <- visit_row$visittype
        visit_day <- visit_row$visitday
        visit_category <- visit_row$visit_category

        if (visit_category == "treatment" && !is.na(visit_day) && is.numeric(as.numeric(visit_day))) {
          # 提取周期信息
          cycle_match <- regmatches(visit_type, regexpr("\\d+", visit_type))
          current_cycle <- if (length(cycle_match) > 0) as.numeric(cycle_match[1]) else 1

          # 检查是否有实际访视记录
          actual_d1_visit <- subj_actual[subj_actual$VISIT == visit_name, ]
          actual_d1_date <- if (nrow(actual_d1_visit) > 0) actual_d1_visit$actual_date[1] else as.Date(NA)

          # 计算计划日期
          planned_date <- NA

          if (current_cycle == 1) {
            # C1D1：如果有实际访视，使用实际日期；否则使用首次给药日期
            if (!is.na(actual_d1_date)) {
              planned_date <- actual_d1_date
            } else {
              planned_date <- first_dose_date
            }
          } else {
            # CnD1（n>1）：基于上一周期D1日期 + 周期天数
            # 优先使用上一周期D1的实际日期；如果没有，则使用计划日期
            prev_cycle_key <- paste0("C", current_cycle - 1, "D1")

            # 选择上一周期D1的基准日期：实际日期（如有）> 计划日期
            prev_d1_base_date <- if (!is.null(cycle_d1_actual_dates[[prev_cycle_key]])) {
              cycle_d1_actual_dates[[prev_cycle_key]]
            } else if (!is.null(cycle_d1_dates[[prev_cycle_key]])) {
              cycle_d1_dates[[prev_cycle_key]]
            } else if (!is.null(last_cycle_d1_date)) {
              last_cycle_d1_date
            } else {
              NULL
            }

            if (!is.null(prev_d1_base_date) && !is.na(prev_d1_base_date)) {
              planned_date <- prev_d1_base_date + cycle_days
            } else {
              # 兜底方案：如果找不到上一个周期，使用首次给药日期
              planned_date <- first_dose_date + (current_cycle - 1) * cycle_days
            }
          }

          # 存储当前周期D1的计划日期和实际日期
          cycle_key <- paste0("C", current_cycle, "D1")
          cycle_d1_dates[[cycle_key]] <- planned_date
          if (!is.na(actual_d1_date)) {
            cycle_d1_actual_dates[[cycle_key]] <- actual_d1_date
          }
          last_cycle_d1_date <- planned_date
        }
      }
    }

    # 然后处理所有访视（包括已处理的D1访视）
    for (i in seq_len(nrow(visit_info))) {
      visit_row <- visit_info[i, ]
      visit_name <- visit_row$visit
      visit_num <- visit_row$visitnum
      visit_type <- visit_row$visittype
      visit_day <- visit_row$visitday
      visit_category <- visit_row$visit_category
      is_d1 <- visit_row$is_d1

      # 根据访视类型和访视日计算计划日期
      planned_date <- NA

      if (visit_category == "screening") {
        # 筛选期访视：计划日期为首次给药日期
        planned_date <- first_dose_date
      } else if (visit_category == "treatment") {
        # 治疗期访视：新的规则
        if (!is.na(visit_day) && is.numeric(as.numeric(visit_day))) {
          current_day <- as.numeric(visit_day)

          # 提取周期信息
          cycle_match <- regmatches(visit_type, regexpr("\\d+", visit_type))
          current_cycle <- if (length(cycle_match) > 0) as.numeric(cycle_match[1]) else 1

          if (is_d1) {
            # D1访视的计划日期已在预处理步骤中计算，直接获取
            cycle_key <- paste0("C", current_cycle, "D1")
            planned_date <- cycle_d1_dates[[cycle_key]]
          } else {
            # 非D1访视，基于当前周期D1日期计算
            cycle_key <- paste0("C", current_cycle, "D1")

            # 优先使用该周期D1的实际访视日期（如果有）；否则使用计划日期
            base_d1_date <- if (!is.null(cycle_d1_actual_dates[[cycle_key]])) {
              cycle_d1_actual_dates[[cycle_key]]
            } else {
              cycle_d1_dates[[cycle_key]]
            }

            if (!is.null(base_d1_date) && !is.na(base_d1_date)) {
              # 计划日期 = D1日期（实际或计划）+ (该访视的访视日 - 1)
              planned_date <- base_d1_date + (current_day - 1)
            } else {
              # 如果找不到对应周期的D1日期，设为NA
              planned_date <- as.Date(NA)
            }
          }
        }
      } else if (visit_category == "end_of_treatment") {
        # 治疗结束访视：根据访视日内容解析
        if (visit_day == "EOT" && !is.na(eot_date)) {
          planned_date <- eot_date
        } else if (grepl("^EOT\\+", visit_day) && !is.na(eot_date)) {
          # 解析 EOT+数字 格式
          days_add <- as.numeric(gsub("^EOT\\+", "", visit_day))
          if (!is.na(days_add)) {
            planned_date <- eot_date + days_add
          }
        } else if (visit_day == "EOS" && !is.na(eos_date)) {
          planned_date <- eos_date
        }
      } else if (visit_category == "follow_up") {
        # 随访访视：可能基于EOT日期或末次用药日期
        if (grepl("^EOT\\+", visit_day) && !is.na(eot_date)) {
          # 基于EOT日期
          days_add <- as.numeric(gsub("^EOT\\+", "", visit_day))
          if (!is.na(days_add)) {
            planned_date <- eot_date + days_add
          }
        } else if (grepl("^(Last Dose|LD)\\+", visit_day) && !is.na(last_dose_date)) {
          # 基于末次用药日期：支持 "Last Dose+30" 或 "LD+30" 格式
          days_add <- as.numeric(gsub("^(Last Dose|LD)\\+", "", visit_day))
          if (!is.na(days_add)) {
            planned_date <- last_dose_date + days_add
          }
        }
      }

      # 检查实际访视状态

      actual_visit <- subj_actual[subj_actual$VISITNUM == visit_num, ]

      if (nrow(actual_visit) > 0) {
        visit_status <- "completed"
        actual_date <- actual_visit$actual_date[1]
      } else {
        visit_status <- "missing"
        actual_date <- as.Date(NA)
      }

      # 计算窗口期的开始和结束日期
      window_type <- visit_row$wp_type
      window_value <- visit_row$wp_value
      window_start <- as.Date(NA)
      window_end <- as.Date(NA)

      if (!is.na(planned_date) && !is.na(window_type) && !is.na(window_value)) {
        if (window_type == "±") {
          window_start <- planned_date - window_value
          window_end <- planned_date + window_value
        } else if (window_type == "+") {
          window_start <- planned_date
          window_end <- planned_date + window_value
        } else if (window_type == "-") {
          window_start <- planned_date - window_value
          window_end <- planned_date
        } else if (window_type == "≤") {
          window_start <- planned_date - window_value
          window_end <- planned_date
        } else if (window_type == "≥") {
          window_start <- planned_date
          window_end <- planned_date + window_value
        } else {
          # 默认±1d
          window_start <- planned_date - 1
          window_end <- planned_date + 1
        }
      }

      visit_results[[length(visit_results) + 1]] <- data.frame(
        SUBJID = subj_id,
        VISIT = visit_name,
        VISITNUM = visit_num,
        visittype = visit_type,
        visitday = visit_day,
        planned_date = planned_date,
        wp_start = window_start,
        wp_end = window_end,
        wp_type = window_type,
        wp_value = window_value,
        actual_date = actual_date,
        status = visit_status,
        first_dose_date = first_dose_date,
        last_dose_date = last_dose_date,
        eot_date = eot_date,
        eos_date = eos_date,
        stringsAsFactors = FALSE
      )
    }

    # 合并所有访视结果
    if (length(visit_results) > 0) {
      return(do.call(rbind, visit_results))
    } else {
      return(data.frame())
    }
  })
  planned_dates <- do.call(rbind, planned_dates_list)
  # 返回结果
  if (nrow(planned_dates) > 0) {
    result <- planned_dates %>%
      arrange(SUBJID, actual_date)
    return(result)
  } else {
    return(data.frame())
  }
}
