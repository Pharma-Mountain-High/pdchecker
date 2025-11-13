#' 生成各访视的计划进行日期
#'
#' @description
#' 根据访视编码数据和临床试验数据，为每个受试者的每次访视计算计划日期、窗口期范围。
#' 支持筛选期、治疗期、治疗结束期和随访期访视的计划日期计算。
#'
#' @details
#' ## 访视计划日期计算规则
#'
#' 该函数根据访视类型采用不同的计算逻辑：
#'
#' ### 1. 筛选期访视（Screening）
#' 计划日期 = 首次给药日期
#'
#' ### 2. 治疗期访视（Treatment）
#' - **D1访视（访视日=1）**：
#'   - 第1周期D1：计划日期 = 首次给药日期（如有实际访视则使用实际日期）
#'   - 后续周期D1：计划日期 = 上一周期D1日期 + 周期天数
#' - **非D1访视（如D8, D15）**：
#'   - 计划日期 = 当前周期D1日期 + (当前访视日 - 1)
#'   - 例如：C1D8 = C1D1 + 7天，C2D15 = C2D1 + 14天
#'
#' ### 3. 治疗结束访视（End of Treatment）
#' - EOT访视：计划日期 = 治疗结束日期
#' - EOT+数字格式（如EOT+7）：计划日期 = 治疗结束日期 + 天数
#' - EOS访视：计划日期 = 研究结束日期
#'
#' ### 4. 随访访视（Follow-up）
#' - EOT+数字格式（如EOT+30）：计划日期 = 治疗结束日期 + 天数
#'
#' ## 窗口期计算
#'
#' 根据访视编码数据中的窗口期类型计算窗口期范围：
#' - `±3d`：计划日期前后各3天（窗口开始 = 计划日期-3，窗口结束 = 计划日期+3）
#' - `+2d`：只能延后2天（窗口开始 = 计划日期，窗口结束 = 计划日期+2）
#' - `-2d`：只能提前2天（窗口开始 = 计划日期-2，窗口结束 = 计划日期）
#' - `≤2d`：最多提前2天（窗口开始 = 计划日期-2，窗口结束 = 计划日期）
#' - `≥2d`：最多延后2天（窗口开始 = 计划日期，窗口结束 = 计划日期+2）
#'
#' ## 首次给药日期计算
#'
#' 从所有指定的试验用药数据集中提取每个受试者的最早一次用药开始日期作为首次给药日期。
#' 支持多个用药数据集（如EX1, EX2），可为每个数据集指定不同的日期列名。
#'
#' @param data List 类型，包含所有临床试验数据集的列表
#' @param visit_schedule_data Data frame，访视编码数据，应为 \code{\link{read_visitcode_file}} 函数的输出结果。
#'   必须包含以下列：访视名称、访视编码、窗口期、周期、访视日、type、wpvalue
#' @param ex_datasets Character vector，试验用药数据集名称向量（默认: "EX"）。
#'   可指定多个数据集，如 c("EX1", "EX2")
#' @param ex_date_var Character vector，用药开始日期变量名向量（默认: "EXSTDAT"）。
#'   - 如果长度为1，则所有数据集使用同一列名
#'   - 如果长度等于 \code{ex_datasets} 的长度，则与数据集一一对应
#'   - 例如：c("EXSTDAT1", "EXSTDAT2") 分别对应 c("EX1", "EX2")
#' @param sv_dataset Character string，访视数据集名称（默认: "SV"）
#' @param sv_visit_var Character string，访视数据集中访视名称变量名（默认: "VISIT"）
#' @param sv_visitnum_var Character string，访视数据集中访视编号变量名（默认: "VISITNUM"）
#' @param sv_date_var Character string，访视数据集中访视日期变量名（默认: "SVDAT"）
#' @param eot_dataset Character string，治疗结束数据集名称（默认: "EOT"）。
#'   如数据中不存在该数据集，则治疗结束相关访视的计划日期为NA
#' @param eot_date_var Character string，治疗结束日期变量名（默认: "EOTDAT"）
#' @param ds_dataset Character string，研究结束数据集名称（默认: "DS"）。
#'   如数据中不存在该数据集，则研究结束相关访视的计划日期为NA
#' @param ds_date_var Character string，研究结束日期变量名（默认: "DSDAT"）
#' @param cycle_days Numeric，治疗周期天数（默认: 28）。用于计算后续周期D1访视的计划日期
#'
#' @return Data frame，包含以下列：
#'   \describe{
#'     \item{SUBJID}{受试者ID}
#'     \item{VISIT}{访视名称}
#'     \item{VISITNUM}{访视编号}
#'     \item{visittype}{访视类型（周期信息）}
#'     \item{visitday}{访视日}
#'     \item{planned_date}{计划访视日期}
#'     \item{wp_start}{窗口期开始日期}
#'     \item{wp_end}{窗口期结束日期}
#'     \item{wp_type}{窗口期类型（±、+、-、≤、≥等）}
#'     \item{wp_value}{窗口期数值（天数）}
#'     \item{actual_date}{实际访视日期（如有）}
#'     \item{status}{访视状态（completed=已完成，missing=未完成）}
#'     \item{first_ex_date}{受试者首次给药日期}
#'     \item{eot_date}{受试者治疗结束日期}
#'     \item{eos_date}{受试者研究结束日期}
#'   }
#'
#' @importFrom dplyr filter select mutate arrange group_by ungroup left_join case_when if_else sym distinct bind_rows slice
#' @importFrom magrittr %>%
#' @export
generate_planned_visit_dates <- function(data,
                                         visit_schedule_data,
                                         ex_datasets = "EX",
                                         ex_date_var = "EXSTDAT",
                                         sv_dataset = "SV",
                                         sv_visit_var = "VISIT",
                                         sv_visitnum_var = "VISITNUM",
                                         sv_date_var = "SVDAT",
                                         eot_dataset = "EOT",
                                         eot_date_var = "EOTDAT",
                                         ds_dataset = "DS",
                                         ds_date_var = "DSDAT",
                                         cycle_days = 28) {
  # 模糊匹配访视类型的辅助函数
  match_visit_type <- function(visit_type) {
    if (is.na(visit_type)) {
      return("unknown")
    }
    visit_type <- tolower(visit_type)

    if (grepl("筛选", visit_type)) {
      return("screening")
    } else if (grepl("治疗", visit_type) && !grepl("治疗结束", visit_type)) {
      return("treatment")
    } else if (grepl("治疗结束|退出", visit_type)) {
      return("end_of_treatment")
    } else if (grepl("随访", visit_type)) {
      return("follow_up")
    } else {
      return("unknown")
    }
  }

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

  # 计算首次用药日期 - 从所有指定的EX数据集中提取最早的用药开始日期
  # 如果只提供一个日期列名，则对所有数据集使用同一个列名
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

  first_ex_dates_list <- list()
  for (i in seq_along(ex_datasets)) {
    ex_ds <- ex_datasets[i]
    current_date_var <- ex_date_vars[i]

    if (ex_ds %in% names(data)) {
      ex_df <- data[[ex_ds]]
      if (current_date_var %in% names(ex_df) && "SUBJID" %in% names(ex_df)) {
        ex_dates <- ex_df %>%
          filter(!is_sas_na(!!sym(current_date_var))) %>%
          mutate(ex_date = as.Date(!!sym(current_date_var))) %>%
          filter(!is.na(ex_date)) %>%
          select(SUBJID, ex_date)

        if (nrow(ex_dates) > 0) {
          first_ex_dates_list[[length(first_ex_dates_list) + 1]] <- ex_dates
        }
      }
    }
  }

  # 合并所有EX数据集的日期，并获取每个受试者的最早日期
  if (length(first_ex_dates_list) > 0) {
    first_ex_dates <- bind_rows(first_ex_dates_list) %>%
      group_by(SUBJID) %>%
      filter(ex_date == min(ex_date)) %>%
      slice(1) %>%
      ungroup() %>%
      select(SUBJID, first_ex_date = ex_date)
  } else {
    first_ex_dates <- data.frame(SUBJID = character(), first_ex_date = as.Date(character()))
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

  # 合并每个受试者的相关日期（首次用药、治疗结束、研究结束）
  subject_dates <- data.frame(SUBJID = subjects) %>%
    left_join(first_ex_dates, by = "SUBJID") %>%
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


    first_ex_date <- subj_dates$first_ex_date[1]
    eot_date <- subj_dates$eot_date[1]
    eos_date <- subj_dates$eos_date[1]

    # 为每个访视计算计划日期
    visit_results <- list()
    cycle_d1_dates <- list() # 存储每个周期D1的计划日期
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

          # D1访视处理
          actual_d1_visit <- subj_actual[subj_actual$VISIT == visit_name, ]
          planned_date <- NA

          if (nrow(actual_d1_visit) > 0) {
            # 如果已有实际访视，使用实际日期
            planned_date <- actual_d1_visit$actual_date[1]
          } else {
            # 没有实际访视，计算计划日期
            if (current_cycle == 1) {
              # 第一个周期D1，使用首次给药日期
              planned_date <- first_ex_date
            } else {
              # 后续周期D1，基于上一个周期D1日期 + 周期天数
              prev_cycle_key <- paste0("C", current_cycle - 1, "D1")

              if (!is.null(cycle_d1_dates[[prev_cycle_key]])) {
                planned_date <- cycle_d1_dates[[prev_cycle_key]] + cycle_days
              } else if (!is.null(last_cycle_d1_date)) {
                planned_date <- last_cycle_d1_date + cycle_days
              } else {
                # 如果找不到上一个周期，使用首次给药日期
                planned_date <- first_ex_date + (current_cycle - 1) * cycle_days
              }
            }
          }

          # 存储当前周期D1日期
          cycle_key <- paste0("C", current_cycle, "D1")
          cycle_d1_dates[[cycle_key]] <- planned_date
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
        planned_date <- first_ex_date
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
            cycle_d1_date <- cycle_d1_dates[[cycle_key]]

            if (!is.null(cycle_d1_date) && !is.na(cycle_d1_date)) {
              # 计划日期 = 该周期D1计划日期 + (该访视的访视日 - D1访视日的差值)
              planned_date <- cycle_d1_date + (current_day - 1)
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
        # 随访访视：可能基于EOT日期
        if (grepl("^EOT\\+", visit_day) && !is.na(eot_date)) {
          days_add <- as.numeric(gsub("^EOT\\+", "", visit_day))
          if (!is.na(days_add)) {
            planned_date <- eot_date + days_add
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
        first_ex_date = first_ex_date,
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
