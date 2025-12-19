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
#' **示例**：如果首次给药日期为 2024-01-01，则筛选访视的计划日期为 2024-01-01
#'
#' ### 2. 治疗期访视（Treatment）
#'
#' #### **D1访视（访视日=1）**
#'
#' **C1D1（第1周期D1）**：
#' - 如有实际访视记录：计划日期 = 实际访视日期
#' - 无实际访视记录：计划日期 = 首次给药日期
#'
#' **CnD1（后续周期D1，n>1）**：
#' - 计划日期 = 上一周期D1的基准日期 + 周期天数（cycle_days，默认28天）
#' - **基准日期选择规则**：
#'   1. 优先使用上一周期D1的**实际访视日期**（如果存在）
#'   2. 否则使用上一周期D1的**计划日期**
#'
#' **关键特性**：
#' - C2D1、C3D1等访视的计划日期按规则递推，不直接使用其自身的实际访视日期
#' - 如果C2D1有实际访视，该实际日期会影响C3D1的计划日期计算
#'
#' **示例**：
#' ```
#' 首次给药日期：2024-01-01
#' C1D1计划日期：2024-01-01（无实际访视）
#' C2D1计划日期：2024-01-01 + 28天 = 2024-01-29
#' C2D1实际日期：2024-02-02（延后访视）
#' C3D1计划日期：2024-02-02 + 28天 = 2024-03-01（基于C2D1实际日期）
#' ```
#'
#' #### **非D1访视（如D8, D15, D21）**
#'
#' 计划日期 = 该周期D1的基准日期 + (访视日 - 1)
#'
#' **基准日期选择规则**：
#' 1. 优先使用该周期D1的**实际访视日期**（如果存在）
#' 2. 否则使用该周期D1的**计划日期**
#'
#' **示例**：
#' ```
#' C2D1计划日期：2024-01-29
#' C2D1实际日期：2024-02-02
#'
#' C2D8计划日期：2024-02-02 + 7天 = 2024-02-09（基于D1实际日期）
#' C2D15计划日期：2024-02-02 + 14天 = 2024-02-16（基于D1实际日期）
#' ```
#'
#' ### 3. 治疗结束访视（End of Treatment）
#'
#' - **EOT**：计划日期 = 治疗结束日期（eot_date）
#' - **EOT+数字**：计划日期 = 治疗结束日期 + 天数
#'   - 例如：EOT+7 = eot_date + 7天
#' - **EOS**：计划日期 = 研究结束日期（eos_date）
#'
#' ### 4. 随访访视（Follow-up）
#'
#' - **EOT+数字**：计划日期 = 治疗结束日期 + 天数
#'   - 例如：EOT+30 = eot_date + 30天
#' - **Last Dose+数字** 或 **LD+数字**：计划日期 = 末次用药日期 + 天数
#'   - 例如：LD+30 = last_dose_date + 30天
#'   - 例如：Last Dose+60 = last_dose_date + 60天
#'
#' ## 窗口期计算
#'
#' 根据访视编码数据中的窗口期类型计算窗口期范围：
#'
#' | 窗口期类型 | 窗口开始 | 窗口结束 | 说明 | 示例 |
#' |-----------|---------|---------|------|------|
#' | `±3d` | 计划日期-3 | 计划日期+3 | 前后各3天 | 计划2024-01-10，窗口2024-01-07至2024-01-13 |
#' | `+2d` | 计划日期 | 计划日期+2 | 只能延后 | 计划2024-01-10，窗口2024-01-10至2024-01-12 |
#' | `-2d` | 计划日期-2 | 计划日期 | 只能提前 | 计划2024-01-10，窗口2024-01-08至2024-01-10 |
#' | `≤2d` | 计划日期-2 | 计划日期 | 最多提前2天 | 计划2024-01-10，窗口2024-01-08至2024-01-10 |
#' | `≥2d` | 计划日期 | 计划日期+2 | 最多延后2天 | 计划2024-01-10，窗口2024-01-10至2024-01-12 |
#'
#' **窗口期用途**：用于判断实际访视是否在允许的时间范围内，超出窗口期视为方案偏离。
#'
#' ## 首次和末次给药日期计算
#'
#' ### **首次给药日期（first_dose_date）**
#' - 从所有指定的试验用药数据集（ex_datasets）中提取每个受试者的**最早**用药开始日期
#' - 始终使用**给药开始日期**变量（ex_date_var）
#' - 支持多个用药数据集，会自动合并并取最小值
#'
#' **示例**：
#' ```r
#' EX1数据：受试者001，用药日期 2024-01-01、2024-01-29
#' EX2数据：受试者001，用药日期 2024-01-03
#' 首次给药日期 = min(2024-01-01, 2024-01-29, 2024-01-03) = 2024-01-01
#' ```
#'
#' ### **末次给药日期（last_dose_date）**
#' - 根据是否指定给药结束日期变量（ex_end_date_var）采用不同逻辑：
#'
#' **情况1：未指定 ex_end_date_var（默认）**
#' - 使用给药开始日期（ex_date_var）
#' - 从所有用药记录中取**最晚**的给药开始日期
#'
#' **情况2：指定了 ex_end_date_var**
#' - 使用给药结束日期（ex_end_date_var）
#' - 从所有用药记录中取**最晚**的给药结束日期
#' - 适用于有明确给药开始和结束日期的研究
#'
#' **示例**：
#' ```r
#' # 情况1：只有开始日期
#' EX数据：受试者001
#'   - 2024-01-01（开始）
#'   - 2024-01-29（开始）
#' 末次给药日期 = max(2024-01-01, 2024-01-29) = 2024-01-29
#'
#' # 情况2：有开始和结束日期
#' EX数据：受试者001
#'   - 2024-01-01（开始）→ 2024-01-03（结束）
#'   - 2024-01-29（开始）→ 2024-02-01（结束）
#' 末次给药日期 = max(2024-01-03, 2024-02-01) = 2024-02-01
#' ```
#'
#' **参数配置**：
#' - 支持多个数据集：`ex_datasets = c("EX1", "EX2")`
#' - 可为每个数据集指定不同的列名：
#'   ```r
#'   ex_date_var = c("EXSTDAT1", "EXSTDAT2")
#'   ex_end_date_var = c("EXENDAT1", "EXENDAT2")
#'   ```
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
#' @param ex_end_date_var Character vector，用药结束日期变量名向量（默认: NULL）。
#'   - 如果为 NULL 或空向量，则末次给药日期使用 \code{ex_date_var}（给药开始日期）
#'   - 如果指定，则末次给药日期使用给药结束日期
#'   - 如果长度为1，则所有数据集使用同一列名
#'   - 如果长度等于 \code{ex_datasets} 的长度，则与数据集一一对应
#'   - 例如：c("EXENDAT1", "EXENDAT2") 分别对应 c("EX1", "EX2")
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
#'     \item{first_dose_date}{受试者首次用药日期}
#'     \item{last_dose_date}{受试者末次用药日期}
#'     \item{eot_date}{受试者治疗结束日期}
#'     \item{eos_date}{受试者研究结束日期}
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
