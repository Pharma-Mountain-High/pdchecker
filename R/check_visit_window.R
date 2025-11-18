#' 检查访视超窗
#'
#' @description
#' 检查已完成的访视是否在规定的窗口期内进行。接收 \code{\link{generate_planned_visit_dates}} 函数生成的
#' 计划访视数据，检查实际访视日期是否在窗口期范围内（wp_start 到 wp_end）。
#'
#' @details
#' ## 使用流程
#'
#' 该函数设计为与 \code{generate_planned_visit_dates} 配合使用：
#'
#' ```r
#' # 步骤1：生成计划访视日期
#' planned_dates <- generate_planned_visit_dates(
#'   data = study_data,
#'   visit_schedule_data = visit_codes
#' )
#'
#' # 步骤2：检查访视超窗
#' result <- check_visit_window(
#'   planned_dates = planned_dates
#' )
#' ```
#'
#' ## 超窗判断标准
#'
#' 对于已完成的访视（status == "completed"）：
#' - 如果实际访视日期在窗口期内（wp_start <= actual_date <= wp_end）：符合方案
#' - 如果实际访视日期在窗口期外（actual_date < wp_start 或 actual_date > wp_end）：超窗偏差
#'
#' ## 窗口期类型
#'
#' 窗口期由 \code{generate_planned_visit_dates} 函数根据访视编码数据自动计算：
#' - ±3d：计划日期前后各3天
#' - +2d：只能延后2天
#' - -2d：只能提前2天
#' - ≤2d：最多提前2天
#' - ≥2d：最多延后2天
#'
#' @param planned_dates Data frame，由 \code{\link{generate_planned_visit_dates}} 函数生成的计划访视数据。
#'   必须包含以下列：SUBJID, VISIT, VISITNUM, planned_date, wp_start, wp_end, wp_type, wp_value, actual_date, status
#'
#' @return List，包含以下组件：
#'   \describe{
#'     \item{has_deviation}{逻辑值。TRUE表示有超窗访视，FALSE表示无超窗访视}
#'     \item{messages}{字符向量。偏差描述信息}
#'     \item{details}{Data frame。超窗访视详细信息，每个超窗访视一条记录，包含以下列：
#'       \itemize{
#'         \item SUBJID: 受试者ID
#'         \item first_dose_date: 受试者的首次用药日期
#'         \item VISIT: 超窗的访视名称
#'         \item VISITNUM: 访视编号
#'         \item planned_date: 计划访视日期
#'         \item actual_date: 实际访视日期
#'         \item wp_start: 窗口期开始日期
#'         \item wp_end: 窗口期结束日期
#'         \item wp_type: 窗口期类型
#'         \item wp_value: 窗口期值
#'         \item deviation_days: 偏离计划日期的天数（正数表示延后，负数表示提前）
#'         \item total_completed_visits: 该受试者已完成的访视总数
#'         \item out_of_window_count: 超窗访视数量
#'       }
#'     }
#'     \item{planned_visits}{Data frame。输入的完整计划访视信息}
#'   }
#'
#' @importFrom dplyr filter select mutate group_by ungroup pull distinct
#' @importFrom magrittr %>%
#' @export
#'
check_visit_window <- function(planned_dates) {
  # 验证输入参数
  if (!is.data.frame(planned_dates)) {
    stop("planned_dates 必须是 data frame 类型")
  }

  # 检查必要的列
  required_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "planned_date",
    "wp_start", "wp_end", "wp_type", "wp_value",
    "actual_date", "status"
  )
  missing_cols <- setdiff(required_cols, names(planned_dates))
  if (length(missing_cols) > 0) {
    stop("planned_dates 缺少必要的列: ", paste(missing_cols, collapse = ", "))
  }

  # Initialize results
  results <- list(
    has_deviation = FALSE,
    messages = character(),
    details = data.frame(),
    planned_visits = planned_dates # 存储输入的计划访视信息
  )

  # 只检查已完成的访视
  completed_visits <- dplyr::filter(
    planned_dates,
    status == "completed" & !is.na(actual_date)
  )

  # 如果没有已完成的访视，直接返回
  if (nrow(completed_visits) == 0) {
    class(results) <- c("visit_window_check", "list")
    return(results)
  }

  # 为每个受试者检查超窗访视
  subjects <- unique(completed_visits$SUBJID)

  subject_deviations_list <- lapply(subjects, function(subj_id) {
    # 获取该受试者的已完成访视
    subj_completed <- dplyr::filter(completed_visits, SUBJID == subj_id)

    # 检查每个访视是否在窗口期内
    # 窗口期内：wp_start <= actual_date <= wp_end
    out_of_window <- dplyr::filter(
      subj_completed,
      !is.na(wp_start) & !is.na(wp_end) &
        (actual_date < wp_start | actual_date > wp_end)
    )

    # 如果有超窗访视，构建结果
    if (nrow(out_of_window) > 0) {
      # 获取受试者的首次用药日期
      first_dose_date <- unique(subj_completed$first_dose_date)[1]

      # 为每个超窗访视创建一条记录
      out_of_window_records <- lapply(seq_len(nrow(out_of_window)), function(i) {
        visit_name <- out_of_window$VISIT[i]
        visitnum <- out_of_window$VISITNUM[i]
        planned_date <- out_of_window$planned_date[i]
        actual_date <- out_of_window$actual_date[i]
        wp_start <- out_of_window$wp_start[i]
        wp_end <- out_of_window$wp_end[i]
        wp_type <- out_of_window$wp_type[i]
        wp_value <- out_of_window$wp_value[i]

        # 计算偏差天数（基于计划日期）
        deviation_days <- as.numeric(actual_date - planned_date)

        data.frame(
          SUBJID = subj_id,
          first_dose_date = first_dose_date,
          VISIT = visit_name,
          VISITNUM = visitnum,
          planned_date = planned_date,
          actual_date = actual_date,
          wp_start = wp_start,
          wp_end = wp_end,
          wp_type = wp_type,
          wp_value = wp_value,
          deviation_days = deviation_days,
          total_completed_visits = nrow(subj_completed),
          out_of_window_count = nrow(out_of_window),
          stringsAsFactors = FALSE
        )
      })

      return(do.call(rbind, out_of_window_records))
    } else {
      return(data.frame())
    }
  })

  # 合并所有受试者的结果
  if (length(subject_deviations_list) > 0) {
    # 过滤掉空的数据框
    non_empty_lists <- subject_deviations_list[sapply(subject_deviations_list, nrow) > 0]
    if (length(non_empty_lists) > 0) {
      subject_deviations <- do.call(rbind, non_empty_lists)
    } else {
      subject_deviations <- data.frame()
    }
  } else {
    subject_deviations <- data.frame()
  }

  # Compile results
  if (nrow(subject_deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "基于计划访视日期检查，访视超窗"
    results$details <- subject_deviations
  }

  class(results) <- c("visit_window_check", "list")
  return(results)
}


#' Print method for visit window check results
#' @param x Object of class visit_window_check
#' @param ... Additional arguments
#' @export
print.visit_window_check <- function(x, ...) {
  cat("8.4 访视超窗检查\n")
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
      first_dose <- if (!is.na(row["first_dose_date"])) {
        row["first_dose_date"]
      } else {
        "未记录"
      }

      # 构建窗口期描述
      wp_type <- row["wp_type"]
      wp_value <- row["wp_value"]
      if (!is.na(wp_type) && !is.na(wp_value)) {
        window_desc <- sprintf("%s%s", wp_type, wp_value)
      } else {
        window_desc <- "未定义"
      }

      # 计算偏差天数的绝对值
      deviation_days <- abs(as.numeric(row["deviation_days"]))

      deviation_text <- sprintf(
        "偏离计划时间点%d天，不在<窗口期%s>天内",
        deviation_days,
        window_desc
      )

      visit_info <- sprintf(
        "%s(计划日期:%s,实际日期:%s),%s",
        row["VISIT"],
        row["planned_date"],
        row["actual_date"],
        deviation_text
      )

      sprintf(
        "受试者编号%s，首次用药时间为%s，%s",
        row["SUBJID"],
        first_dose,
        visit_info
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
