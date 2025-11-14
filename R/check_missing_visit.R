#' 检查缺失访视
#'
#' @description
#' 基于计划访视日期检查缺失访视。接收 \code{\link{generate_planned_visit_dates}} 函数生成的
#' 计划访视数据，根据不同访视类型使用不同的截止日期标准判断访视是否应该完成。
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
#' # 步骤2：检查缺失访视
#' result <- check_missing_visit(
#'   planned_dates = planned_dates,
#'   cutoffdt = as.Date("2024-12-31")
#' )
#' ```
#'
#' ## 访视完成判断标准
#'
#' 不同类型的访视使用不同的终止日期判断标准：
#'
#' ### 筛选期和治疗期访视
#' 计划日期必须 < min(治疗结束日期, 研究结束日期, 数据截止日期)
#'
#' ### 治疗结束和随访期访视
#' 计划日期必须 <= min(研究结束日期, 数据截止日期)
#'
#' ## 访视类型识别
#'
#' 访视类型由 \code{generate_planned_visit_dates} 函数自动识别和分类：
#' - screening: 筛选期访视
#' - treatment: 治疗期访视
#' - end_of_treatment: 治疗结束访视
#' - follow_up: 随访访视
#'
#' @param planned_dates Data frame，由 \code{\link{generate_planned_visit_dates}} 函数生成的计划访视数据。
#'   必须包含以下列：SUBJID, VISIT, VISITNUM, visittype, planned_date, status, eot_date, eos_date
#' @param cutoffdt Date 类型，数据截止日期（默认: 当前日期）。用于判断访视是否应该完成
#'
#' @return List，包含以下组件：
#'   \describe{
#'     \item{has_deviation}{逻辑值。TRUE表示有缺失访视，FALSE表示无缺失访视}
#'     \item{messages}{字符向量。偏差描述信息}
#'     \item{details}{Data frame。缺失访视详细信息，包含以下列：
#'       \itemize{
#'         \item SUBJID: 受试者ID
#'         \item first_ex_date: 受试者的首次给药日期
#'         \item missing_visits: 缺失的访视名称及计划日期（格式：访视名称(计划日期)，多个访视用"、"分隔）
#'         \item eot_date: 受试者的治疗结束日期
#'         \item eos_date: 受试者的研究结束日期
#'         \item cutoffdt: 数据截止日期
#'         \item valid_visits_count: 应该完成的访视总数
#'         \item completed_visits_count: 实际完成的访视数
#'       }
#'     }
#'     \item{planned_visits}{Data frame。输入的完整计划访视信息}
#'   }
#'
#' @importFrom dplyr filter select mutate group_by ungroup pull distinct
#' @importFrom magrittr %>%
#' @export
#'
check_missing_visit <- function(planned_dates,
                                cutoffdt = Sys.Date()) {
  # 验证输入参数
  if (!is.data.frame(planned_dates)) {
    stop("planned_dates 必须是 data frame 类型")
  }

  # 检查必要的列
  required_cols <- c(
    "SUBJID", "VISIT", "VISITNUM", "visittype", "planned_date",
    "status", "eot_date", "eos_date"
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

  # 确保数据截止日期为Date类型
  if (!inherits(cutoffdt, "Date")) {
    cutoffdt <- as.Date(cutoffdt)
  }

  # 为计划访视数据添加访视类型分类
  # 使用 utils.R 中的共享函数 match_visit_type
  planned_dates$visit_category <- sapply(planned_dates$visittype, match_visit_type)

  # 为每个受试者检查缺失访视
  subjects <- unique(planned_dates$SUBJID)

  subject_visits_list <- lapply(subjects, function(subj_id) {
    # 获取该受试者的计划访视
    subj_planned <- dplyr::filter(planned_dates, SUBJID == subj_id)

    # 为不同类型的访视计算不同的终止日期
    # 筛选期和治疗期：min(eotdate, eosdate, cutoffdt)
    # 治疗结束、退出研究、随访期：min(eosdate, cutoffdt)

    # 获取该受试者的各种日期
    first_ex_date <- unique(subj_planned$first_ex_date)[1]
    eot_date <- unique(subj_planned$eot_date)[1]
    eos_date <- unique(subj_planned$eos_date)[1]

    # 计算筛选期和治疗期访视的终止日期：min(eotdate, eosdate, cutoffdt)
    trtmax_dates <- c(cutoffdt)
    if (!is.na(eot_date)) {
      trtmax_dates <- c(trtmax_dates, eot_date)
    }
    if (!is.na(eos_date)) {
      trtmax_dates <- c(trtmax_dates, eos_date)
    }
    cutoffdt_trt <- as.Date(min(trtmax_dates, na.rm = TRUE), origin = "1970-01-01")

    # 计算治疗结束、退出研究、随访期访视的终止日期：min(eosdate, cutoffdt)
    followup_dates <- c(cutoffdt)
    if (!is.na(eos_date)) {
      followup_dates <- c(followup_dates, eos_date)
    }
    cutoffdt_fu <- as.Date(min(followup_dates, na.rm = TRUE), origin = "1970-01-01")

    # 根据访视类型筛选应该检查的访视
    # 不同类型的访视有不同的终止日期要求
    valid_planned_visits <- dplyr::filter(
      subj_planned,
      !is.na(planned_date) &
        (
          # 筛选期和治疗期：计划日期必须 < cutoffdt_trt
          (visit_category %in% c("screening", "treatment") & planned_date < cutoffdt_trt) |
            # 治疗结束和随访期：计划日期必须 <= cutoffdt_fu
            (visit_category %in% c("end_of_treatment", "follow_up") & planned_date <= cutoffdt_fu)
        )
    )

    # 在有效访视中，找出缺失的访视（应该完成但未完成的）
    missing_visit_subset <- dplyr::filter(valid_planned_visits, status == "missing")

    # 构建结果 - 报告根据访视类型判断应该完成但未完成的访视
    if (nrow(missing_visit_subset) > 0) {
      # 构建带计划日期的缺失访视文本：访视名称(计划日期)
      missing_with_dates <- sapply(seq_len(nrow(missing_visit_subset)), function(i) {
        visit_name <- missing_visit_subset$VISIT[i]
        planned_date <- missing_visit_subset$planned_date[i]
        sprintf("%s(%s)", visit_name, as.character(planned_date))
      })
      missing_text <- paste(missing_with_dates, collapse = "、")

      # 计算完成的访视数（只计算有效计划访视中的）
      completed_subset <- dplyr::filter(valid_planned_visits, status == "completed")
      completed_count <- nrow(completed_subset)

      return(data.frame(
        SUBJID = subj_id,
        first_ex_date = first_ex_date,
        missing_visits = missing_text,
        eot_date = eot_date,
        eos_date = eos_date,
        cutoffdt = cutoffdt,
        valid_visits_count = nrow(valid_planned_visits),
        completed_visits_count = completed_count,
        stringsAsFactors = FALSE
      ))
    } else {
      return(data.frame())
    }
  })

  # 合并所有受试者的结果
  if (length(subject_visits_list) > 0) {
    # 过滤掉空的数据框
    non_empty_lists <- subject_visits_list[sapply(subject_visits_list, nrow) > 0]
    if (length(non_empty_lists) > 0) {
      subject_visits <- do.call(rbind, non_empty_lists)
    } else {
      subject_visits <- data.frame()
    }
  } else {
    subject_visits <- data.frame()
  }

  # Get deviations (subjects with missing visits)
  # 检查数据框是否为空
  if (nrow(subject_visits) > 0) {
    deviations <- dplyr::filter(subject_visits, !is.na(missing_visits))
  } else {
    deviations <- data.frame()
  }

  # Compile results
  if (nrow(deviations) > 0) {
    results$has_deviation <- TRUE
    results$messages <- "基于计划访视日期检查，未按计划进行访视"
    results$details <- deviations
  }

  class(results) <- c("missing_visit_check", "list")
  return(results)
}


#' Print method for missing visit check results
#' @param x Object of class missing_visit_check
#' @param ... Additional arguments
#' @export
print.missing_visit_check <- function(x, ...) {
  cat("8.2 按计划进行现场访视\n")
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
      missing_info <- if (!is.na(row["missing_visits"])) row["missing_visits"] else "无缺失访视"
      first_ex <- if (!is.na(row["first_ex_date"])) row["first_ex_date"] else "未记录"
      sprintf(
        "受试者编号%s，首次用药时间为%s，计划进行的%s访视遗漏。",
        row["SUBJID"],
        first_ex,
        missing_info
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
