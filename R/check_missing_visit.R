#' Check Missing Visits
#'
#' @description
#' Check for missing visits based on planned visit dates. Receives planned visit data
#' from \code{\link{generate_planned_visit_dates}} and uses different cutoff date
#' criteria based on visit type to determine if a visit should have been completed.
#'
#' @details
#' ## Usage Workflow
#'
#' This function is designed to work with \code{generate_planned_visit_dates}:
#'
#' ```r
#' # Step 1: Generate planned visit dates
#' planned_dates <- generate_planned_visit_dates(
#'   data = study_data,
#'   visit_schedule_data = visit_codes
#' )
#'
#' # Step 2: Check missing visits
#' result <- check_missing_visit(
#'   planned_dates = planned_dates,
#'   cutoffdt = as.Date("2024-12-31")
#' )
#' ```
#'
#' ## Visit Completion Criteria
#'
#' Different visit types use different termination date criteria:
#'
#' ### Screening and Treatment Visits
#' Planned date must be < min(end of treatment date, end of study date, cutoff date)
#'
#' ### End of Treatment and Follow-up Visits
#' Planned date must be <= min(end of study date, cutoff date)
#'
#' ## Visit Type Classification
#'
#' Visit types are automatically identified by \code{generate_planned_visit_dates}:
#' - screening: Screening visits
#' - treatment: Treatment visits
#' - end_of_treatment: End of treatment visits
#' - follow_up: Follow-up visits
#'
#' @param planned_dates Data frame from \code{\link{generate_planned_visit_dates}}.
#'   Must contain: SUBJID, VISIT, VISITNUM, visittype, planned_date, status, eot_date, eos_date
#' @param cutoffdt Date, data cutoff date (default: current date). Used to determine if visit should be completed
#'
#' @return List with the following components:
#'   \describe{
#'     \item{has_deviation}{Logical. TRUE if there are missing visits}
#'     \item{messages}{Character vector. Deviation description messages}
#'     \item{details}{Data frame. Missing visit details with columns:
#'       \itemize{
#'         \item SUBJID: Subject ID
#'         \item first_dose_date: Subject's first dose date
#'         \item VISIT: Missing visit name
#'         \item VISITNUM: Visit number
#'         \item planned_date: Planned visit date
#'         \item visittype: Visit type
#'         \item eot_date: Subject's end of treatment date
#'         \item eos_date: Subject's end of study date
#'         \item cutoffdt: Data cutoff date
#'         \item valid_visits_count: Total visits that should be completed
#'         \item completed_visits_count: Actual completed visits
#'       }
#'     }
#'     \item{planned_visits}{Data frame. Complete input planned visit information}
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
    first_dose_date <- unique(subj_planned$first_dose_date)[1]
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
      # 计算完成的访视数（只计算有效计划访视中的）
      completed_subset <- dplyr::filter(valid_planned_visits, status == "completed")
      completed_count <- nrow(completed_subset)

      # 为每个缺失访视创建一条记录
      missing_records <- lapply(seq_len(nrow(missing_visit_subset)), function(i) {
        data.frame(
          SUBJID = subj_id,
          first_dose_date = first_dose_date,
          VISIT = missing_visit_subset$VISIT[i],
          VISITNUM = missing_visit_subset$VISITNUM[i],
          planned_date = missing_visit_subset$planned_date[i],
          visittype = missing_visit_subset$visittype[i],
          eot_date = eot_date,
          eos_date = eos_date,
          cutoffdt = cutoffdt,
          valid_visits_count = nrow(valid_planned_visits),
          completed_visits_count = completed_count,
          stringsAsFactors = FALSE
        )
      })

      return(do.call(rbind, missing_records))
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
    deviations <- dplyr::filter(subject_visits, !is.na(VISIT))
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
      visit_info <- sprintf("%s(%s)", row["VISIT"], row["planned_date"])
      first_dose <- if (!is.na(row["first_dose_date"])) row["first_dose_date"] else "未记录"
      sprintf(
        "受试者编号%s，首次用药时间为%s，计划进行的%s访视遗漏。",
        row["SUBJID"],
        first_dose,
        visit_info
      )
    })
    cat(formatted_details, sep = "\n")
  }
}
