#' Parse Visit Window Period String
#'
#' @param window_str Window period string (e.g., "+/-3d", "<=24h", "+2d", "-1d")
#' @return List containing window type and value
#' @noRd
parse_window_period <- function(window_str) {
  # 处理缺失值
  if (is.na(window_str) || window_str == "" || is.null(window_str)) {
    return(list(type = NA, value = NA))
  }

  # 转换为字符串并清理空格
  window_str <- trimws(as.character(window_str))

  # 解析时间单位，转换为天数
  parse_time_unit <- function(value_str) {
    if (grepl("h$|小时$", value_str)) {
      # 小时转天
      num <- as.numeric(gsub("h$|小时$", "", value_str))
      return(num / 24)
    } else if (grepl("w$|周$", value_str)) {
      # 周转天
      num <- as.numeric(gsub("w$|周$", "", value_str))
      return(num * 7)
    } else if (grepl("d$|天$|日$", value_str)) {
      # 天数
      num <- as.numeric(gsub("d$|天$|日$", "", value_str))
      return(num)
    } else {
      # 默认为天
      return(as.numeric(value_str))
    }
  }

  # 判断窗口期类型
  if (grepl("^±", window_str)) {
    # ± 类型 (如: ±3d, ±24h)
    value_part <- gsub("^±", "", window_str)
    value <- parse_time_unit(value_part)
    return(list(type = "±", value = value))
  } else if (grepl("^≤|^<=", window_str)) {
    # ≤ 类型 (如: ≤24h, ≤1d)
    value_part <- gsub("^≤|^<=", "", window_str)
    value <- parse_time_unit(value_part)
    return(list(type = "≤", value = value))
  } else if (grepl("^≥|^>=", window_str)) {
    # ≥ 类型 (如: ≥1d)
    value_part <- gsub("^≥|^>=", "", window_str)
    value <- parse_time_unit(value_part)
    return(list(type = "≥", value = value))
  } else if (grepl("^\\+", window_str)) {
    # + 类型 (如: +2d)
    value_part <- gsub("^\\+", "", window_str)
    value <- parse_time_unit(value_part)
    return(list(type = "+", value = value))
  } else if (grepl("^-", window_str) && !grepl("到|至", window_str)) {
    # - 类型 (如: -1d)
    value_part <- gsub("^-", "", window_str)
    value <- parse_time_unit(value_part)
    return(list(type = "-", value = value))
  } else if (grepl("到|至", window_str)) {
    # 范围类型 (如: -2到+4, 1至3天)
    return(list(type = "范围", value = window_str))
  } else if (grepl("^[0-9]", window_str)) {
    # 数字无前缀类型 (如: 2d)
    value_part <- window_str
    value <- parse_time_unit(value_part)
    return(list(type = "+", value = value))
  } else {
    # 其他格式
    return(list(type = "其他", value = window_str))
  }
}

#' Read Visit Schedule Data and Parse Window Periods
#'
#' @description
#' Read visit schedule Excel or CSV file and parse window period column into two new variables:
#' - type: Distinguishes window types (<=, +/-, +, -, fixed, range, etc.)
#' - wpvalue: Specific window time value (automatically converts hours to days)
#'
#' Supported window period format examples:
#' - "+/-3d" -> type: +/-, value: 3
#' - "<=24h" -> type: <=, value: 1
#' - "+2d" -> type: +, value: 2
#' - "-1d" -> type: -, value: 1
#' - "1w" -> type: +, value: 7 (1 week = 7 days)
#' - "+/-2w" -> type: +/-, value: 14 (2 weeks = 14 days)
#'
#' @details
#' ## Window Period Column Detection
#' Function automatically searches for column names (in order): window, Window, WINDOW.
#'
#' ## Time Unit Conversion
#' All time units are converted to days:
#' - Hours (h): divided by 24
#' - Days (d): unchanged
#' - Weeks (w): multiplied by 7
#'
#' ## Encoding Support
#' CSV files default to UTF-8 encoding, with fallback to system default.
#'
#' ## Column Name Conflicts
#' If type or wpvalue columns already exist, they will be overwritten with a message.
#'
#' @param file_path File path (.xlsx, .xls, or .csv)
#' @param sheet_name Excel sheet name (default: "Sheet1"). Ignored for CSV files.
#' @return A tibble with all columns from input file, plus two new (or overwritten) columns:
#'   \item{type}{Window type (character): +/-, <=, >=, +, -, range, other, or NA}
#'   \item{wpvalue}{Window value (character) in days. "NA" if unparseable}
#' @importFrom utils read.csv
#' @export
read_visitcode_file <- function(file_path, sheet_name = "Sheet1") {
  # 检查文件是否存在
  if (!file.exists(file_path)) {
    stop("文件未找到: ", file_path, call. = FALSE)
  }

  # 读取文件
  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext %in% c("xlsx", "xls")) {
    # 读取Excel文件
    data <- readxl::read_excel(file_path, sheet = sheet_name)
  } else if (file_ext == "csv") {
    # 读取CSV文件，使用UTF-8编码支持中文
    data <- tryCatch(
      {
        # 优先尝试 UTF-8 编码
        read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
      },
      error = function(e) {
        # 如果失败，尝试系统默认编码
        read.csv(file_path, stringsAsFactors = FALSE)
      }
    )
    # 转换为tibble以保持一致性
    data <- tibble::as_tibble(data)
  } else {
    stop("不支持的文件格式。请使用 .xlsx, .xls 或 .csv 文件。", call. = FALSE)
  }

  # 查找窗口期列（支持多种可能的列名）
  window_col_names <- c("窗口期", "访视窗口", "窗口", "window", "Window", "WINDOW")
  window_col <- NULL

  for (col_name in window_col_names) {
    if (col_name %in% names(data)) {
      window_col <- col_name
      break
    }
  }

  if (is.null(window_col)) {
    stop("文件中缺少窗口期列。请确保文件包含以下列名之一: ",
      paste(window_col_names, collapse = ", "),
      call. = FALSE
    )
  }

  # 处理列名冲突：如果已存在 type 或 wpvalue 列，先删除
  if ("type" %in% names(data)) {
    message("注意：数据中已存在 'type' 列，将被覆盖。")
    data <- data[, !names(data) %in% "type"]
  }
  if ("wpvalue" %in% names(data)) {
    message("注意：数据中已存在 'wpvalue' 列，将被覆盖。")
    data <- data[, !names(data) %in% "wpvalue"]
  }

  # 初始化新列
  data <- tibble::add_column(data,
    type = NA_character_,
    wpvalue = NA_character_
  )

  # 逐行解析窗口期
  if (nrow(data) > 0) {
    for (i in seq_len(nrow(data))) {
      window_str <- data[[window_col]][i]

      # 解析窗口期
      result <- parse_window_period(window_str)
      data[i, "type"] <- result$type
      data[i, "wpvalue"] <- as.character(result$value)
    }
  }

  return(data)
}
