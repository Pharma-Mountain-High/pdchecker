#' 解析窗口期字符串
#'
#' @param window_str 窗口期字符串 (如: "±3d", "≤24h", "+2天", "-1d")
#' @return 包含窗口期类型和数值的列表
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

#' 读取访视编码数据并解析窗口期
#'
#' @description
#' 读取访视编码Excel或CSV文件，解析窗口期列为两个新变量：
#' - type：区分 ≤、±、+、-、固定、范围等类型
#' - wpvalue：具体的窗口时间数值（自动转换h为天数）
#'
#' 支持的窗口期格式示例：
#' - "±3d" → 类型: ±, 数值: 3
#' - "≤24h" → 类型: ≤, 数值: 1
#' - "+2天" → 类型: +, 数值: 2
#' - "-1d" → 类型: -, 数值: 1
#' - "1w" → 类型: +, 数值: 7 （1周 = 7天）
#' - "±2周" → 类型: ±, 数值: 14 （2周 = 14天）
#'
#' @details
#' ## 窗口期列识别
#' 函数会自动查找以下列名（按顺序）：窗口期、访视窗口、窗口、window、Window。
#' 
#' ## 时间单位转换
#' 所有时间单位统一转换为天数：
#' - 小时（h、小时）：除以 24
#' - 天（d、天、日）：保持不变
#' - 周（w、周）：乘以 7
#' 
#' ## 编码支持
#' CSV 文件默认使用 UTF-8 编码读取，支持中文列名和内容。
#' 
#' ## 列名冲突处理
#' 如果数据中已存在 type 或 wpvalue 列，将被覆盖并提示消息。
#'
#' @param file_path 文件路径（.xlsx, .xls 或 .csv）
#' @param sheet_name Excel工作表名称（默认: "Sheet1"）。注意：CSV文件会忽略此参数。
#' @return 一个 tibble 对象，包含输入文件的所有列，并新增（或覆盖）两列：
#'   \item{type}{窗口期类型（字符型），可能的值：±、≤、≥、+、-、范围、其他、NA}
#'   \item{wpvalue}{窗口期数值（字符型），以天为单位。无法解析时为 "NA"}
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
    data <- tryCatch({
      # 优先尝试 UTF-8 编码
      read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    }, error = function(e) {
      # 如果失败，尝试系统默认编码
      read.csv(file_path, stringsAsFactors = FALSE)
    })
    # 转换为tibble以保持一致性
    data <- tibble::as_tibble(data)
  } else {
    stop("不支持的文件格式。请使用 .xlsx, .xls 或 .csv 文件。", call. = FALSE)
  }

  # 查找窗口期列（支持多种可能的列名）
  window_col_names <- c("窗口期", "访视窗口", "窗口", "window", "Window")
  window_col <- NULL

  for (col_name in window_col_names) {
    if (col_name %in% names(data)) {
      window_col <- col_name
      break
    }
  }

  if (is.null(window_col)) {
    stop("文件中缺少窗口期列。请确保文件包含以下列名之一: ",
         paste(window_col_names, collapse = ", "), call. = FALSE)
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
                             wpvalue = NA_character_)

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

