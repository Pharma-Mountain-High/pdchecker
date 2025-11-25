#' 准备检查项数据用于缺失检查
#'
#' @description
#' 从包含所有数据的 list 中提取指定的检查项数据集，将其与访视数据集进行合并，
#' 并创建标准化的列名（TBNAME、TESTCAT、TESTDE、TESTDAT、TESTYN、ORRES）用于后续的缺失检查。
#'
#' @details
#' ## 使用流程
#'
#' 该函数用于 \code{\link{check_missing_test}} 之前的数据准备：
#'
#' ```r
#' # 步骤1：准备检查项数据
#' prepared_data <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB"
#' )
#'
#' # 步骤2：检查缺失
#' result <- check_missing_test(
#'   data = prepared_data,
#'   sv_data = sv_data,
#'   test_var = "LBTEST",
#'   test = "血常规"
#' )
#' ```
#'
#' ## 数据处理逻辑
#'
#' 1. 从 data list 中提取指定的检查项数据集（如 "LB"）
#' 2. 从 data list 中提取访视数据集（默认 "SV"），只保留必要的列：
#'    SUBJID、VISIT、VISITNUM、SVDAT
#' 3. 将检查项数据集通过 SUBJID、VISIT、VISITNUM left_join 到访视数据集
#' 4. 创建标准化列名：
#'    - TBNAME：数据集名称（映射自原始数据集名称列，或使用 test_dataset）
#'    - TESTCAT：检查分类（映射自原始检查分类列）
#'    - TESTDE：检查项目名称（映射自原始检查项目列），如"红细胞计数"等
#'    - TESTDAT：检查日期（映射自原始检查日期列）
#'    - TESTYN：是否进行检查（映射自原始是否检查列）
#'    - ORRES：检查结果（映射自原始检查结果列）
#' 5. 返回包含检查项数据集原始列和衍生列的数据集，列顺序为：
#'    SUBJID, VISIT, VISITNUM, TBNAME, TESTCAT, TESTDE, TESTYN, TESTDAT, ORRES, 检查项数据集的其他原始列
#'
#' ## 注意事项
#'
#' - 如果原始数据集中不存在某个映射列（如 test_yn_var），则对应的衍生列将为 NA
#' - left_join 基于访视数据集，因此返回的记录以访视记录为准
#' - SV 数据集只保留 SUBJID、VISIT、VISITNUM、SVDAT 列，其他列不会出现在返回结果中
#' - 检查项数据集的所有原始列都会保留在返回结果中
#' - 衍生列会按指定顺序排在前面，便于查看
#'
#' @param data List 类型，包含所有临床试验数据集的列表
#' @param test_dataset Character string，要提取的检查项数据集名称（如 "LB"）
#' @param test_date_var Character string，原始检查日期变量名（默认: "LBDAT"）
#' @param test_yn_var Character string，原始是否进行检查变量名（默认: "YN"）
#' @param test_result_var Character string，原始检查结果变量名（默认: "ORRES"）
#' @param test_cat_var Character string，原始检查分类变量名（默认: "LBCAT"）。
#'   如果为 NULL 或空字符串，则 TESTCAT 列将为 NA
#' @param test_de_var Character string，原始检查项目名称变量名（默认: NULL）。
#'   如果为 NULL 或空字符串，则 TESTDE 列将为 NA。用于表示具体的检查项目，如"红细胞计数"等
#' @param tb_name_var Character string，原始数据集名称标识变量名（默认: NULL）。
#'   如果为 NULL 或空字符串，则 TBNAME 列将使用 test_dataset 参数的值（如 "LB"）。
#'   用于标识检查数据集的名称
#' @param sv_dataset Character string，访视数据集名称（默认: "SV"）
#' @param sv_visit_var Character string，访视数据集中访视名称变量名（默认: "VISIT"）
#' @param sv_visitnum_var Character string，访视数据集中访视编号变量名（默认: "VISITNUM"）
#' @param sv_date_var Character string，访视数据集中访视日期变量名（默认: "SVDAT"）
#' @param config Character string 或 Data frame，检查项访视配置（默认: NULL）。
#'   如果为字符串，表示Excel配置文件路径；如果为数据框，则直接使用。
#'   配置文件需包含列：TESTCAT（检查分类）、VISITNUM（逗号分隔的访视编号）。
#'   如果提供配置，将根据配置预先生成应该存在的访视-检查项组合骨架，
#'   即使原始检查数据中没有相应记录，也会在输出中体现为空记录（TESTDAT=NA等）
#' @param config_cat Character vector，从配置文件中筛选的检查分类范围（默认: NULL）。
#'   如果为NULL，使用配置文件中的所有TESTCAT；
#'   如果提供值（如 c("血常规", "血生化")），则只使用配置中这些TESTCAT的记录。
#'   注意：只有在提供 config 参数时，此参数才有效
#' @param enrl_dataset Character string，入组信息数据集名称（默认: "ENROL"）。
#'   用于筛选入组受试者
#' @param enrl_yn_var Character string，入组数据集中是否入组变量名（默认: "ENRYN"）。
#'   只保留该变量值为"是"的受试者记录
#' @param filter_cond Character string，自定义筛选条件（默认: NULL）。
#'   格式为 "数据集名|筛选条件"，支持多个筛选条件用分号分隔，例如：
#'   - "SUBJECT|SEX=='男'" - 从SUBJECT数据集中筛选性别为男的受试者
#'   - "SUBJECT|AGE>=18" - 从SUBJECT数据集中筛选年龄大于等于18的受试者
#'   - "SUBJECT|SEX=='男' & AGE>=18" - 同一数据集的多个条件
#'   - "SUBJECT|SEX=='男';DM|AGE>18" - 从多个数据集筛选（取交集）
#'   如果为NULL，则不进行自定义筛选
#'
#' @return Data frame，列顺序为：
#'   \describe{
#'     \item{SUBJID}{受试者ID（来自 SV 数据集）}
#'     \item{VISIT}{访视名称（来自 SV 数据集）}
#'     \item{VISITNUM}{访视编号（来自 SV 数据集）}
#'     \item{TBNAME}{数据集名称（衍生列，映射自 tb_name_var 或使用 test_dataset）}
#'     \item{TESTCAT}{检查分类（衍生列，映射自 test_cat_var）}
#'     \item{TESTDE}{检查项目名称（衍生列，映射自 test_de_var），如"红细胞计数"等}
#'     \item{TESTYN}{是否进行检查（衍生列，映射自 test_yn_var）}
#'     \item{TESTDAT}{检查日期（衍生列，映射自 test_date_var）}
#'     \item{ORRES}{检查结果（衍生列，映射自 test_result_var）}
#'     \item{...}{检查项数据集的其他所有原始列}
#'   }
#'   注：SV 数据集只保留 SUBJID、VISIT、VISITNUM、SVDAT 列，其他列不会包含在返回结果中
#'
#' @importFrom dplyr left_join select mutate sym all_of bind_rows rename inner_join filter pull
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # 准备血常规检查数据
#' lb_prepared <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   test_de_var = "LBTEST", # 检查项目名称，如"红细胞计数"
#'   tb_name_var = NULL # 使用 test_dataset "LB" 作为数据集名称
#' )
#'
#' # 准备生命体征检查数据，使用原始数据中的数据集名称列
#' vs_prepared <- prepare_test_data(
#'   data = list(VS = vs_data, SV = sv_data),
#'   test_dataset = "VS",
#'   test_date_var = "VSDAT",
#'   test_result_var = "VSORRES",
#'   test_cat_var = NULL, # 如果没有分类变量
#'   test_de_var = "VSTEST", # 检查项目名称
#'   tb_name_var = "VSTB" # 如果原始数据中有数据集名称列
#' )
#'
#' # 使用配置文件预先生成应该存在的检查记录骨架
#' # 配置文件指定哪些检查项应该在哪些访视进行
#' config_df <- data.frame(
#'   TESTCAT = c("血常规", "血生化", "尿常规"),
#'   VISITNUM = c("2,3,5", "2,4", "2,5,10")
#' )
#' lb_prepared_with_config <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   test_de_var = "LBTEST",
#'   config = config_df # 或使用文件路径: config = "test_config.xlsx"
#' )
#' # 输出将包含所有应该存在的访视-检查项组合
#' # 即使原始数据中没有相应记录，也会显示为空记录（TESTDAT=NA等）
#'
#' # 只准备配置文件中特定的检查分类
#' lb_prepared_partial <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   test_de_var = "LBTEST",
#'   config = config_df,
#'   config_cat = c("血常规", "血生化") # 只使用血常规和血生化，忽略尿常规
#' )
#'
#' # 只保留入组受试者的记录
#' lb_prepared_enrolled <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data, ENROL = enrol_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   enrl_dataset = "ENROL", # 入组数据集
#'   enrl_yn_var = "ENRYN" # 是否入组变量
#' )
#' # 只保留 ENRYN="是" 的受试者记录
#'
#' # 使用自定义筛选条件筛选特定受试者
#' lb_prepared_male <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data, SUBJECT = subject_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   filter_cond = "SUBJECT|SEX=='男'" # 只保留男性受试者
#' )
#'
#' # 同一数据集的多个条件
#' lb_prepared_filtered <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data, SUBJECT = subject_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   filter_cond = "SUBJECT|SEX=='男' & AGE>=18 & AGE<=65"
#' )
#'
#' # 从多个数据集筛选（取交集）
#' lb_prepared_multi <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data, SUBJECT = subject_data, DM = dm_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   filter_cond = "SUBJECT|SEX=='男';DM|AGE>18" # 从SUBJECT筛选男性，从DM筛选年龄>18，取交集
#' )
#'
#' # 同时使用入组筛选和自定义筛选（取交集）
#' lb_prepared_both <- prepare_test_data(
#'   data = list(LB = lb_data, SV = sv_data, SUBJECT = subject_data, ENROL = enrol_data),
#'   test_dataset = "LB",
#'   test_date_var = "LBDAT",
#'   test_result_var = "ORRES",
#'   test_cat_var = "LBCAT",
#'   enrl_dataset = "ENROL",
#'   enrl_yn_var = "ENRYN",
#'   filter_cond = "SUBJECT|SEX=='男'" # 只保留既入组又是男性的受试者
#' )
#' }
#'
prepare_test_data <- function(data,
                              test_dataset,
                              test_date_var = "LBDAT",
                              test_yn_var = "YN",
                              test_result_var = "ORRES",
                              test_cat_var = "LBCAT",
                              test_de_var = NULL,
                              tb_name_var = NULL,
                              sv_dataset = "SV",
                              sv_visit_var = "VISIT",
                              sv_visitnum_var = "VISITNUM",
                              sv_date_var = "SVDAT",
                              config = NULL,
                              config_cat = NULL,
                              enrl_dataset = "ENROL",
                              enrl_yn_var = "ENRYN",
                              filter_cond = NULL) {
  # 验证输入参数
  if (!is.list(data)) {
    stop("data 必须是 list 类型")
  }

  if (missing(test_dataset) || is.null(test_dataset)) {
    stop("必须指定 test_dataset 参数")
  }

  # 检查必要的数据集是否存在
  if (!test_dataset %in% names(data)) {
    stop(paste0("data 中不存在检查项数据集: ", test_dataset))
  }

  if (!sv_dataset %in% names(data)) {
    stop(paste0("data 中不存在访视数据集: ", sv_dataset))
  }

  # 获取数据集
  test_data <- data[[test_dataset]]
  sv_data <- data[[sv_dataset]]

  # 处理自定义筛选条件
  filtered_subjids <- NULL
  if (!is.null(filter_cond) && filter_cond != "") {
    # 支持多个筛选条件，用分号分隔: "数据集名1|条件1;数据集名2|条件2"
    filter_conditions <- strsplit(filter_cond, ";", fixed = TRUE)[[1]]
    filter_conditions <- trimws(filter_conditions) # 去除每个条件前后的空格

    # 存储每个筛选条件的结果
    all_filtered_subjids <- list()

    # 处理每个筛选条件
    for (i in seq_along(filter_conditions)) {
      filter_item <- filter_conditions[i]

      # 解析单个筛选条件: "数据集名|筛选条件"
      filter_parts <- strsplit(filter_item, "\\|", fixed = FALSE)[[1]]

      if (length(filter_parts) != 2) {
        stop(paste0(
          "filter_cond 参数格式错误，第 ", i, " 个筛选条件应为 '数据集名|筛选条件'，",
          "例如 'SUBJECT|SEX==\"男\"'"
        ))
      }

      filter_ds_name <- trimws(filter_parts[1])
      filter_expr <- trimws(filter_parts[2])

      # 检查筛选数据集是否存在
      if (!filter_ds_name %in% names(data)) {
        stop(paste0("filter_cond 中指定的数据集不存在: ", filter_ds_name))
      }

      filter_ds <- data[[filter_ds_name]]

      # 检查筛选数据集是否包含 SUBJID
      if (!"SUBJID" %in% names(filter_ds)) {
        stop(paste0("筛选数据集 ", filter_ds_name, " 中缺少 SUBJID 列"))
      }

      # 应用筛选条件
      tryCatch(
        {
          filtered_ds <- filter_ds %>%
            filter(eval(parse(text = filter_expr)))

          subjids <- unique(filtered_ds$SUBJID)

          if (length(subjids) == 0) {
            warning(paste0("筛选条件 '", filter_item, "' 未匹配到任何受试者"))
          }

          all_filtered_subjids[[i]] <- subjids
        },
        error = function(e) {
          stop(paste0(
            "筛选条件 '", filter_item, "' 执行失败: ", e$message,
            "\n请检查筛选条件语法"
          ))
        }
      )
    }

    # 如果有多个筛选条件，取交集；如果只有一个，直接使用
    if (length(all_filtered_subjids) > 0) {
      filtered_subjids <- all_filtered_subjids[[1]]
      if (length(all_filtered_subjids) > 1) {
        for (i in 2:length(all_filtered_subjids)) {
          filtered_subjids <- intersect(filtered_subjids, all_filtered_subjids[[i]])
        }
      }

      if (length(filtered_subjids) == 0) {
        warning("所有筛选条件的交集为空，结果可能为空")
      }
    }
  }

  # 读取入组数据集，筛选入组受试者
  if (enrl_dataset %in% names(data)) {
    enrl_data <- data[[enrl_dataset]]

    # 检查入组数据集必要的列
    enrl_required_cols <- c("SUBJID", enrl_yn_var)
    enrl_missing_cols <- setdiff(enrl_required_cols, names(enrl_data))
    if (length(enrl_missing_cols) > 0) {
      warning(paste0(
        "入组数据集 ", enrl_dataset, " 缺少必要的列: ",
        paste(enrl_missing_cols, collapse = ", "),
        "，将不进行入组筛选"
      ))
      enrolled_subjids <- NULL
    } else {
      # 获取入组的受试者ID列表（ENRYN="是"）
      enrolled_subjids <- enrl_data %>%
        filter(!!sym(enrl_yn_var) == "是") %>%
        pull(SUBJID) %>%
        unique()

      if (length(enrolled_subjids) == 0) {
        warning("入组数据集中没有入组的受试者（", enrl_yn_var, " = '是'），结果可能为空")
      }
    }
  } else {
    # 如果入组数据集不存在，给出警告但继续处理
    warning(paste0("data 中不存在入组数据集: ", enrl_dataset, "，将不进行入组筛选"))
    enrolled_subjids <- NULL
  }

  # 合并入组筛选和自定义筛选的结果
  final_subjids <- NULL
  if (!is.null(enrolled_subjids) && !is.null(filtered_subjids)) {
    # 两个筛选条件取交集
    final_subjids <- intersect(enrolled_subjids, filtered_subjids)
    if (length(final_subjids) == 0) {
      warning("入组筛选和自定义筛选的交集为空，结果可能为空")
    }
  } else if (!is.null(enrolled_subjids)) {
    final_subjids <- enrolled_subjids
  } else if (!is.null(filtered_subjids)) {
    final_subjids <- filtered_subjids
  }

  # 检查访视数据集必要的列
  sv_required_cols <- c("SUBJID", sv_visit_var, sv_visitnum_var, sv_date_var)
  sv_missing_cols <- setdiff(sv_required_cols, names(sv_data))
  if (length(sv_missing_cols) > 0) {
    stop(paste0(
      "访视数据集 ", sv_dataset, " 缺少必要的列: ",
      paste(sv_missing_cols, collapse = ", ")
    ))
  }

  # 检查检查项数据集必要的列
  test_required_cols <- c("SUBJID", sv_visit_var, sv_visitnum_var)
  test_missing_cols <- setdiff(test_required_cols, names(test_data))
  if (length(test_missing_cols) > 0) {
    stop(paste0(
      "检查项数据集 ", test_dataset, " 缺少必要的列: ",
      paste(test_missing_cols, collapse = ", ")
    ))
  }

  # 从 SV 数据集中只保留需要的列，并重命名为标准列名
  sv_data_subset <- sv_data %>%
    select(
      SUBJID = SUBJID,
      VISIT = !!sym(sv_visit_var),
      VISITNUM = !!sym(sv_visitnum_var),
      SVDAT = !!sym(sv_date_var)
    ) %>%
    mutate(VISITNUM = as.character(VISITNUM)) # 统一为字符型

  # 如果有受试者筛选（入组筛选或自定义筛选），只保留符合条件的受试者
  if (!is.null(final_subjids) && length(final_subjids) > 0) {
    sv_data_subset <- sv_data_subset %>%
      filter(SUBJID %in% final_subjids)

    # 如果筛选后没有记录，给出警告
    if (nrow(sv_data_subset) == 0) {
      warning("筛选受试者后，访视数据集为空")
    }
  }

  # 如果提供了配置文件，先生成应该存在的访视-检查项组合骨架
  if (!is.null(config)) {
    # 读取配置
    if (is.character(config)) {
      # 如果是文件路径，读取Excel文件
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("需要安装 readxl 包以读取Excel配置文件")
      }
      config_df <- readxl::read_excel(config)
    } else if (is.data.frame(config)) {
      config_df <- config
    } else {
      stop("config 参数必须是字符串（文件路径）或数据框")
    }

    # 验证配置文件必要的列
    if (!"TESTCAT" %in% names(config_df) || !"VISITNUM" %in% names(config_df)) {
      stop("配置文件必须包含 TESTCAT 和 VISITNUM 列")
    }

    # 解析配置：将VISITNUM字符串（逗号分隔）转换为访视编号列表
    config_list <- lapply(seq_len(nrow(config_df)), function(i) {
      testcat <- config_df$TESTCAT[i]
      visitnum_str <- as.character(config_df$VISITNUM[i])
      # 解析逗号分隔的访视编号（先转数值去除空格，再转回字符）
      visitnums <- as.numeric(unlist(strsplit(visitnum_str, "[,，]")))
      visitnums <- visitnums[!is.na(visitnums)] # 移除NA值

      data.frame(
        TESTCAT = testcat,
        VISITNUM = as.character(visitnums), # 统一为字符型
        stringsAsFactors = FALSE
      )
    })
    config_expanded <- bind_rows(config_list)

    # 根据 config_cat 筛选检查分类
    if (!is.null(config_cat) && length(config_cat) > 0) {
      config_expanded <- config_expanded %>%
        filter(TESTCAT %in% config_cat)

      # 如果筛选后没有记录，警告并返回空数据框架
      if (nrow(config_expanded) == 0) {
        warning("config_cat 筛选后没有匹配的检查分类，请检查 config_cat 的值是否在配置文件中存在")
      }
    }

    # 生成应该存在的访视-检查项组合骨架
    # 基于实际发生的访视（sv_data_subset）和配置的检查项（config_expanded）
    skeleton <- sv_data_subset %>%
      inner_join(config_expanded, by = "VISITNUM") %>%
      select(SUBJID, VISIT, VISITNUM, SVDAT, TESTCAT)

    # 标准化检查项数据集的列名
    test_data_standard <- test_data %>%
      rename(
        VISIT = !!sym(sv_visit_var),
        VISITNUM = !!sym(sv_visitnum_var)
      ) %>%
      mutate(VISITNUM = as.character(VISITNUM))

    # 如果检查项数据集中有 test_cat_var 列，重命名为 TESTCAT_orig 以便后续合并
    if (!is.null(test_cat_var) && test_cat_var != "" && test_cat_var %in% names(test_data_standard)) {
      test_data_standard <- test_data_standard %>%
        rename(TESTCAT_orig = !!sym(test_cat_var))
    } else {
      test_data_standard$TESTCAT_orig <- NA_character_
    }

    # 将实际检查数据 left_join 到骨架上
    merged_data <- skeleton %>%
      left_join(
        test_data_standard,
        by = c("SUBJID", "VISIT", "VISITNUM", "TESTCAT" = "TESTCAT_orig")
      )
  } else {
    # 不使用配置时，保持原有逻辑：将检查项数据集 left_join 到访视数据集
    # 先将 test_data 的 VISITNUM 转换为字符型，以匹配 sv_data_subset
    test_data_for_join <- test_data
    test_data_for_join[[sv_visitnum_var]] <- as.character(test_data_for_join[[sv_visitnum_var]])

    merged_data <- sv_data_subset %>%
      left_join(
        test_data_for_join,
        by = c("SUBJID", "VISIT" = sv_visit_var, "VISITNUM" = sv_visitnum_var)
      )
  }

  # 创建衍生列
  # TBNAME: 数据集名称
  if (!is.null(tb_name_var) && tb_name_var != "" && tb_name_var %in% names(merged_data)) {
    merged_data$TBNAME <- merged_data[[tb_name_var]]
  } else {
    # 如果没有指定 tb_name_var 或该列不存在，使用 test_dataset 作为数据集名称
    merged_data$TBNAME <- test_dataset
  }

  # TESTCAT: 检查分类
  # 如果使用了配置，TESTCAT 列已经从骨架中创建，不需要再映射
  if (!"TESTCAT" %in% names(merged_data)) {
    if (!is.null(test_cat_var) && test_cat_var != "" && test_cat_var %in% names(merged_data)) {
      merged_data$TESTCAT <- merged_data[[test_cat_var]]
    } else {
      if (!is.null(test_cat_var) && test_cat_var != "" && !test_cat_var %in% names(merged_data)) {
        warning(paste0("检查项数据集中不存在列: ", test_cat_var, "，TESTCAT 将设为 NA"))
      }
      merged_data$TESTCAT <- NA
    }
  }

  # TESTDE: 检查项目名称
  if (!is.null(test_de_var) && test_de_var != "" && test_de_var %in% names(merged_data)) {
    merged_data$TESTDE <- merged_data[[test_de_var]]
  } else {
    if (!is.null(test_de_var) && test_de_var != "" && !test_de_var %in% names(merged_data)) {
      warning(paste0("检查项数据集中不存在列: ", test_de_var, "，TESTDE 将设为 NA"))
    }
    merged_data$TESTDE <- NA
  }

  # TESTYN: 是否进行检查
  if (test_yn_var %in% names(merged_data)) {
    merged_data$TESTYN <- merged_data[[test_yn_var]]
  } else {
    warning(paste0("检查项数据集中不存在列: ", test_yn_var, "，TESTYN 将设为 NA"))
    merged_data$TESTYN <- NA
  }

  # TESTDAT: 检查日期
  if (test_date_var %in% names(merged_data)) {
    merged_data$TESTDAT <- merged_data[[test_date_var]]
  } else {
    warning(paste0("检查项数据集中不存在列: ", test_date_var, "，TESTDAT 将设为 NA"))
    merged_data$TESTDAT <- NA
  }

  # ORRES: 检查结果
  if (test_result_var %in% names(merged_data)) {
    # 如果原始数据已经有 ORRES 列，且与 test_result_var 不同，则使用 test_result_var
    if (test_result_var != "ORRES") {
      merged_data$ORRES <- merged_data[[test_result_var]]
    }
    # 如果 test_result_var 就是 ORRES，则不需要额外处理
  } else {
    warning(paste0("检查项数据集中不存在列: ", test_result_var, "，ORRES 将设为 NA"))
    merged_data$ORRES <- NA
  }

  # 填补 TBNAME 的 NA 值
  # 由于是 left_join，如果某个访视没有检查记录，TBNAME 会是 NA

  # 确定填补值：使用该数据集中第一个非 NA 的 TBNAME 值，如果全为 NA 则使用 test_dataset
  if (any(!is.na(merged_data$TBNAME))) {
    fill_value <- merged_data$TBNAME[!is.na(merged_data$TBNAME)][1]
  } else {
    fill_value <- test_dataset
  }

  # 填补 TBNAME
  merged_data$TBNAME <- ifelse(is.na(merged_data$TBNAME), fill_value, merged_data$TBNAME)

  # 重新排列列顺序：SUBJID、VISIT、VISITNUM、TBNAME、TESTCAT、TESTDE、TESTYN、TESTDAT、ORRES，其他原始列
  key_cols <- c("SUBJID", "VISIT", "VISITNUM")
  derived_cols <- c("TBNAME", "TESTCAT", "TESTDE", "TESTYN", "TESTDAT", "ORRES")

  # 获取其他原始列（排除关键列和衍生列）
  other_cols <- setdiff(names(merged_data), c(key_cols, derived_cols))

  # 返回重新排列后的数据集
  result <- merged_data %>%
    select(all_of(key_cols), all_of(derived_cols), all_of(other_cols))

  return(result)
}
