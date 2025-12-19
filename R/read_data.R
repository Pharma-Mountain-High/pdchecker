#' Read raw data
#'
#' Scan specified directory and read all SAS format files
#' @param folder Path to data directory containing SAS files
#' @param iwrs_file Path to iwrs CSV file. Defaults to NULL (no iwrs file).
#'   If provided, the CSV file should have 2 header rows that will be skipped
#'   during import. If NULL or if the file does not exist, iwrs data will be
#'   skipped with a message/warning.
#' @param format_file Optional path to Excel format mapping file (.xlsx).
#'   If provided, will be used to map coded values to labels. The Excel file
#'   should contain columns: "表" (table name), "变量" (variable name),
#'   "编码值" (coded value), "编码说明" (label), and "匹配状态"
#'   (match status, should be "完全匹配").
#' @return A named list of data.frames with the following structure:
#'   \describe{
#'     \item{iwrs}{iwrs data frame with randomization information. Present as
#'       the first element only if iwrs_file is provided and successfully read.}
#'     \item{<DATASET_NAME>}{Additional datasets read from SAS files
#'       (e.g., DM, AE, LB, VS, etc.). Dataset names are converted to uppercase
#'       and derived from the SAS filename without the .sas7bdat extension.}
#'   }
#'   All column names within each data frame are converted to uppercase.
#'   Format mapping is applied if format files are available.
#'   Datasets that failed to read are excluded from the returned list (with a warning message).
#'   Empty datasets (0 rows) are included but reported via warning.
#' @importFrom haven read_sas
#' @importFrom dplyr rename_with filter select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
read_raw_data <- function(folder, iwrs_file = NULL, format_file = NULL) {
  # Validate directory existence
  if (!dir.exists(folder)) {
    stop("Specified directory does not exist: ", folder)
  }

  # Find all SAS files
  sas_files <- list.files(folder,
    pattern = "\\.sas7bdat$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Handle no files found
  if (length(sas_files) == 0) {
    warning("No SAS files found in the specified directory")
  }

  # Initialize format variables
  fmt_df <- NULL
  fmt_tbl <- NULL

  # Process format files only if SAS files exist
  if (length(sas_files) > 0) {
    # Find format file
    fmt_file <- sas_files[grepl("formats", sas_files, ignore.case = TRUE)]

    # Validate format file
    if (length(fmt_file) == 0) {
      warning("No SAS format file (containing 'formats' in filename) found in directory: ", folder)
    } else {
      if (length(fmt_file) > 1) {
        warning("Multiple format files found, using the first one: ", basename(fmt_file[1]))
        fmt_file <- fmt_file[1]
      }

      # Read SAS format files with error handling
      fmt_df <- tryCatch(
        {
          haven::read_sas(fmt_file)
        },
        error = function(e) {
          warning("Failed to read format file: ", basename(fmt_file), "\nError: ", e$message)
          NULL
        }
      )
    }

    if (!is.null(format_file) && file.exists(format_file)) {
      fmt_tbl <- readxl::read_excel(format_file) %>%
        filter(.data[["匹配状态"]] == "完全匹配") %>%
        select(
          TABLE = .data[["表"]],
          FMTNAME = .data[["变量"]],
          START = .data[["编码值"]],
          LABEL = .data[["编码说明"]]
        ) %>%
        unique()

      # Filter out formats that are in the additional format file
      if (!is.null(fmt_df)) {
        fmt_df <- fmt_df %>%
          filter(!.data[["FMTNAME"]] %in% fmt_tbl$FMTNAME) %>%
          unique()
      }
    }
  }


  # Read iwrs data from CSV file
  iwrs <- NULL
  if (is.null(iwrs_file)) {
    message("No iwrs file specified. Skipping iwrs data.")
  } else if (!file.exists(iwrs_file)) {
    warning("iwrs file does not exist: ", iwrs_file, ". Skipping iwrs data.")
  } else {
    iwrs <- tryCatch(
      {
        df <- readr::read_csv(iwrs_file,
          skip = 2, # Skip first 2 rows to start reading from row 3
          show_col_types = FALSE
        )
        if (nrow(df) == 0) {
          warning("iwrs file is empty: ", iwrs_file)
        }
        df
      },
      error = function(e) {
        warning("Failed to read iwrs file: ", iwrs_file, "\nError: ", e$message)
        NULL
      }
    )
  }

  # Read files with error handling
  data_list <- lapply(sas_files, function(file) {
    message("Reading file: ", basename(file))

    tryCatch(
      {
        # Read SAS file and convert to data.frame
        df <- as.data.frame(haven::read_sas(file))
        if (!is.null(fmt_tbl)) {
          # Use local variable to avoid modifying fmt_tbl in the outer scope
          fmt_tbl_filtered <- fmt_tbl %>% filter(.data[["TABLE"]] == toupper(tools::file_path_sans_ext(basename(file))))
          df <- apply_format_mapping(df, fmt_tbl_filtered)
        }

        df <- apply_format_mapping(df, fmt_df)

        # Standardize column names to uppercase
        df %>% rename_with(toupper)
      },
      error = function(e) {
        warning("Failed to read file: ", basename(file), "\nError: ", e$message)
        NULL
      }
    )
  })

  # Combine iwrs data with other datasets
  if (!is.null(iwrs)) {
    data_list <- c(list(iwrs = iwrs), data_list)
    # Name the list with dataset names
    names(data_list) <- toupper(c("iwrs", gsub("\\.sas7bdat$", "", basename(sas_files))))
  } else {
    # Name the list with dataset names (without iwrs)
    names(data_list) <- toupper(gsub("\\.sas7bdat$", "", basename(sas_files)))
  }

  # Report any failed reads
  failed_files <- names(which(sapply(data_list, is.null)))
  if (length(failed_files) > 0) {
    warning(
      "Failed to read the following files:\n",
      paste("- ", failed_files, collapse = "\n")
    )
  }

  # Print empty datasets
  empty_data <- names(which(sapply(data_list[!names(data_list) %in% failed_files], function(df) nrow(df) == 0)))
  if (length(empty_data) > 0) {
    warning(
      "The following datasets are empty:\n",
      paste("- ", empty_data, collapse = "\n")
    )
  }

  # Remove failed reads and return
  Filter(Negate(is.null), data_list)
}


#' Read raw data with SAS format catalog
#'
#' This function reads raw SAS datasets using a format catalog file to apply formats.
#' Formats are applied by converting labelled columns to factors using the catalog labels.
#'
#' @param data_dir Directory containing SAS data files (.sas7bdat)
#' @param catalog_file Path to SAS format catalog file (.sas7bcat). This file contains
#'   the format definitions that will be applied to the data.
#' @param iwrs_file Path to iwrs CSV file. Defaults to NULL (no iwrs file).
#'   If provided, the CSV file should have 2 header rows that will be skipped during import.
#'   If NULL or if the file does not exist, iwrs data will be skipped with a message/warning.
#' @param encoding Character encoding to use when reading SAS files and catalog file.
#'   Defaults to "UTF-8". Common alternatives include "latin1", "GBK", etc.
#' @return A named list of data.frames with the following structure:
#'   \describe{
#'     \item{iwrs}{iwrs data frame with randomization information. Present as
#'       the first element only if iwrs_file is provided and successfully read.}
#'     \item{<DATASET_NAME>}{Additional datasets read from SAS files
#'       (e.g., DM, AE, LB, VS, etc.). Dataset names are converted to uppercase
#'       and derived from the SAS filename without the .sas7bdat extension.}
#'   }
#'   All column names within each data frame are converted to uppercase.
#'   Columns with SAS format labels (labelled data) are automatically converted to factors,
#'   with factor levels corresponding to the format labels from the catalog file.
#'   Datasets that failed to read are excluded from the returned list (with a warning message).
#'   Empty datasets (0 rows) are included but reported via warning.
#' @importFrom haven read_sas as_factor is.labelled
#' @importFrom dplyr rename_with
#' @importFrom magrittr %>%
#' @export
read_raw_data_with_formats <- function(data_dir, catalog_file, iwrs_file = NULL, encoding = "UTF-8") {
  # Validate inputs
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist: ", data_dir)
  }

  if (!file.exists(catalog_file)) {
    stop("Format catalog file does not exist: ", catalog_file)
  }

  if (!grepl("\\.sas7bcat$", catalog_file)) {
    stop("Format catalog file must be a .sas7bcat file")
  }

  # Find all SAS data files
  sas_files <- list.files(data_dir,
    pattern = "\\.sas7bdat$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(sas_files) == 0) {
    warning("No SAS data files found in directory: ", data_dir)
  }

  # Read iwrs data from CSV file
  iwrs <- NULL
  if (is.null(iwrs_file)) {
    message("No iwrs file specified. Skipping iwrs data.")
  } else if (!file.exists(iwrs_file)) {
    warning("iwrs file does not exist: ", iwrs_file, ". Skipping iwrs data.")
  } else {
    iwrs <- tryCatch(
      {
        df <- readr::read_csv(iwrs_file,
          skip = 2, # Skip first 2 rows to start reading from row 3
          show_col_types = FALSE
        )
        if (nrow(df) == 0) {
          warning("iwrs file is empty: ", iwrs_file)
        }
        df
      },
      error = function(e) {
        warning("Failed to read iwrs file: ", iwrs_file, "\nError: ", e$message)
        NULL
      }
    )
  }

  # Read each SAS file with catalog
  data_list <- lapply(sas_files, function(file) {
    message("Reading file: ", basename(file))

    tryCatch(
      {
        # Read SAS file with catalog file
        df <- as.data.frame(haven::read_sas(
          data_file = file,
          catalog_file = catalog_file,
          encoding = encoding,
          catalog_encoding = encoding
        ))

        # Apply formats from catalog using labels
        # Convert labelled columns to factors (which applies labels)
        for (col_name in names(df)) {
          if (haven::is.labelled(df[[col_name]])) {
            # Using as_factor to apply labels.
            # To get character vectors instead of factors
            df[[col_name]] <- haven::as_factor(df[[col_name]])
          }
        }

        # Standardize column names to uppercase
        df %>% dplyr::rename_with(toupper)
      },
      error = function(e) {
        warning("Failed to read file: ", basename(file), "\nError: ", e$message)
        NULL
      }
    )
  })

  # Combine iwrs data with other datasets
  if (!is.null(iwrs)) {
    data_list <- c(list(iwrs = iwrs), data_list)
    # Name the list with dataset names
    names(data_list) <- toupper(c("iwrs", gsub("\\.sas7bdat$", "", basename(sas_files))))
  } else {
    # Name the list with dataset names (without iwrs)
    names(data_list) <- toupper(gsub("\\.sas7bdat$", "", basename(sas_files)))
  }

  # Report any failed reads
  failed_files <- names(which(sapply(data_list, is.null)))
  if (length(failed_files) > 0) {
    warning(
      "Failed to read the following files:\n",
      paste("- ", failed_files, collapse = "\n")
    )
  }

  # Print empty datasets
  empty_data <- names(which(sapply(data_list[!names(data_list) %in% failed_files], function(df) nrow(df) == 0)))
  if (length(empty_data) > 0) {
    warning(
      "The following datasets are empty:\n",
      paste("- ", empty_data, collapse = "\n")
    )
  }

  # Remove failed reads and return
  Filter(Negate(is.null), data_list)
}
