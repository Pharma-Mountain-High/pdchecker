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
#'   should contain columns: (table name), (variable name),
#'   (coded value),  (label), and (match status).
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
#' @examples
#' \dontrun{
#' # Read SAS data files from a directory
#' data <- read_raw_data(
#'   folder = "path/to/sas/files",
#'   iwrs_file = "path/to/iwrs.csv"
#' )
#'
#' # Read with additional Excel format mapping file
#' data <- read_raw_data(
#'   folder = "path/to/sas/files",
#'   iwrs_file = "path/to/iwrs.csv",
#'   format_file = "path/to/formats.xlsx"
#' )
#'
#' # Access individual datasets
#' dm <- data$DM
#' ae <- data$AE
#' }
#'
#' @note
#' - Column names in all returned data frames are converted to uppercase.
#' - The FORMATS file (if found) is excluded from the returned data list.
#' - IWRS CSV file is expected to have 2 header rows that will be skipped.
#' - Format mapping priority: Excel format file > SAS format file.
#'
#' @seealso [read_raw_data_with_formats()] for reading data with SAS catalog file
#' @family data reading functions
#'
#' @importFrom haven read_sas
#' @importFrom dplyr rename_with filter select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @export
read_raw_data <- function(folder, iwrs_file = NULL, format_file = NULL) {
  # Validate parameter types
  if (!is.character(folder) || length(folder) != 1) {
    stop("'folder' must be a single character string")
  }
  if (!is.null(iwrs_file) && (!is.character(iwrs_file) || length(iwrs_file) != 1)) {
    stop("'iwrs_file' must be NULL or a single character string")
  }
  if (!is.null(format_file) && (!is.character(format_file) || length(format_file) != 1)) {
    stop("'format_file' must be NULL or a single character string")
  }

  # Validate directory existence
  if (!dir.exists(folder)) {
    stop("Specified directory does not exist: ", folder)
  }

  # Validate format_file extension if provided
  if (!is.null(format_file) && file.exists(format_file)) {
    if (!grepl("\\.xlsx$", format_file, ignore.case = TRUE)) {
      warning("format_file should be an Excel file (.xlsx), got: ", basename(format_file))
    }
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
#' @examples
#' \dontrun{
#' # Read SAS data files with format catalog
#' data <- read_raw_data_with_formats(
#'   data_dir = "path/to/sas/files",
#'   catalog_file = "path/to/formats.sas7bcat",
#'   iwrs_file = "path/to/iwrs.csv"
#' )
#'
#' # Read with custom encoding (e.g., for Chinese characters)
#' data <- read_raw_data_with_formats(
#'   data_dir = "path/to/sas/files",
#'   catalog_file = "path/to/formats.sas7bcat",
#'   iwrs_file = "path/to/iwrs.csv",
#'   encoding = "GBK"
#' )
#'
#' # Access individual datasets
#' dm <- data$DM
#' ae <- data$AE
#' }
#'
#' @note
#' - Column names in all returned data frames are converted to uppercase.
#' - Labelled columns from SAS are automatically converted to factors.
#' - IWRS CSV file is expected to have 2 header rows that will be skipped.
#' - The same encoding is applied to both data files and catalog file.
#'
#' @seealso [read_raw_data()] for reading data with SAS format data file
#' @family data reading functions
#'
#' @importFrom haven read_sas as_factor is.labelled
#' @importFrom dplyr rename_with
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @export
read_raw_data_with_formats <- function(data_dir, catalog_file, iwrs_file = NULL, encoding = "UTF-8") {
  # Validate parameter types
  if (!is.character(data_dir) || length(data_dir) != 1) {
    stop("'data_dir' must be a single character string")
  }
  if (!is.character(catalog_file) || length(catalog_file) != 1) {
    stop("'catalog_file' must be a single character string")
  }
  if (!is.null(iwrs_file) && (!is.character(iwrs_file) || length(iwrs_file) != 1)) {
    stop("'iwrs_file' must be NULL or a single character string")
  }
  if (!is.character(encoding) || length(encoding) != 1) {
    stop("'encoding' must be a single character string")
  }

  # Validate inputs
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist: ", data_dir)
  }

  if (!file.exists(catalog_file)) {
    stop("Format catalog file does not exist: ", catalog_file)
  }

  if (!grepl("\\.sas7bcat$", catalog_file, ignore.case = TRUE)) {
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
