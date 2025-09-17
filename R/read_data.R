#' Read raw data
#'
#' Scan specified directory and read all SAS format files
#' @param folder Path to data directory
#' @return A named list of data.frames where names are filenames
#' @importFrom haven read_sas
#' @importFrom dplyr rename_with
#' @export
read_raw_data <- function(folder, iwrs_file = "iwrs.csv", format_file = NULL) {
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
  
  # Find format file
  fmt_file <- sas_files[grepl("formats", sas_files)]
  # Read SAS format files
  FMT_DF <- haven::read_sas(fmt_file)

  if (!is.null(format_file) && file.exists(format_file)) {
    FMT_TBL <- readxl::read_excel(format_file) %>% 
      filter(`匹配状态` == "完全匹配") %>%
      select(TABLE = "表",
             FMTNAME = "变量",
             START = "编码值",
             LABEL = "编码说明") %>% unique()
    
    # Filter out formats that are in the additional format file
    FMT_DF <- FMT_DF %>%
      filter(!FMTNAME %in% FMT_TBL$FMTNAME) %>% unique()
  } else {
    FMT_TBL <- NULL
  }

  # Handle no files found
  if (length(sas_files) == 0) {
    warning("No SAS files found in the specified directory")
    return(list())
  }

  # Read IWRS data from Excel file
  IWRS <- readr::read_csv(iwrs_file,
    skip = 2, # Skip first 2 rows to start reading from row 3
    show_col_types = FALSE
  )

  # Read files with error handling
  data_list <- lapply(sas_files, function(file) {
    tryCatch(
      {
        # Read SAS file and convert to data.frame
        df <- as.data.frame(haven::read_sas(file))
        if (!is.null(FMT_TBL)) {
          FMT_TBL <- FMT_TBL %>% filter(TABLE == toupper(tools::file_path_sans_ext(basename(file))))
          df <- apply_format_mapping(df, FMT_TBL)
        }

        df <- apply_format_mapping(df, FMT_DF)

        # Standardize column names to uppercase
        df %>% rename_with(toupper)
      },
      error = function(e) {
        warning("Failed to read file: ", basename(file), "\nError: ", e$message)
        NULL
      }
    )
  })

  # Combine IWRS data with other datasets
  data_list <- c(list(IWRS = IWRS), data_list)

  # Name the list with dataset names
  names(data_list) <- toupper(c("IWRS", gsub(".sas7bdat", "", basename(sas_files))))

  # Report any failed reads
  failed_files <- names(which(sapply(data_list, is.null)))
  if (length(failed_files) > 0) {
    warning(
      "Failed to read the following files:\n",
      paste("- ", failed_files, collapse = "\n")
    )
  }

  # Print empty datasets
  empty_data <- names(which(sapply(data_list, function(df) nrow(df) == 0)))
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
#'
#' @param data_dir Directory containing SAS data files (.sas7bdat)
#' @param catalog_file Path to SAS format catalog file (.sas7bcat)
#' @param catalog_encoding Encoding to use for the catalog file, defaults to "UTF-8"
#' @return List of data frames containing study data with formats applied
#' @importFrom haven read_sas as_factor is.labelled
#' @importFrom dplyr rename_with
#' @export
read_raw_data_with_formats <- function(data_dir, catalog_file, iwrs_file = "iwrs.csv", encoding = "UTF-8") {
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
  sas_files <- list.files(data_dir, pattern = "\\.sas7bdat$", full.names = TRUE)
  
  if (length(sas_files) == 0) {
    stop("No SAS data files found in directory: ", data_dir)
  }
  
  # Read IWRS data from Excel file
  IWRS <- readr::read_csv(iwrs_file,
    skip = 2, # Skip first 2 rows to start reading from row 3
    show_col_types = FALSE
  )

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
            # If character vectors are preferred over factors, one could use as.character(haven::as_factor(df[[col_name]]))
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
  
  # Combine IWRS data with other datasets
  data_list <- c(list(IWRS = IWRS), data_list)
  
  # Name the list with dataset names
  names(data_list) <- toupper(c("IWRS", gsub("\\.sas7bdat$", "", basename(sas_files))))
  
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



