#' Apply format mapping to data
#'
#' @description
#' Map values in data frames based on format information, similar to SAS formats.
#' This function applies value mappings defined in a format data frame to corresponding
#' columns in the input data.
#'
#' @param data Data frame or list of data frames containing study data
#' @param format_df Data frame containing format mapping information with columns:
#'   FMTNAME, START, and LABEL
#'
#' @return Data frame or list of data frames with values mapped according to format_df
#'
#' @family format utilities
#' @export
apply_format_mapping <- function(data, format_df) {
  # Convert format_df START column to character to ensure matching
  format_df$START <- as.character(format_df$START)

  # Function to map values for a single variable
  map_variable <- function(x, fmt_name) {
    # Get mapping for this format name
    mapping <- format_df[format_df$FMTNAME == fmt_name, ]
    if (nrow(mapping) == 0) {
      return(x)
    }

    # Convert x to character for matching
    x_char <- as.character(x)

    # Create mapping vector
    mapped_values <- mapping$LABEL
    names(mapped_values) <- mapping$START

    # Replace values
    result <- mapped_values[x_char]

    # Return original value if no match found
    result[is.na(result)] <- x_char[is.na(result)]
    result
  }

  # Get all format names from the format file
  format_names <- unique(format_df$FMTNAME)

  # Process each column in the dataset
  for (col in names(data)) {
    # Check if column name exists in format names
    if (toupper(col) %in% format_names) {
      data[[col]] <- map_variable(data[[col]], toupper(col))
    }
  }
  return(data)
}


#' Check if elements could be SAS missing data variants
#'
#' Missing data from sas7bdat imported datasets could be imported in different
#' ways. This function checks for 'NA', NA, '.', and '' (empty string).
#'
#' @param x Vector with data to check
#'
#' @return Logical vector indicating which elements are SAS missing values
#'
#' @export
is_sas_na <- function(x) {
  x <- trimws(x)
  is.na(x) |
    vapply(x, function(xi) {
      identical(xi, "") | identical(xi, "NA") | identical(xi, ".")
    }, logical(1))
}

#' Convert check results to a standardized data frame
#'
#' @description
#' Converts check result objects (lists) into standardized data frames for easier
#' manipulation and reporting. This function is particularly useful for combining
#' results from multiple checks.
#'
#' @details
#' The function expects check_result to be a list with at least the following components:
#' \describe{
#'   \item{has_deviation}{Logical. Whether any deviation was found}
#'   \item{messages}{Character vector. Summary messages}
#'   \item{details}{Data frame (optional). Detailed deviation information}
#' }
#'
#' If details are present and non-empty, the function returns the detailed data frame
#' with added check_name and has_deviation columns. Otherwise, it returns a summary
#' data frame with one row.
#'
#' @param check_result List containing check results from any check function
#' @param check_name Character string. Name of the check (optional, will be extracted
#'   from the object's class attribute if not provided)
#'
#' @return Data frame with standardized check results containing columns:
#' \describe{
#'   \item{check_name}{Character. Name of the check}
#'   \item{has_deviation}{Logical. Whether deviation was found}
#'   \item{message}{Character. Summary message (collapsed from messages vector)}
#'   \item{...}{Additional columns from details if present}
#' }
#'
#' @family check result utilities
#' @importFrom dplyr bind_rows
#' @export
as_check_df <- function(check_result, check_name = NULL) {
  # Extract check name from class if not provided
  if (is.null(check_name)) {
    classes <- class(check_result)
    # First class is usually the specific check type
    if (length(classes) > 0 && classes[1] != "list") {
      check_name <- classes[1]
    } else {
      check_name <- "unnamed_check"
    }
  }

  # Initialize empty result data frame
  result_df <- data.frame(
    check_name = character(),
    has_deviation = logical(),
    message = character(),
    stringsAsFactors = FALSE
  )

  # Add main check information
  result_df <- rbind(result_df, data.frame(
    check_name = check_name,
    has_deviation = check_result$has_deviation,
    message = ifelse(length(check_result$messages) > 0,
      paste(check_result$messages, collapse = "; "),
      NA_character_
    ),
    stringsAsFactors = FALSE
  ))

  # Add details if available
  if (!is.null(check_result$details) && nrow(check_result$details) > 0) {
    # Convert details to data frame if it's not already
    if (!inherits(check_result$details, "data.frame")) {
      details_df <- as.data.frame(check_result$details, stringsAsFactors = FALSE)
    } else {
      details_df <- check_result$details
    }

    # Add check name to details
    details_df$check_name <- check_name
    details_df$has_deviation <- check_result$has_deviation

    # Return the detailed results
    return(details_df)
  }

  # Return the basic results if no details
  return(result_df)
}

#' Combine multiple check results into a single data frame
#'
#' @description
#' Combines multiple check result objects into a single data frame by converting
#' each result using \code{\link{as_check_df}} and then row-binding them together.
#' NULL results are automatically skipped.
#'
#' @details
#' This is a convenience function for aggregating results from multiple data quality
#' checks. Each check result is converted to a standardized data frame format before
#' combining, ensuring consistent structure in the output.
#'
#' @param ... Multiple check result objects (lists) to combine. NULL values are ignored
#'
#' @return Data frame with combined check results. Each row represents either a check
#'   summary or a detailed deviation record, depending on the input check results
#'
#' @seealso \code{\link{as_check_df}} for converting individual check results
#'
#' @family check result utilities
#' @importFrom dplyr bind_rows
#' @export
combine_check_results <- function(...) {
  # Get all check results
  check_results <- list(...)

  # Convert each to data frame and combine
  result_dfs <- lapply(check_results, function(result) {
    # Skip NULL results
    if (is.null(result)) {
      return(NULL)
    }

    # Convert to data frame
    as_check_df(result)
  })

  # Combine all data frames
  combined_df <- dplyr::bind_rows(result_dfs)

  return(combined_df)
}

#' Parse console output from check functions into a data frame
#'
#' @description
#' Parses the console output from check functions and converts it into a standardized
#' data frame format. Can either parse existing text or capture output directly from
#' a function call.
#'
#' @details
#' The function expects output in a specific format with sections:
#' \itemize{
#'   \item Header: Check name, typically numbered (e.g., "8.4 Visit Window Check")
#'   \item Deviation status: "Has deviation: YES" or "Has deviation: NO"
#'   \item Findings: Summary messages following "Findings:"
#'   \item Deviation Details: Detailed information following "Deviation Details:"
#' }
#'
#' When parsing deviation details, the function attempts to extract subject IDs (SUBJID)
#' from lines containing the Chinese character "Subject" followed by numeric IDs. If multiple
#' subjects are found, each gets a separate row in the output.
#'
#' @param text Character string or vector containing check function console output,
#'   or a connection object to read from. Used when capture_output is FALSE
#' @param capture_output Logical. If TRUE, the function will capture output from
#'   check_fn instead of using the text parameter
#' @param check_fn Function to call (only used if capture_output is TRUE)
#' @param ... Additional arguments to pass to check_fn (only used if capture_output is TRUE)
#'
#' @return Data frame with standardized check results containing columns:
#' \describe{
#'   \item{SUBJID}{Character. Subject ID (extracted from details if available, otherwise NA)}
#'   \item{check_name}{Character. Name of the check}
#'   \item{has_deviation}{Logical. Whether deviation was found}
#'   \item{message}{Character. Summary findings message}
#'   \item{details}{Character. Detailed deviation information}
#' }
#'
#' @family check result utilities
#' @importFrom utils capture.output
#' @export
parse_check_output <- function(text = NULL, capture_output = FALSE, check_fn = NULL, ...) {
  if (capture_output) {
    if (is.null(check_fn)) {
      stop("check_fn must be provided when capture_output is TRUE")
    }
    text <- utils::capture.output(check_result <- check_fn(...))
  }

  if (is.null(text)) {
    stop("Either text or (capture_output and check_fn) must be provided")
  }

  # Join text lines into a single string
  if (is.character(text) && length(text) > 1) {
    text <- paste(text, collapse = "\n")
  }

  # Read from connection if provided
  if (inherits(text, "connection")) {
    text <- paste(readLines(text), collapse = "\n")
  }

  # Initialize result data
  check_name <- "unnamed_check"
  has_deviation <- FALSE
  message <- NA_character_
  details <- NA_character_

  # Extract check name and section title
  # Try first pattern for most checks
  header_pattern <- "^([0-9\\.]+\\s+[^\\n]+)\\n==+"
  header_match <- regexpr(header_pattern, text, perl = TRUE)

  if (header_match > 0) {
    header_text <- regmatches(text, header_match)
    check_name <- gsub("\\n==+", "", header_text)
  } else {
    # Try alternate pattern for cases without the full header format
    alt_header_pattern <- "^([0-9\\.]+[^\\n]+)\\n"
    alt_header_match <- regexpr(alt_header_pattern, text, perl = TRUE)
    if (alt_header_match > 0) {
      header_text <- regmatches(text, alt_header_match)
      check_name <- gsub("\\n$", "", header_text)
    }
  }

  if (is.null(check_name) || length(check_name) == 0 || check_name == "") {
    check_name <- "unnamed_check"
  }

  # Extract deviation status
  has_dev_pattern <- "Has deviation:\\s+(YES|NO)"
  has_dev_match <- regexpr(has_dev_pattern, text, perl = TRUE)

  if (has_dev_match > 0) {
    has_dev_text <- regmatches(text, has_dev_match)
    has_deviation <- grepl("YES", has_dev_text)
  }

  # Extract findings/messages - now handling multi-line findings
  # Look for the text between "Findings:" and the next section (Deviation Details:)
  findings_section_start <- regexpr("Findings:", text, fixed = TRUE)

  if (findings_section_start > 0) {
    # Get everything after "Findings:"
    findings_start <- findings_section_start + nchar("Findings:")

    # Look for the start of the next section (Deviation Details:)
    next_section <- regexpr("Deviation Details:", text, fixed = TRUE)

    if (next_section > 0) {
      # Extract text between Findings: and Deviation Details:
      findings_text <- substr(text, findings_start, next_section - 1)
    } else {
      # If there's no Deviation Details section, just get the rest of the text
      findings_text <- substr(text, findings_start, nchar(text))
    }

    # Clean up the findings text (remove leading/trailing whitespace, cleanup bullet points)
    findings_text <- trimws(findings_text)
    findings_text <- gsub("^\\s*-\\s*", "", findings_text) # Remove leading bullet points
    message <- findings_text
  }

  # Extract deviation details as a whole
  # Improved pattern to capture multi-line details until the end of text or a clear section separator
  details_section_start <- regexpr("Deviation Details:", text, perl = TRUE)

  if (details_section_start > 0) {
    # Find the start of the details section
    details_start <- details_section_start + attr(details_section_start, "match.length")

    # Extract everything after "Deviation Details:" to the end of the text
    details_text <- substr(text, details_start, nchar(text))

    # Clean up leading whitespace and newlines
    details_text <- gsub("^\\s*\\n", "", details_text)

    # Find the end of the details section (if there's a clear section separator or end of text)
    next_section_match <- regexpr("\\n\\n+[\\d\\.]+\\s+[^\\n]+\\n==+", details_text, perl = TRUE)
    if (next_section_match > 0) {
      details_text <- substr(details_text, 1, next_section_match - 1)
    }

    # Trim trailing whitespace
    details <- trimws(details_text)

    # Create result data frame with a row for each record
    result_rows <- list()

    # Check if details contains the pattern "受试者"
    if (grepl("受试者", details)) {
      # Split details into lines
      detail_lines <- unlist(strsplit(details, "\n"))

      # Process each line that contains "受试者"
      subject_lines <- detail_lines[grepl("受试者", detail_lines)]

      if (length(subject_lines) > 0) {
        for (i in seq_along(subject_lines)) {
          line <- subject_lines[i]

          # Find position of "受试者" in the line
          subj_pos <- regexpr("受试者", line, fixed = TRUE)[1]
          if (subj_pos > 0) {
            # Extract subject ID using regex to find continuous digits after "受试者"
            # This pattern looks for "受试者" followed by optional non-digit chars, then captures all digits
            subj_pattern <- "受试者[^0-9]*([0-9]+)"
            subj_match <- regexpr(subj_pattern, line, perl = TRUE)

            if (subj_match > 0) {
              # Extract the matched text
              matched_text <- regmatches(line, subj_match)
              # Extract only the digits
              subjid <- gsub("[^0-9]", "", matched_text)
            } else {
              # Fallback: try to extract any number after "受试者"
              remaining_text <- substr(line, subj_pos + nchar("受试者"), nchar(line))
              subjid <- gsub(".*?([0-9]+).*", "\\1", remaining_text)
            }

            # Create a row for this subject
            result_rows[[i]] <- data.frame(
              SUBJID = subjid,
              check_name = check_name,
              has_deviation = has_deviation,
              message = message,
              details = line, # Just include this specific line
              stringsAsFactors = FALSE
            )
          }
        }

        # Combine all rows
        if (length(result_rows) > 0) {
          return(do.call(rbind, result_rows))
        }
      }
    }
  }

  # If no subject-specific rows were created, return a single row
  result_df <- data.frame(
    SUBJID = NA_character_,
    check_name = check_name,
    has_deviation = has_deviation,
    message = message,
    details = details,
    stringsAsFactors = FALSE
  )

  return(result_df)
}

#' Capture and parse output from multiple check functions
#'
#' @description
#' Runs multiple check functions, captures their console output, parses it, and
#' combines the results into a single data frame. This is useful for batch processing
#' multiple data quality checks.
#'
#' @details
#' Each function in \code{...} should accept a single \code{data} parameter and
#' should print formatted output to the console. The console output is captured
#' and parsed using \code{\link{parse_check_output}}.
#'
#' If a function produces no output, the function attempts to use the return value
#' directly via \code{\link{as_check_df}}. If the check name cannot be extracted
#' from the output, the parameter name is used as the check name.
#'
#' @param ... Named functions to run. Each should be a function that accepts a
#'   data parameter. The names will be used as check identifiers if check names
#'   cannot be extracted from the output
#' @param data Data object to pass to each check function. Can be NULL if the
#'   functions don't use it (optional, defaults to NULL)
#'
#' @return Data frame with combined check results from all functions, with columns
#'   depending on the parsed output structure
#'
#' @seealso \code{\link{parse_check_output}}, \code{\link{as_check_df}},
#'   \code{\link{combine_check_results}}
#'
#' @family check result utilities
#' @export
capture_check_results <- function(..., data = NULL) {
  check_fns <- list(...)

  # If no names provided, create default names
  if (is.null(names(check_fns))) {
    names(check_fns) <- paste0("check_", seq_along(check_fns))
  }

  # Initialize result list
  results <- list()

  # Run each check function and capture its output
  for (i in seq_along(check_fns)) {
    fn_name <- names(check_fns)[i]
    fn <- check_fns[[i]]

    # Create a text connection to capture output
    output <- utils::capture.output(result <- fn(data))

    if (length(output) == 0 || all(output == "")) {
      if (exists("result") && !is.null(result)) {
        parsed_result <- as_check_df(result, check_name = fn_name)
      } else {
        parsed_result <- data.frame(
          check_name = fn_name,
          has_deviation = FALSE,
          message = "No output captured",
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Parse the output
      parsed_result <- parse_check_output(text = output)

      # If check_name was not found in the output, use function name
      check_name <- parsed_result$check_name
      is_empty <- is.null(check_name) || length(check_name) == 0 ||
        all(is.na(check_name)) || all(check_name == "")
      if (is_empty) {
        parsed_result$check_name <- fn_name
      }
    }

    # Add to results
    results[[fn_name]] <- parsed_result
  }

  # Combine all results
  combined_df <- dplyr::bind_rows(results)

  return(combined_df)
}

#' Match visit type based on visit type string
#'
#' @description
#' Internal helper function to classify visit types based on Chinese text patterns.
#' This function is used by both \code{generate_planned_visit_dates} and
#' \code{check_missing_visit} to ensure consistent visit type classification.
#'
#' @param visit_type Character string, visit type description in Chinese
#'   (e.g., "筛选" for screening, "治疗周期1" for treatment cycle 1)
#'
#' @return Character string, one of: "screening", "treatment", "end_of_treatment",
#'   "follow_up", or "unknown"
#'
#' @keywords internal
#' @noRd
match_visit_type <- function(visit_type) {
  if (is.na(visit_type)) {
    return("unknown")
  }
  visit_type <- tolower(visit_type)

  if (grepl("筛选", visit_type)) {
    return("screening")
  } else if (grepl("治疗", visit_type) && !grepl("治疗结束", visit_type)) {
    return("treatment")
  } else if (grepl("治疗结束|退出", visit_type)) {
    return("end_of_treatment")
  } else if (grepl("随访", visit_type)) {
    return("follow_up")
  } else {
    return("unknown")
  }
}
