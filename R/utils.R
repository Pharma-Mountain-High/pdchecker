#' Apply format mapping to data
#'
#' Map values in data frames based on format information
#' @param data List of data frames containing study data
#' @param format_df Data frame containing format information
#' @return List of data frames with mapped values
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


#' Check if the elements could be of any SAS missing data variant
#'
#' Missing Data from sas7bdat imported datesets could be imported in different
#' ways we currently check for 'NA', NA, '.', and ''.
#'
#' @param x vector with data
#'
#' @return logical vector
#'
#' @export
#' @keywords internal
#' @examples
#' is_sas_na(c(1, 2, NA))
#'
#' is_sas_na(c("a", NA, "NA", ""))
is_sas_na <- function(x) {
  x <- trimws(x)
  is.na(x) |
    vapply(x, function(xi) {
      identical(xi, "") | identical(xi, "NA") | identical(xi, ".")
    }, logical(1))
}

#' Convert check results to a standardized data frame
#'
#' @param check_result List containing check results from any check function
#' @param check_name Name of the check (optional, will be extracted from class if not provided)
#' @return Data frame with standardized check results
#' @importFrom dplyr bind_rows mutate
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
#' @param ... Multiple check result objects
#' @return Data frame with combined check results
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
#' This function captures or parses the console output from check functions
#' and converts it into a standardized data frame format.
#'
#' @param text Text string containing check function console output, or a connection to read from
#' @param capture_output If TRUE, the function will capture output from the provided function call
#' @param check_fn Function to call (only used if capture_output is TRUE)
#' @param ... Arguments to pass to check_fn (only used if capture_output is TRUE)
#' @return Data frame with standardized check results
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
  check_name <- NULL
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
            # Extract subject ID (assuming ID is 5 digits after "受试者")
            subj_start <- subj_pos + nchar("受试者")
            subj_id <- substr(line, subj_start, subj_start + 4)
            # Clean up any non-numeric characters
            SUBJID <- gsub("[^0-9]", "", subj_id)

            # Create a row for this subject
            result_rows[[i]] <- data.frame(
              check_name = check_name,
              has_deviation = has_deviation,
              message = message,
              details = line, # Just include this specific line
              SUBJID = SUBJID,
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
    check_name = check_name,
    has_deviation = has_deviation,
    message = message,
    details = details,
    SUBJID = NA_character_,
    stringsAsFactors = FALSE
  )

  return(result_df)
}

#' Capture and parse output from multiple check functions
#'
#' @param ... Named list of check functions to run
#' @param data Data to pass to each check function
#' @return Data frame with combined check results
#' @export
capture_check_results <- function(..., data) {
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

    # Parse the output
    parsed_result <- parse_check_output(text = output)

    # If check_name was not found in the output, use function name
    if (is.null(parsed_result$check_name) || length(parsed_result$check_name) == 0 ||
      all(is.na(parsed_result$check_name)) || all(parsed_result$check_name == "")) {
      parsed_result$check_name <- fn_name
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
#' @param visit_type Character string, visit type description (e.g., "筛选", "治疗周期1")
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
