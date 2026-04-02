#' Generate markdown report from check results
#'
#' @param checks_df Data frame containing combined check results
#' @param output_file Path to output markdown file (optional)
#' @param include_no_deviation Whether to include checks with no deviations (default: FALSE)
#' @param title Report title (default: "Study Deviation Report")
#' @return Invisibly returns the report content as a character string
#'
#' @examples
#' \dontrun{
#' # Combine check results first
#' all_results <- combine_check_results(res_1, res_2, res_3)
#'
#' # Generate markdown report to file
#' generate_markdown_report(all_results, output_file = "report.md")
#'
#' # Get report content as string
#' report_text <- generate_markdown_report(all_results)
#'
#' # Include checks with no deviations
#' generate_markdown_report(all_results,
#'   output_file = "full_report.md",
#'   include_no_deviation = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{generate_html_report}} for HTML format
#' \code{\link{generate_excel_report}} for Excel format
#' \code{\link{combine_check_results}} for combining check results
#'
#' @family report generation
#'
#' @importFrom dplyr filter group_by summarize arrange n bind_rows select_if if_else
#' @export
generate_markdown_report <- function(checks_df, output_file = NULL,
                                     include_no_deviation = FALSE,
                                     title = "Study Deviation Report") {
  # Check if knitr is available
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for markdown reports. Please install it.",
      call. = FALSE
    )
  }

  # Parameter validation

  if (!is.data.frame(checks_df)) {
    stop("'checks_df' must be a data frame", call. = FALSE)
  }
  if (nrow(checks_df) == 0) {
    warning("'checks_df' is empty, report will have no content")
  }
  if (!is.null(output_file) && (!is.character(output_file) || length(output_file) != 1)) {
    stop("'output_file' must be a character string or NULL", call. = FALSE)
  }
  if (!is.logical(include_no_deviation) || length(include_no_deviation) != 1) {
    stop("'include_no_deviation' must be a single logical value", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1) {
    stop("'title' must be a single character string", call. = FALSE)
  }

  # Initialize report content
  report <- c()

  # Add title
  report <- c(report, paste0("# ", title), "")
  report <- c(report, paste0("Report generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "")

  # Calculate summary statistics
  summary_stats <- checks_df %>%
    group_by(check_name) %>%
    summarize(
      has_deviation = any(has_deviation),
      deviation_count = sum(if_else(has_deviation, n(), 0L)),
      .groups = "drop"
    ) %>%
    arrange(check_name)

  # Add summary section
  report <- c(report, "## Summary", "")

  # Create summary table
  summary_table <- knitr::kable(
    summary_stats,
    col.names = c("Check Type", "Has Deviation", "Deviation Count"),
    format = "markdown"
  )
  report <- c(report, summary_table, "")

  # Add detailed section for each check
  report <- c(report, "## Detailed Check Results", "")

  # Loop through checks in summary_stats order
  for (i in seq_len(nrow(summary_stats))) {
    current_check_name <- summary_stats$check_name[i]
    current_has_deviation <- summary_stats$has_deviation[i]

    # Skip if no deviation and we're not including those
    if (!current_has_deviation && !include_no_deviation) {
      next
    }

    # Get this check's results
    check_df <- checks_df %>%
      filter(check_name == !!current_check_name)

    # Add check header
    report <- c(report, paste0("### ", current_check_name), "")

    # Add deviation status
    status <- if (current_has_deviation) "YES" else "NO"
    report <- c(report, paste0("**Has deviation:** ", status), "")

    # Add messages if available
    if (!is.null(check_df$message) && any(!is.na(check_df$message))) {
      report <- c(report, "**Findings:**", "")
      messages <- unique(check_df$message[!is.na(check_df$message)])
      for (msg in messages) {
        report <- c(report, paste0("- ", msg), "")
      }
    }

    # Add details if available (now as a text block rather than a table)
    if (current_has_deviation && "details" %in% names(check_df)) {
      report <- c(report, "**Deviation Details:**", "")

      # Get unique detail messages (non-NA)
      detail_msgs <- unique(check_df$details[!is.na(check_df$details)])

      if (length(detail_msgs) > 0) {
        # Add each detail line, preserving the original format
        for (detail in detail_msgs) {
          # Split by newlines and add each line
          detail_lines <- paste0(unlist(strsplit(detail, "\n")), "  ")
          report <- c(report, detail_lines, "")
        }
      } else {
        report <- c(report, "No additional details available", "")
      }
    }

    # Add separator
    report <- c(report, "---", "")
  }

  # Create complete report as a single string
  report_text <- paste(report, collapse = "\n")

  # Write to file if output_file is provided
  if (!is.null(output_file)) {
    writeLines(report_text, output_file)
    message("Report written to: ", output_file)
  }

  # Return report invisibly
  invisible(report_text)
}

#' Generate HTML report from check results
#'
#' @param checks_df Data frame containing combined check results
#' @param output_file Path to output HTML file
#' @param include_no_deviation Whether to include checks with no deviations (default: FALSE)
#' @param title Report title (default: "Study Deviation Report")
#' @param css_file Path to CSS file for styling (optional)
#' @return Invisibly returns the output file path
#'
#' @examples
#' \dontrun{
#' # Combine check results first
#' all_results <- combine_check_results(res_1, res_2, res_3)
#'
#' # Generate HTML report
#' generate_html_report(all_results, output_file = "report.html")
#'
#' # With custom title
#' generate_html_report(all_results,
#'   output_file = "report.html",
#'   title = "PD Report - Study ABC"
#' )
#' }
#'
#' @seealso
#' \code{\link{generate_markdown_report}} for Markdown format
#' \code{\link{generate_excel_report}} for Excel format
#'
#' @family report generation
#'
#' @export
generate_html_report <- function(checks_df, output_file,
                                 include_no_deviation = FALSE,
                                 title = "Study Deviation Report",
                                 css_file = NULL) {
  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for HTML reports. Please install it.",
      call. = FALSE
    )
  }

  # Parameter validation
  if (!is.data.frame(checks_df)) {
    stop("'checks_df' must be a data frame", call. = FALSE)
  }
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1) {
    stop("'output_file' must be a single character string", call. = FALSE)
  }
  if (!is.logical(include_no_deviation) || length(include_no_deviation) != 1) {
    stop("'include_no_deviation' must be a single logical value", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1) {
    stop("'title' must be a single character string", call. = FALSE)
  }
  if (!is.null(css_file) && (!is.character(css_file) || length(css_file) != 1)) {
    stop("'css_file' must be a single character string or NULL", call. = FALSE)
  }

  # Create temporary markdown file
  temp_md <- tempfile(fileext = ".md")
  on.exit(unlink(temp_md), add = TRUE)

  # Generate markdown content
  generate_markdown_report(
    checks_df = checks_df,
    output_file = temp_md,
    include_no_deviation = include_no_deviation,
    title = title
  )

  # Set render options
  html_options <- list(
    self_contained = TRUE,
    highlight = "tango"
  )

  # Add custom CSS if provided
  if (!is.null(css_file)) {
    html_options$css <- css_file
  }

  # Create output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  # Render markdown to HTML
  rmarkdown::render(
    input = temp_md,
    output_file = basename(output_file),
    output_dir = output_dir,
    output_format = do.call(rmarkdown::html_document, html_options),
    quiet = TRUE
  )

  # Return output file path invisibly
  invisible(output_file)
}

#' Generate Excel report from check results
#'
#' @param checks_df Data frame containing combined check results
#' @param output_file Path to output Excel file
#' @param include_no_deviation Whether to include checks with no deviations (default: FALSE)
#' @param title Report title (default: "Study Deviation Report")
#' @param report_cols Character vector of column names to include in the "All Deviations"
#'   sheet. Only columns that exist in \code{checks_df} will be included.
#'   Default: \code{c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")}.
#' @return Invisibly returns the output file path
#'
#' @examples
#' \dontrun{
#' # Combine check results first
#' all_results <- combine_check_results(res_1, res_2, res_3)
#'
#' # Generate Excel report
#' generate_excel_report(all_results, output_file = "pd_report.xlsx")
#'
#' # Include all checks (even those without deviations)
#' generate_excel_report(all_results,
#'   output_file = "full_report.xlsx",
#'   include_no_deviation = TRUE
#' )
#'
#' # Specify custom columns to output
#' generate_excel_report(all_results,
#'   output_file = "custom_report.xlsx",
#'   report_cols = c("PDNO", "SITEID", "SUBJID", "TBNAME", "PDCAT", "DESCRIPTION")
#' )
#' }
#'
#' @seealso
#' \code{\link{generate_markdown_report}} for Markdown format
#' \code{\link{generate_html_report}} for HTML format
#' \code{\link{combine_check_results}} for combining check results
#'
#' @family report generation
#'
#' @importFrom dplyr desc across all_of mutate select arrange
#' @importFrom rlang .data
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook setColWidths createStyle addFilter addStyle
#' @export
generate_excel_report <- function(checks_df, output_file,
                                  include_no_deviation = FALSE,
                                  title = "Study Deviation Report",
                                  report_cols = c("PDNO", "SITEID", "SUBJID", "TBNAME", "DESCRIPTION")) {
  # Check if openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel reports. Please install it.",
      call. = FALSE
    )
  }

  # Parameter validation
  if (!is.data.frame(checks_df)) {
    stop("'checks_df' must be a data frame", call. = FALSE)
  }
  if (missing(output_file) || !is.character(output_file) || length(output_file) != 1) {
    stop("'output_file' must be a single character string", call. = FALSE)
  }
  if (!is.logical(include_no_deviation) || length(include_no_deviation) != 1) {
    stop("'include_no_deviation' must be a single logical value", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1) {
    stop("'title' must be a single character string", call. = FALSE)
  }
  if (!is.character(report_cols) || length(report_cols) == 0) {
    stop("'report_cols' must be a non-empty character vector", call. = FALSE)
  }

  # Filter to include only deviations if needed
  all_checks_df <- if (!include_no_deviation) {
    checks_df %>%
      filter(has_deviation)
  } else {
    checks_df
  }

  # Create workbook

  wb <- openxlsx::createWorkbook()

  # Add summary sheet
  openxlsx::addWorksheet(wb, "Summary")

  # Write title to summary sheet
  openxlsx::writeData(wb, "Summary", title, startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "Summary",
    paste0("Report generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    startRow = 2, startCol = 1
  )

  # Create summary statistics
  summary_stats <- all_checks_df %>%
    group_by(check_name) %>%
    summarize(
      has_deviation = any(has_deviation),
      deviation_count = sum(if_else(has_deviation, n(), 0L)),
      .groups = "drop"
    ) %>%
    arrange(desc(has_deviation), desc(deviation_count))

  # Write summary stats
  openxlsx::writeData(wb, "Summary", "Check Type Summary", startRow = 4, startCol = 1)
  openxlsx::writeData(wb, "Summary", summary_stats,
    startRow = 5, startCol = 1,
    colNames = TRUE, rowNames = FALSE
  )

  # Define styles
  header_style <- openxlsx::createStyle(
    fontName = "\u5b8b\u4f53",
    fontSize = 12,
    halign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    fgFill = "#EDF2F9",
    fontColour = "#112277",
    wrapText = TRUE
  )

  # Create all deviations sheet with all details in one place
  openxlsx::addWorksheet(wb, "All Deviations")

  available_cols <- intersect(report_cols, names(all_checks_df))
  if (length(available_cols) == 0) {
    stop("None of the specified 'report_cols' exist in 'checks_df'", call. = FALSE)
  }
  all_deviations_df <- all_checks_df %>%
    select(all_of(available_cols)) %>%
    arrange(.data$PDNO, .data$SUBJID)

  # Write the data to the sheet
  openxlsx::writeData(wb, "All Deviations", all_deviations_df,
    startRow = 1, startCol = 1,
    colNames = TRUE, rowNames = FALSE
  )

  # Apply formatting to the header row
  openxlsx::addStyle(wb, "All Deviations",
    style = header_style,
    rows = 1, cols = seq_len(ncol(all_deviations_df))
  )

  # Apply formatting to data rows
  body_style <- openxlsx::createStyle(
    fontName = "\u5b8b\u4f53",
    fontSize = 10,
    halign = "left",
    border = "TopBottomLeftRight",
    wrapText = TRUE
  )
  data_rows <- seq(2, nrow(all_deviations_df) + 1)
  openxlsx::addStyle(wb, "All Deviations",
    style = body_style,
    rows = data_rows, cols = seq_len(ncol(all_deviations_df)),
    gridExpand = TRUE
  )

  # Add filtering
  openxlsx::addFilter(wb, "All Deviations", row = 1, cols = seq_len(ncol(all_deviations_df)))

  # Calculate adaptive column widths: based on content, capped at max_col_width
  min_col_width <- 8
  max_col_width <- 50
  col_widths <- vapply(seq_len(ncol(all_deviations_df)), function(j) {
    col_values <- as.character(all_deviations_df[[j]])
    col_values <- col_values[!is.na(col_values)]
    header_len <- nchar(names(all_deviations_df)[j])
    content_len <- if (length(col_values) > 0) max(nchar(col_values)) else 0
    min(max(header_len + 4, content_len * 0.8, min_col_width), max_col_width)
  }, numeric(1))
  openxlsx::setColWidths(wb, "All Deviations",
    cols = seq_len(ncol(all_deviations_df)),
    widths = col_widths
  )

  # Save workbook
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

  # Return output file path invisibly
  invisible(output_file)
}
