#' @noRd
.ai_check_packages <- function() {
  for (pkg in c("httr2", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for AI features.\n",
        "Install it with: install.packages('", pkg, "')",
        call. = FALSE
      )
    }
  }
}

#' Resolve AI API credentials from environment variables.
#' @noRd
.ai_creds <- function() {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  base_url <- Sys.getenv("OPENROUTER_BASE_URL")
  model <- Sys.getenv("OPENROUTER_MODEL")

  missing <- character()
  if (!nzchar(api_key)) missing <- c(missing, "OPENROUTER_API_KEY")
  if (!nzchar(base_url)) missing <- c(missing, "OPENROUTER_BASE_URL")
  if (!nzchar(model)) missing <- c(missing, "OPENROUTER_MODEL")
  if (length(missing) > 0) {
    stop(
      "Missing AI environment variables: ", paste(missing, collapse = ", "),
      ". Set them in .Renviron.",
      call. = FALSE
    )
  }

  list(api_key = api_key, base_url = base_url, model = model)
}

#' Call an OpenAI-compatible chat completions API.
#' @noRd
.ai_chat_completion <- function(messages,
                                api_key,
                                base_url,
                                model,
                                temperature = 0.2,
                                timeout = 180,
                                verbose = FALSE) {
  .ai_check_packages()

  endpoint <- paste0(gsub("/$", "", base_url), "/chat/completions")
  response <- tryCatch(
    {
      httr2::request(endpoint) |>
        httr2::req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(list(
          model = model,
          messages = messages,
          temperature = temperature
        )) |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()
    },
    error = function(e) {
      stop("AI API request failed (", endpoint, "): ", e$message, call. = FALSE)
    }
  )

  body <- httr2::resp_body_json(response)
  if (!is.null(body[["error"]])) {
    stop("AI API returned an error: ", body[["error"]][["message"]], call. = FALSE)
  }

  content <- body[["choices"]][[1]][["message"]][["content"]]
  if (verbose) {
    cat("--- AI Raw Response ---\n", content, "\n-----------------------\n")
  }
  content
}

#' Call AI and parse a JSON object from the response text.
#' @noRd
.ai_chat_json <- function(system,
                          user,
                          temperature = 0.2,
                          timeout = 180,
                          verbose = FALSE) {
  creds <- .ai_creds()
  raw <- .ai_chat_completion(
    messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)
    ),
    api_key = creds$api_key,
    base_url = creds$base_url,
    model = creds$model,
    temperature = temperature,
    timeout = timeout,
    verbose = verbose
  )
  .parse_ai_json(raw)
}

#' Extract a JSON object from AI response text.
#' @noRd
.parse_ai_json <- function(text) {
  .ai_check_packages()

  if (!is.character(text) || length(text) != 1) {
    stop("AI response is not valid text.", call. = FALSE)
  }

  json_block_pattern <- "```(?:json)?\\s*([\\s\\S]*?)```"
  m <- regexpr(json_block_pattern, text, perl = TRUE)
  if (m[1] != -1) {
    block <- regmatches(text, regexec(json_block_pattern, text, perl = TRUE))[[1]][2]
    return(jsonlite::fromJSON(block, simplifyVector = TRUE))
  }

  trimmed <- trimws(text)
  start <- regexpr("[\\[{]", trimmed, perl = TRUE)[1]
  if (is.na(start) || start < 1) {
    stop("No JSON object found in AI response.", call. = FALSE)
  }
  jsonlite::fromJSON(substring(trimmed, start), simplifyVector = TRUE)
}
