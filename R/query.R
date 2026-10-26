WEIGHTXREPS_DEFAULT_ENDPOINT <- "https://weightxreps.net/api/graphql"
WEIGHTXREPS_LOGIN_QUERY <- "mutation Login($u: String!, $p: String!) {
  login(u: $u, p: $p)
}"
WEIGHTXREPS_DOWNLOAD_LOGS_QUERY <- "query DownloadLogs {
  downloadLogs {
    did {
      __typename
      ... on JEditorBWTag { bw }
      ... on JEditorDayTag { on }
      ... on JEditorEBlock {
        e
        sets { usebw v lb r s c rpe t d dunit type }
      }
      ... on JEditorNewExercise { newExercise }
      ... on JEditorText { text }
      ... on UTagValue { id tagid logid ymd type value }
    }
    exercises { e { id name type } days reps }
    etags
    utags { id name automatic }
    baseBW
  }
}"

weightxreps_resolve_token <- function(token) {
  if (is.null(token)) {
    return(NULL)
  }
  if (!is.character(token) || length(token) != 1L || !nzchar(token)) {
    stop("`token` must be one non-empty string or NULL", call. = FALSE)
  }
  token
}

#' Construct a request for the WeightXReps GraphQL API
#' @keywords internal
weightxreps_request <- function(query,
                                variables = list(),
                                endpoint = WEIGHTXREPS_DEFAULT_ENDPOINT,
                                token = NULL) {
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("`query` must be one non-empty string", call. = FALSE)
  }
  if (!is.character(endpoint) || length(endpoint) != 1L || !nzchar(endpoint)) {
    stop("`endpoint` must be one non-empty string", call. = FALSE)
  }
  if (!is.list(variables)) {
    stop("`variables` must be a list", call. = FALSE)
  }
  token <- weightxreps_resolve_token(token)

  request <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_body_json(list(query = query, variables = variables))

  if (!is.null(token)) {
    request <- httr2::req_headers(request, Authorization = paste("Bearer", token))
  }
  request
}

weightxreps_error <- function(message) {
  structure(
    list(message = message),
    class = c("weightxreps_error", "error", "condition")
  )
}

#' Parse a WeightXReps GraphQL response
#' @keywords internal
weightxreps_parse <- function(response) {
  status <- httr2::resp_status(response)
  body <- httr2::resp_body_raw(response)
  parsed <- jsonlite::parse_json(rawToChar(body), simplifyVector = FALSE)

  if (status < 200L || status >= 300L) {
    message <- paste0("WeightXReps API returned HTTP status ", status)
    details <- parsed$error %||% parsed$message
    if (!is.null(details)) {
      message <- paste(message, details, sep = ": ")
    }
    stop(weightxreps_error(message))
  }

  if (length(parsed$errors) > 0L) {
    messages <- vapply(parsed$errors, function(error) {
      if (is.null(error$message)) "Unknown GraphQL error" else error$message
    }, character(1L))
    stop(weightxreps_error(paste(messages, collapse = "; ")))
  }

  parsed$data
}

weightxreps_write_logs <- function(logs, path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be one non-empty string", call. = FALSE)
  }
  jsonlite::write_json(logs, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  invisible(path)
}

weightxreps_parse_login <- function(parsed) {
  token <- parsed$data$login
  if (!is.character(token) || length(token) != 1L || !nzchar(token)) {
    stop(weightxreps_error("WeightXReps login did not return a JWT"))
  }

  parts <- strsplit(token, ".", fixed = TRUE)[[1L]]
  if (length(parts) != 3L || !nzchar(parts[[2L]])) {
    stop(weightxreps_error("WeightXReps login returned an invalid JWT"))
  }

  payload <- chartr("-_", "+/", parts[[2L]])
  payload <- paste0(payload, strrep("=", (4L - nchar(payload) %% 4L) %% 4L))
  claims <- tryCatch(
    jsonlite::parse_json(rawToChar(jsonlite::base64_dec(payload)), simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (!is.list(claims) || !is.numeric(claims$id) || length(claims$id) != 1L ||
      !is.numeric(claims$exp) || length(claims$exp) != 1L) {
    stop(weightxreps_error("WeightXReps login returned a JWT with invalid claims"))
  }

  list(token = token, uid = as.integer(claims$id), exp = as.numeric(claims$exp))
}

#' Log in to the WeightXReps GraphQL API
#'
#' @param username WeightXReps login email or username.
#' @param password WeightXReps password.
#' @param endpoint The GraphQL endpoint URL.
#' @return A list containing `token`, numeric `uid`, and expiration `exp`.
#' @export
weightxreps_login <- function(username,
                              password,
                              endpoint = WEIGHTXREPS_DEFAULT_ENDPOINT) {
  if (!is.character(username) || length(username) != 1L || !nzchar(username)) {
    stop("`username` must be one non-empty string", call. = FALSE)
  }
  if (!is.character(password) || length(password) != 1L || !nzchar(password)) {
    stop("`password` must be one non-empty string", call. = FALSE)
  }

  request <- weightxreps_request(
    WEIGHTXREPS_LOGIN_QUERY,
    variables = list(u = username, p = password),
    endpoint = endpoint
  )
  response <- httr2::req_perform(request)
  parsed <- jsonlite::parse_json(rawToChar(httr2::resp_body_raw(response)),
                                 simplifyVector = FALSE)
  if (httr2::resp_status(response) != 200L || length(parsed$errors) > 0L) {
    weightxreps_parse(response)
  }
  weightxreps_parse_login(parsed)
}

#' Download all training logs for the authenticated user
#'
#' @param token A WeightXReps JWT returned by [weightxreps_login()].
#' @param endpoint The GraphQL endpoint URL.
#' @param file Optional path where the complete download should be written as JSON.
#' @return The `downloadLogs` data, invisibly if `file` is supplied.
#' @export
weightxreps_download_logs <- function(token,
                                      endpoint = WEIGHTXREPS_DEFAULT_ENDPOINT,
                                      file = NULL) {
  token <- weightxreps_resolve_token(token)
  if (is.null(token)) {
    stop("`token` is required", call. = FALSE)
  }

  logs <- weightxreps_query(
    WEIGHTXREPS_DOWNLOAD_LOGS_QUERY,
    endpoint = endpoint,
    token = token
  )$downloadLogs

  if (!is.null(file)) {
    weightxreps_write_logs(logs, file)
    return(invisible(logs))
  }
  logs
}

#' Query the WeightXReps GraphQL API
#'
#' @param query A non-empty GraphQL query or mutation string.
#' @param variables A named list of GraphQL variables.
#' @param endpoint The GraphQL endpoint URL.
#' @param token An optional WeightXReps JWT returned by [weightxreps_login()].
#' @return The `data` field from the GraphQL response.
#' @export
weightxreps_query <- function(query,
                              variables = list(),
                              endpoint = WEIGHTXREPS_DEFAULT_ENDPOINT,
                              token = NULL) {
  request <- weightxreps_request(query, variables, endpoint, token)
  response <- httr2::req_perform(request)
  weightxreps_parse(response)
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}
