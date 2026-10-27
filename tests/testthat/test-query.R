test_that("GraphQL requests are encoded and variables are passed", {
  request <- weightxreps_request(
    query = "query Exercises($limit: Int!) { exercises(limit: $limit) { id } }",
    variables = list(limit = 3)
  )

  expect_s3_class(request, "httr2_request")
  expect_equal(request$url, "https://weightxreps.net/api/graphql")

  body <- request$body$data
  expect_equal(body$query, "query Exercises($limit: Int!) { exercises(limit: $limit) { id } }")
  expect_equal(body$variables, list(limit = 3))
})

test_that("successful GraphQL responses return data", {
  response <- structure(
    list(status_code = 200L, body = charToRaw('{"data":{"exercises":[{"id":1}]}}')),
    class = "httr2_response"
  )

  expect_equal(weightxreps_parse(response), list(exercises = list(list(id = 1))))
})

test_that("GraphQL errors include server messages", {
  response <- structure(
    list(
      status_code = 200L,
      body = charToRaw('{"errors":[{"message":"Unknown field exercises"}]}')
    ),
    class = "httr2_response"
  )

  expect_error(weightxreps_parse(response), "Unknown field exercises", class = "weightxreps_error")
})

test_that("invalid query input fails early", {
  expect_error(weightxreps_query(query = NULL), "query", fixed = TRUE)
})

test_that("invalid endpoint input fails early", {
  expect_error(weightxreps_query(query = "{ __typename }", endpoint = NULL), "endpoint", fixed = TRUE)
})

test_that("login mutation sends credentials without authorization", {
  request <- weightxreps_request(
    query = WEIGHTXREPS_LOGIN_QUERY,
    variables = list(u = "runner@example.com", p = "secret")
  )

  expect_equal(request$body$data$variables,
               list(u = "runner@example.com", p = "secret"))
  expect_false("Authorization" %in% names(request$headers))
})

test_that("authenticated requests add a bearer token", {
  request <- weightxreps_request(
    query = "{ getSession { user { id } } }",
    token = "example-token"
  )

  expect_equal(request$headers$Authorization, "Bearer example-token")
})

test_that("JWT login payload is decoded", {
  encode <- function(value) {
    encoded <- jsonlite::base64_enc(charToRaw(value))
    gsub("=+$", "", chartr("+/", "-_", encoded))
  }
  token <- paste(encode('{"alg":"none"}'),
                 encode('{"id":2751,"exp":1893456000}'),
                 "signature", sep = ".")

  session <- weightxreps_parse_login(list(data = list(login = token)))

  expect_equal(session$token, token)
  expect_equal(session$uid, 2751)
  expect_equal(session$exp, 1893456000)
})

test_that("login rejects malformed JWTs", {
  expect_error(
    weightxreps_parse_login(list(data = list(login = "not-a-jwt"))),
    "JWT",
    fixed = TRUE
  )
})

test_that("training log download requests the complete JEditorData union", {
  request <- weightxreps_request(
    query = WEIGHTXREPS_DOWNLOAD_LOGS_QUERY,
    token = "example-token"
  )
  query <- request$body$data$query

  expect_match(query, "downloadLogs", fixed = TRUE)
  expect_match(query, "... on JEditorEBlock", fixed = TRUE)
  expect_match(query, "... on JEditorText", fixed = TRUE)
  expect_match(query, "... on UTagValue", fixed = TRUE)
  expect_match(query, "e { id name type }", fixed = TRUE)
})

test_that("training log downloads can be written to JSON", {
  path <- tempfile(fileext = ".json")
  logs <- list(etags = list("Strength"), baseBW = 80)

  expect_identical(weightxreps_write_logs(logs, path), path)
  expect_equal(jsonlite::read_json(path, simplifyVector = FALSE), logs)
})

test_that("training log JSON output rejects invalid paths", {
  expect_error(weightxreps_write_logs(list(), NULL), "path", fixed = TRUE)
})

test_that("training log lists are converted to tibbles", {
  logs <- weightxreps_tibble_logs(list(
    did = list(
      list("__typename" = "JEditorDayTag", on = "2026-01-02"),
      list("__typename" = "JEditorEBlock", e = 17L,
           sets = list(list(v = 100, r = 5), list(v = 102.5, r = 5)))
    ),
    exercises = list(list(e = list(id = "17", name = "Squat", type = 1),
                          days = 8L, reps = 40L)),
    etags = list("Strength"),
    utags = list(list(id = "3", name = "Gym", automatic = FALSE)),
    baseBW = 80
  ))

  expect_s3_class(logs$did, "tbl_df")
  expect_s3_class(logs$exercises, "tbl_df")
  expect_s3_class(logs$utags, "tbl_df")
  expect_s3_class(logs$exercises$e[[1]], "tbl_df")
  expect_s3_class(logs$did$sets[[2]], "tbl_df")
  expect_equal(logs$did$on[[1]], "2026-01-02")
  expect_equal(logs$etags, list("Strength"))
})

test_that("single-day workout query uses the session uid and date variables", {
  request <- weightxreps_request(
    query = WEIGHTXREPS_JDAY_QUERY,
    variables = list(uid = "6889", ymd = "2026-10-20"),
    token = "example-token"
  )

  expect_match(request$body$data$query, "jday(uid: $uid, ymd: $ymd)", fixed = TRUE)
  expect_equal(request$body$data$variables,
               list(uid = "6889", ymd = "2026-10-20"))
  expect_equal(request$headers$Authorization, "Bearer example-token")
})

test_that("single-day workout query validates session and date", {
  expect_error(weightxreps_get_workout(list(token = "x"), "2026-10-20"),
               "uid", fixed = TRUE)
  expect_error(weightxreps_get_workout(list(token = "x", uid = 1), "2026-02-30"),
               "date", fixed = TRUE)
})

test_that("single workout response is converted to nested tibbles", {
  workout <- weightxreps_tibble_workout(list(
    log = "Training",
    bw = 80,
    eblocks = list(list(eid = 17L, sets = list(list(w = 100, r = 5)))),
    exercises = list(list(exercise = list(id = "17", name = "Squat", type = 1)))
  ))

  expect_s3_class(workout$eblocks, "tbl_df")
  expect_s3_class(workout$eblocks$sets[[1]], "tbl_df")
  expect_s3_class(workout$exercises, "tbl_df")
  expect_s3_class(workout$exercises$exercise[[1]], "tbl_df")
  expect_null(weightxreps_tibble_workout(NULL))
})
