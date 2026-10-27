# weightxrepsR

An R client for the public WeightXReps GraphQL API.

```r
weightxreps_query(
  'query Exercises($limit: Int!) {
     exercises(limit: $limit) { id }
   }',
  variables = list(limit = 10)
)
```

The default endpoint is `https://weightxreps.net/api/graphql`. A staging endpoint
can be supplied with `endpoint` when needed:

```r
weightxreps_query(
  "{ __typename }",
  endpoint = "https://staging.weightxreps.net/api/graphql"
)
```

GraphQL API errors are reported as R errors and include the server's message.

## Login

```r
session <- weightxreps_login(
  username = Sys.getenv("WEIGHTXREPS_USERNAME"),
  password = Sys.getenv("WEIGHTXREPS_PASSWORD")
)

weightxreps_query(
  "query { getSession { user { id uname } } }",
  token = session$token
)
```

`weightxreps_login()` returns `token`, numeric `uid`, and Unix expiry `exp`. The
package keeps credentials and tokens only in memory; callers decide whether to
persist a token. Pass `token` to authenticated calls with `weightxreps_query()`.
## Download training logs

```r
logs <- weightxreps_download_logs(session$token)

# Download the complete structured response to JSON.
weightxreps_download_logs(session$token, file = "training-logs.json")
```

The result contains `did`, exercise statistics, exercise tags, user tags, and
`baseBW` as a list. `did`, `exercises`, and `utags` are tibbles; exercise
references and exercise-set unions remain nested tibble columns. Writing the
result to JSON preserves the complete API export.

## Get one workout

```r
workout <- weightxreps_get_workout(session, "2026-10-20")
```

Pass the session returned by `weightxreps_login()` and a `Date` or
`YYYY-MM-DD` string. The result includes the workout log and bodyweight, with
exercise blocks, sets, and exercise details as tibbles; it returns `NULL` when
there is no workout on that date.