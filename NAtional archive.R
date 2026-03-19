library(httr2)
library(jsonlite)
library(tidyverse)


#https://catalog.archives.gov/id/569
#https://catalog.archives.gov/id/12044361

#https://github.com/usnationalarchives/Catalog-API?tab=readme-ov-file#how-can-i-use-naras-data

library(httr2)

# Store your API key
api_key <- "YOUR_API_KEY"
base_url <- "https://catalog.archives.gov/api/v2"

# Create a base request with common headers
base_request <- function() {
  request(base_url) |>
    req_headers(
      "Content-Type" = "application/json",
      "x-api-key" = api_key
    )
}

# ── GET: Search records ───────────────────────────────────────────
search_records <- function(query, rows = 10, offset = 0) {
  base_request() |>
    req_url_path_append("records/search") |>
    req_url_query(q = query, rows = rows, offset = offset) |>
    req_perform() |>
    resp_body_json()
}

# ── POST: Add a tag (write operation — requires userId) ───────────
post_tag <- function(tag, target_na_id, user_uuid) {
  base_request() |>
    req_url_path_append("tags") |>
    req_method("POST") |>
    req_body_json(list(
      tag = tag,
      targetNaId = target_na_id,
      userId = user_uuid
    )) |>
    req_perform() |>
    resp_body_json()
}

# ── Usage ─────────────────────────────────────────────────────────
results <- search_records("constitution")
post_tag("example tag", 1667751, "YOUR-USER-UUID")
