library(httr2)
library(dplyr)
library(readr)

# football-data.org API key from environment
api_key <- Sys.getenv("FOOTBALL_API_KEY")

# FIFA World Cup 2026 competition ID on football-data.org is WC
base_url <- "https://api.football-data.org/v4/competitions/WC/matches"

resp <- httr2::request(base_url) |>
  httr2::req_headers("X-Auth-Token" = api_key) |>
  httr2::req_perform()

data <- httr2::resp_body_json(resp, simplifyVector = TRUE)
matches <- data$matches

if (is.null(matches) || nrow(matches) == 0) {
  message("No matches returned from API.")
  quit(status = 0)
}

# Load fixtures to map team codes to match_ids
fixtures <- readr::read_csv("data/fixtures.csv", show_col_types = FALSE)

# Build a lookup: home_code + away_code -> match_id
fixture_lookup <- fixtures |>
  dplyr::mutate(key = paste0(home_code, "_", away_code)) |>
  dplyr::select(key, match_id)

# Parse API response
results <- matches |>
  dplyr::mutate(
    home_code = homeTeam$tla,
    away_code = awayTeam$tla,
    key       = paste0(home_code, "_", away_code),
    home_goals = score$fullTime$home,
    away_goals = score$fullTime$away,
    status     = status,
    updated_at = Sys.time()
  ) |>
  dplyr::left_join(fixture_lookup, by = "key") |>
  dplyr::filter(!is.na(match_id)) |>
  dplyr::select(match_id, home_goals, away_goals, status, updated_at)

readr::write_csv(results, "data/results.csv")
message("Updated results.csv with ", nrow(results), " matches.")
