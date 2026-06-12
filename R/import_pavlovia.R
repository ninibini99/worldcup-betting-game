library(readr)
library(dplyr)
library(tidyr)
library(stringr)

official_players <- c("Nina", "Saoirse", "Stefan", "Luki", "Charlotte")

clean_player_name <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::case_when(
    stringr::str_to_lower(x) == "nina"      ~ "Nina",
    stringr::str_to_lower(x) == "saoirse"   ~ "Saoirse",
    stringr::str_to_lower(x) == "stefan"    ~ "Stefan",
    stringr::str_to_lower(x) == "luki"      ~ "Luki",
    stringr::str_to_lower(x) == "charlotte" ~ "Charlotte",
    TRUE ~ x
  )
}

# Read all CSVs from data/raw/pavlovia/
csv_files <- list.files("data/raw/pavlovia", pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  message("No CSV files found in data/raw/pavlovia/")
  quit(status = 0)
}

raw <- purrr::map_dfr(csv_files, function(f) {
  readr::read_csv(f, show_col_types = FALSE) |>
    dplyr::mutate(source_file = basename(f))
})

# Keep only complete responses
if ("isComplete" %in% names(raw)) {
  raw <- raw |> dplyr::filter(isComplete == TRUE)
}

# Extract player name and submitted time
raw <- raw |>
  dplyr::mutate(
    player       = clean_player_name(`block_1/Name`),
    submitted_at = as.POSIXct(responseDate)
  )

# Load fixtures for join
fixtures <- readr::read_csv("data/fixtures.csv", show_col_types = FALSE)

# Pivot prediction columns only (exclude block_1/Name and bonus columns)
prediction_cols <- names(raw) |>
  stringr::str_subset("^block_1/") |>
  setdiff(c("block_1/Name")) |>
  # Only keep columns that match pattern block_1/XX_YYY (match + team code)
  stringr::str_subset("^block_1/[A-L][0-9]_[A-Z]{2,3}$")

long <- raw |>
  dplyr::select(player, submitted_at, source_file, all_of(prediction_cols)) |>
  tidyr::pivot_longer(
    cols            = all_of(prediction_cols),
    names_to        = "question",
    values_to       = "goals",
    values_transform = list(goals = as.character)
  ) |>
  dplyr::mutate(
    # Remove block_1/ prefix
    question  = stringr::str_remove(question, "^block_1/"),
    # Split into match_id (e.g. A1) and team_code (e.g. MEX)
    match_id  = stringr::str_extract(question, "^[A-L][0-9]"),
    team_code = stringr::str_extract(question, "[A-Z]{2,3}$"),
    goals     = suppressWarnings(as.integer(goals))
  ) |>
  dplyr::filter(!is.na(goals))

# Pivot wide: one row per player + match with home and away goals
predictions <- long |>
  dplyr::left_join(
    fixtures |> dplyr::select(match_id, home_code, away_code),
    by = "match_id"
  ) |>
  dplyr::mutate(
    side = dplyr::case_when(
      team_code == home_code ~ "pred_home_goals",
      team_code == away_code ~ "pred_away_goals",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(side)) |>
  tidyr::pivot_wider(
    id_cols     = c(player, match_id, submitted_at, source_file),
    names_from  = side,
    values_from = goals
  ) |>
  dplyr::filter(!is.na(pred_home_goals), !is.na(pred_away_goals))

# Keep only official players
predictions <- predictions |>
  dplyr::filter(player %in% official_players)

# If someone submitted multiple times, keep the latest
predictions <- predictions |>
  dplyr::arrange(dplyr::desc(submitted_at)) |>
  dplyr::distinct(player, match_id, .keep_all = TRUE)

# Final column selection
predictions <- predictions |>
  dplyr::select(player, match_id, pred_home_goals, pred_away_goals, submitted_at)

readr::write_csv(predictions, "data/predictions.csv")
message("Wrote ", nrow(predictions), " predictions to data/predictions.csv")
