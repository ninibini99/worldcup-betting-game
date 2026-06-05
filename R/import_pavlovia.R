library(tidyverse)
library(readr)
library(stringr)
library(lubridate)

# Official player names
official_players <- c("Nina", "Saoirse", "Stefan", "Luki", "Charlotte")

clean_player_name <- function(x) {
  x <- str_squish(as.character(x))
  
  case_when(
    str_to_lower(x) == "nina" ~ "Nina",
    str_to_lower(x) == "saoirse" ~ "Saoirse",
    str_to_lower(x) == "stefan" ~ "Stefan",
    str_to_lower(x) == "luki" ~ "Luki",
    str_to_lower(x) == "charlotte" ~ "Charlotte",
    TRUE ~ x
  )
}

# Read fixture table
fixtures <- read_csv("data/fixtures.csv", show_col_types = FALSE)

# This script expects fixtures.csv to contain:
# match_id, home_code, away_code
#
# Example:
# A1,MEX,RSA
# A2,CZE,KOR

required_fixture_cols <- c("match_id", "home_code", "away_code")

missing_fixture_cols <- setdiff(required_fixture_cols, names(fixtures))

if (length(missing_fixture_cols) > 0) {
  stop(
    "fixtures.csv is missing these required columns: ",
    paste(missing_fixture_cols, collapse = ", "),
    "\nPlease add match_id, home_code, and away_code to data/fixtures.csv."
  )
}

# Find Pavlovia CSV files
pavlovia_files <- list.files(
  "data/raw/pavlovia",
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(pavlovia_files) == 0) {
  message("No Pavlovia files found. Keeping existing data/predictions.csv unchanged.")
  quit(save = "no", status = 0)
}

read_one_pavlovia_file <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  
  if (!"block_1/Name" %in% names(raw)) {
    stop("Could not find column block_1/Name in: ", path)
  }
  
  if (!"responseDate" %in% names(raw)) {
    stop("Could not find column responseDate in: ", path)
  }
  
  # Keep only completed responses if the column exists
  if ("isComplete" %in% names(raw)) {
    raw <- raw |>
      filter(isComplete == TRUE | isComplete == "true" | isComplete == "TRUE" | isComplete == 1)
  }
  
  # Convert wide Pavlovia columns to long format
  long <- raw |>
    mutate(
      player = clean_player_name(`block_1/Name`),
      submitted_at = responseDate,
      source_file = basename(path)
    ) |>
    filter(player %in% official_players) |>
    select(
      player,
      submitted_at,
      source_file,
      starts_with("block_1/")
    ) |>
    select(
      -`block_1/Name`
    ) |>
    pivot_longer(
      cols = starts_with("block_1/"),
      names_to = "question",
      values_to = "goals",
      values_transform = list(goals = as.character)
    ) |>
    mutate(
      question_clean = str_remove(question, "^block_1/"),
      match_id = str_extract(question_clean, "^[A-L][0-9]+"),
      team_code = str_extract(question_clean, "(?<=_)[A-Z]{3}$"),
      goals = suppressWarnings(as.integer(goals))
    ) |>
    filter(!is.na(match_id), !is.na(team_code), !is.na(goals))
  
  long
}

pavlovia_long <- map_dfr(pavlovia_files, read_one_pavlovia_file)

# Join with fixtures to identify home and away teams
home_preds <- pavlovia_long |>
  inner_join(
    fixtures |> select(match_id, home_code),
    by = "match_id"
  ) |>
  filter(team_code == home_code) |>
  transmute(
    player,
    match_id,
    submitted_at,
    source_file,
    pred_home_goals = goals
  )

away_preds <- pavlovia_long |>
  inner_join(
    fixtures |> select(match_id, away_code),
    by = "match_id"
  ) |>
  filter(team_code == away_code) |>
  transmute(
    player,
    match_id,
    submitted_at,
    source_file,
    pred_away_goals = goals
  )

predictions <- home_preds |>
  inner_join(
    away_preds,
    by = c("player", "match_id", "submitted_at", "source_file")
  ) |>
  arrange(player, match_id)

# If someone submits multiple times, keep their latest submission per match
predictions <- predictions |>
  mutate(submitted_at_parsed = suppressWarnings(ymd_hms(submitted_at))) |>
  group_by(player, match_id) |>
  arrange(desc(submitted_at_parsed), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(
    player,
    match_id,
    pred_home_goals,
    pred_away_goals,
    submitted_at
  ) |>
  arrange(match_id, player)

write_csv(predictions, "data/predictions.csv")

message("Imported Pavlovia predictions: ", nrow(predictions), " rows")
message("Written to data/predictions.csv")