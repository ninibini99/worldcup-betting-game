library(dplyr)

match_outcome <- function(home_goals, away_goals) {
  dplyr::case_when(
    home_goals > away_goals ~ "home",
    home_goals < away_goals ~ "away",
    home_goals == away_goals ~ "draw",
    TRUE ~ NA_character_
  )
}

# Points multiplier by round
round_multiplier <- function(match_id) {
  dplyr::case_when(
    stringr::str_starts(match_id, "R32_") ~ 2L,
    stringr::str_starts(match_id, "R16_") ~ 3L,
    stringr::str_starts(match_id, "QF_")  ~ 4L,
    stringr::str_starts(match_id, "SF_")  ~ 5L,
    stringr::str_starts(match_id, "F_")   ~ 6L,
    TRUE ~ 1L  # group stage
  )
}

score_predictions <- function(predictions, results) {
  predictions |>
    left_join(results, by = "match_id") |>
    filter(status == "FINISHED") |>
    mutate(
      multiplier     = round_multiplier(match_id),
      pred_outcome   = match_outcome(pred_home_goals, pred_away_goals),
      actual_outcome = match_outcome(home_goals, away_goals),
      pred_diff      = pred_home_goals - pred_away_goals,
      actual_diff    = home_goals - away_goals,
      exact_score    = pred_home_goals == home_goals & pred_away_goals == away_goals,
      correct_diff   = pred_diff == actual_diff,
      correct_outcome = pred_outcome == actual_outcome,
      base_points = case_when(
        exact_score     ~ 3L,
        correct_diff    ~ 2L,
        correct_outcome ~ 1L,
        TRUE            ~ 0L
      ),
      points = base_points * multiplier,
      reason = case_when(
        exact_score     ~ paste0("Exact score (×", multiplier, ")"),
        correct_diff    ~ paste0("Correct goal difference (×", multiplier, ")"),
        correct_outcome ~ paste0("Correct outcome (×", multiplier, ")"),
        TRUE            ~ "Wrong outcome"
      )
    )
}

build_leaderboard <- function(scores) {
  scores |>
    group_by(player) |>
    summarise(
      points          = sum(points, na.rm = TRUE),
      exact_scores    = sum(exact_score, na.rm = TRUE),
      correct_outcomes = sum(correct_outcome, na.rm = TRUE),
      matches_scored  = dplyr::n(),
      .groups = "drop"
    ) |>
    arrange(desc(points), desc(exact_scores), desc(correct_outcomes), player) |>
    mutate(rank = dplyr::row_number()) |>
    select(rank, everything())
}
