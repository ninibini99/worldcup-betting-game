library(dplyr)

match_outcome <- function(home_goals, away_goals) {
  dplyr::case_when(
    home_goals > away_goals ~ "home",
    home_goals < away_goals ~ "away",
    home_goals == away_goals ~ "draw",
    TRUE ~ NA_character_
  )
}

score_predictions <- function(predictions, results) {
  predictions |>
    left_join(results, by = "match_id") |>
    filter(status == "FINISHED") |>
    mutate(
      pred_outcome = match_outcome(pred_home_goals, pred_away_goals),
      actual_outcome = match_outcome(home_goals, away_goals),
      pred_diff = pred_home_goals - pred_away_goals,
      actual_diff = home_goals - away_goals,
      exact_score = pred_home_goals == home_goals & pred_away_goals == away_goals,
      correct_diff = pred_diff == actual_diff,
      correct_outcome = pred_outcome == actual_outcome,
      points = case_when(
        exact_score ~ 3L,
        correct_diff ~ 2L,
        correct_outcome ~ 1L,
        TRUE ~ 0L
      ),
      reason = case_when(
        exact_score ~ "Exact score",
        correct_diff ~ "Correct goal difference",
        correct_outcome ~ "Correct outcome",
        TRUE ~ "Wrong outcome"
      )
    )
}

build_leaderboard <- function(scores) {
  scores |>
    group_by(player) |>
    summarise(
      points = sum(points, na.rm = TRUE),
      exact_scores = sum(exact_score, na.rm = TRUE),
      correct_outcomes = sum(correct_outcome, na.rm = TRUE),
      matches_scored = dplyr::n(),
      .groups = "drop"
    ) |>
    arrange(desc(points), desc(exact_scores), desc(correct_outcomes), player) |>
    mutate(rank = dplyr::row_number()) |>
    select(rank, everything())
}
