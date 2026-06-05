library(dplyr)
library(tidyr)
library(plotly)
library(reactable)

prepare_match_view <- function(fixtures, results) {
  fixtures |>
    left_join(results, by = "match_id") |>
    mutate(
      match_date = as.Date(date),
      match = paste0(home_team_de, " vs ", away_team_de),
      score = case_when(
        status == "FINISHED" ~ paste0(home_goals, "–", away_goals),
        TRUE ~ ""
      )
    )
}

prepare_prediction_view <- function(predictions, fixtures, results = NULL) {
  out <- predictions |>
    left_join(fixtures, by = "match_id") |>
    mutate(
      match_date = as.Date(date),
      match = paste0(home_team_de, " vs ", away_team_de),
      prediction = paste0(pred_home_goals, "–", pred_away_goals)
    )

  if (!is.null(results)) {
    out <- out |>
      left_join(results, by = "match_id") |>
      mutate(
        actual_score = case_when(
          status == "FINISHED" ~ paste0(home_goals, "–", away_goals),
          TRUE ~ ""
        )
      )
  }

  out
}

current_day_guesses <- function(predictions, fixtures, results, day = Sys.Date()) {
  prepare_prediction_view(predictions, fixtures, results) |>
    filter(match_date == day) |>
    arrange(match_date, match_id, player) |>
    select(
      group, match_id, match, player, prediction,
      status, actual_score, submitted_at
    )
}

previous_guesses <- function(predictions, fixtures, results, scores, day = Sys.Date()) {
  prepare_prediction_view(predictions, fixtures, results) |>
    left_join(
      scores |>
        select(player, match_id, points, reason),
      by = c("player", "match_id")
    ) |>
    filter(status == "FINISHED" | match_date < day) |>
    arrange(desc(match_date), match_id, player) |>
    select(
      match_date, group, match_id, match, player,
      prediction, actual_score, points, reason, submitted_at
    )
}

build_points_race_data <- function(scores, fixtures) {
  scored_matches <- scores |>
    left_join(fixtures, by = "match_id") |>
    mutate(match_date = as.Date(date)) |>
    filter(!is.na(match_date))

  all_scope <- scored_matches |>
    arrange(match_date, match_id) |>
    group_by(player) |>
    mutate(running_points = cumsum(points)) |>
    ungroup() |>
    mutate(scope = "All groups")

  group_scope <- scored_matches |>
    arrange(group, match_date, match_id) |>
    group_by(player, group) |>
    mutate(running_points = cumsum(points)) |>
    ungroup() |>
    mutate(scope = paste("Group", group))

  bind_rows(all_scope, group_scope) |>
    mutate(
      hover = paste0(
        "<b>", player, "</b>",
        "<br>Match: ", match_id,
        "<br>Group: ", group,
        "<br>Points this match: ", points,
        "<br>Total in view: ", running_points,
        "<extra></extra>"
      )
    )
}

points_race_plot <- function(scores, fixtures) {
  race <- build_points_race_data(scores, fixtures)

  if (nrow(race) == 0) {
    return(htmltools::tags$p("No scored matches yet — the points race will appear once results are available."))
  }

  scopes <- c("All groups", paste("Group", sort(unique(race$group))))

  plot_ly(
    data = race,
    x = ~match_date,
    y = ~running_points,
    color = ~player,
    split = ~player,
    type = "scatter",
    mode = "lines+markers",
    line = list(shape = "hv", width = 3),
    marker = list(size = 8),
    hoverinfo = "text",
    text = ~hover,
    transforms = list(
      list(
        type = "filter",
        target = ~scope,
        operation = "=",
        value = "All groups"
      )
    )
  ) |>
    layout(
      title = list(text = "🏆 Points race over time"),
      xaxis = list(title = "Match date"),
      yaxis = list(title = "Total points"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
      hovermode = "closest",
      updatemenus = list(
        list(
          type = "dropdown",
          x = 0,
          y = 1.15,
          xanchor = "left",
          yanchor = "top",
          buttons = lapply(scopes, function(s) {
            list(
              method = "restyle",
              args = list("transforms[0].value", s),
              label = s
            )
          })
        )
      ),
      margin = list(t = 90, b = 90)
    )
}
