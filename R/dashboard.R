library(dplyr)
library(tidyr)
library(plotly)
library(reactable)
library(stringr)

official_players <- readr::read_csv(
  "data/players.csv",
  show_col_types = FALSE
)$player

clean_player_name <- function(x) {
  x <- stringr::str_squish(x)
  
  dplyr::case_when(
    stringr::str_to_lower(x) == "nina" ~ "Nina",
    stringr::str_to_lower(x) == "saoirse" ~ "Saoirse",
    stringr::str_to_lower(x) == "stefan" ~ "Stefan",
    stringr::str_to_lower(x) == "luki" ~ "Luki",
    stringr::str_to_lower(x) == "charlotte" ~ "Charlotte",
    TRUE ~ x
  )
}

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
      match = paste0(home_team, " vs ", away_team),
      prediction = paste0(pred_home_goals, "–", pred_away_goals)
    )
  
  if (!is.null(results)) {
    out <- out |>
      left_join(results, by = "match_id")
  }
  
  out
}

current_day_guesses <- function(predictions, fixtures, results = NULL) {
  today <- Sys.Date()

  out <- prepare_prediction_view(predictions, fixtures, results) |>
    dplyr::filter(match_date == today)

  if (nrow(out) == 0) {
    return(list())
  }

  if (all(c("home_goals", "away_goals") %in% names(out))) {
    out <- out |>
      dplyr::mutate(
        result = dplyr::if_else(
          is.na(home_goals) | is.na(away_goals),
          "–",
          paste0(home_goals, "–", away_goals)
        )
      )
  } else {
    out <- out |> dplyr::mutate(result = "–")
  }

  if (!"status" %in% names(out)) {
    out <- out |> dplyr::mutate(status = "scheduled")
  }

  # Return a list of per-match data frames
  matches <- unique(out$match_id)
  lapply(matches, function(mid) {
    m <- out |> dplyr::filter(match_id == mid)
    list(
      match_id = mid,
      group    = m$group[1],
      match    = m$match[1],
      result   = m$result[1],
      status   = m$status[1],
      guesses  = m |> dplyr::select(player, prediction) |> dplyr::arrange(player)
    )
  })
}

previous_guesses <- function(predictions, fixtures, results = NULL, scores = NULL) {
  today <- Sys.Date()
  
  empty_previous <- tibble::tibble(
    player = character(),
    group = character(),
    match = character(),
    prediction = character(),
    result = character(),
    points = numeric(),
    reason = character()
  )
  
  out <- prepare_prediction_view(predictions, fixtures, results)
  
  # If there is no status column yet, create a blank one
  if (!"status" %in% names(out)) {
    out <- out |>
      dplyr::mutate(status = "")
  }
  
  # Add result display if result columns exist
  if (all(c("home_goals", "away_goals") %in% names(out))) {
    out <- out |>
      dplyr::mutate(
        result = dplyr::if_else(
          is.na(home_goals) | is.na(away_goals),
          "",
          paste0(home_goals, "–", away_goals)
        )
      )
  } else {
    out <- out |>
      dplyr::mutate(result = "")
  }
  
  # Attach scores if available
  if (!is.null(scores) && all(c("player", "match_id", "points", "reason") %in% names(scores))) {
    out <- out |>
      dplyr::left_join(
        scores |> dplyr::select(player, match_id, points, reason),
        by = c("player", "match_id")
      )
  } else {
    out <- out |>
      dplyr::mutate(
        points = NA_real_,
        reason = ""
      )
  }
  
  out <- out |>
    dplyr::filter(status == "FINISHED" | match_date < today)
  
  if (nrow(out) == 0) {
    return(empty_previous)
  }
  
  out |>
    dplyr::arrange(dplyr::desc(match_date), match, player) |>
    dplyr::select(
      player,
      group,
      match,
      prediction,
      result,
      points,
      reason
    )
}

build_points_race_data <- function(scores, fixtures) {
  scored_matches <- scores |>
    left_join(fixtures, by = "match_id") |>
    mutate(match_date = as.Date(date)) |>
    filter(!is.na(match_date))

  # Sum points per player per date before cumulating
  all_scope <- scored_matches |>
    group_by(player, match_date) |>
    summarise(daily_points = sum(points, na.rm = TRUE), .groups = "drop") |>
    arrange(player, match_date) |>
    group_by(player) |>
    mutate(
      running_points = cumsum(daily_points),
      scope = "All groups"
    ) |>
    ungroup()

  group_scope <- scored_matches |>
    group_by(player, group, match_date) |>
    summarise(daily_points = sum(points, na.rm = TRUE), .groups = "drop") |>
    arrange(player, group, match_date) |>
    group_by(player, group) |>
    mutate(
      running_points = cumsum(daily_points),
      scope = paste("Group", group)
    ) |>
    ungroup()

  bind_rows(all_scope, group_scope) |>
    mutate(
      hover = paste0(
        "<b>", player, "</b>",
        "<br>Date: ", match_date,
        "<br>Points today: ", daily_points,
        "<br>Total: ", running_points,
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

get_today_guesses <- function(predictions, fixtures, results) {
  today <- Sys.Date()
  current_time <- lubridate::now()
  reveal_time <- lubridate::as_datetime(paste(today, "08:00:00"))
  
  if (current_time < reveal_time) {
    return(tibble::tibble(
      message = "Today's guesses will be revealed at 08:00."
    ))
  }
  
  predictions |>
    dplyr::mutate(player = clean_player_name(player)) |>
    dplyr::filter(player %in% official_players) |>
    dplyr::left_join(fixtures, by = "match_id") |>
    dplyr::left_join(results, by = "match_id") |>
    dplyr::filter(as.Date(date) == today) |>
    dplyr::select(
      player,
      group,
      home_team,
      away_team,
      pred_home_goals,
      pred_away_goals,
      status,
      home_goals,
      away_goals
    )
}
