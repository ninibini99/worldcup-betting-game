library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ── Correct answers ──────────────────────────────────────────────
CORRECT_WC_WINNER       <- "ESP"
CORRECT_BONUS1_SUI      <- 5      # item 5 = correct answer for Switzerland qualifying
CORRECT_BONUS2_GOALS    <- 12     # Switzerland goals excl. penalties
CORRECT_BONUS3_NIL      <- 7      # games ending 0-0 or going to pens from 0-0

# ── Load all Pavlovia CSVs ────────────────────────────────────────
csv_files <- list.files("data/raw/pavlovia", pattern = "\\.csv$", full.names = TRUE)

raw <- purrr::map_dfr(csv_files, function(f) {
  readr::read_csv(f, show_col_types = FALSE) |>
    dplyr::mutate(source_file = basename(f))
})

if ("isComplete" %in% names(raw)) {
  raw <- raw |> dplyr::filter(isComplete == TRUE)
}

official_players <- c("Nina", "Saoirse", "Stefan", "Luki", "Charlotte", "Susanne")

clean_player_name <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::case_when(
    stringr::str_to_lower(x) == "nina"      ~ "Nina",
    stringr::str_to_lower(x) == "saoirse"   ~ "Saoirse",
    stringr::str_to_lower(x) == "stefan"    ~ "Stefan",
    stringr::str_to_lower(x) == "luki"      ~ "Luki",
    stringr::str_to_lower(x) == "charlotte" ~ "Charlotte",
    stringr::str_to_lower(x) == "susanne"   ~ "Susanne",
    TRUE ~ x
  )
}

raw <- raw |>
  dplyr::mutate(
    player       = clean_player_name(`block_1/Name`),
    submitted_at = as.POSIXct(responseDate)
  ) |>
  dplyr::filter(player %in% official_players)

# Keep latest submission per player per file type
raw <- raw |>
  dplyr::arrange(dplyr::desc(submitted_at)) |>
  dplyr::distinct(player, source_file, .keep_all = TRUE)

# ── Extract bonus columns (where they exist) ─────────────────────
extract_col <- function(df, col) {
  if (col %in% names(df)) df[[col]] else NA
}

bonus <- raw |>
  dplyr::mutate(
    wc_winner = extract_col(raw, "block_1/WC_Winner"),
    bonus1    = suppressWarnings(as.integer(extract_col(raw, "block_1/Bonus1_Switzerland_Qualify"))),
    bonus2    = suppressWarnings(as.integer(extract_col(raw, "block_1/Bonus2_Goal_CH"))),
    bonus3    = suppressWarnings(as.integer(extract_col(raw, "block_1/Bonus3_Nill_Games")))
  ) |>
  dplyr::select(player, submitted_at, wc_winner, bonus1, bonus2, bonus3)

# If a player has multiple rows (from different CSVs), keep the one with the answer
bonus <- bonus |>
  dplyr::group_by(player) |>
  dplyr::summarise(
    wc_winner = dplyr::first(wc_winner[!is.na(wc_winner)]),
    bonus1    = dplyr::first(bonus1[!is.na(bonus1)]),
    bonus2    = dplyr::first(bonus2[!is.na(bonus2)]),
    bonus3    = dplyr::first(bonus3[!is.na(bonus3)]),
    .groups   = "drop"
  )

# ── Score exact-match questions (10 pts if correct) ──────────────
bonus <- bonus |>
  dplyr::mutate(
    pts_wc_winner = dplyr::if_else(!is.na(wc_winner) & wc_winner == CORRECT_WC_WINNER, 10L, 0L),
    pts_bonus1    = dplyr::if_else(!is.na(bonus1)    & bonus1    == CORRECT_BONUS1_SUI,  10L, 0L)
  )

# ── Score closest-guess questions (10/8/5 pts) ───────────────────
closest_points <- function(answers, correct) {
  diff   <- abs(answers - correct)
  ranked <- rank(diff, ties.method = "min")
  pts    <- dplyr::case_when(
    is.na(answers) ~ 0L,
    ranked == 1    ~ 10L,
    ranked == 2    ~ 8L,
    ranked == 3    ~ 5L,
    TRUE           ~ 0L
  )
  pts
}

bonus <- bonus |>
  dplyr::mutate(
    pts_bonus2 = closest_points(bonus2, CORRECT_BONUS2_GOALS),
    pts_bonus3 = closest_points(bonus3, CORRECT_BONUS3_NIL),
    bonus_total = pts_wc_winner + pts_bonus1 + pts_bonus2 + pts_bonus3
  )

# ── Output ────────────────────────────────────────────────────────
result <- bonus |>
  dplyr::select(
    player,
    wc_winner, pts_wc_winner,
    bonus1,    pts_bonus1,
    bonus2,    pts_bonus2,
    bonus3,    pts_bonus3,
    bonus_total
  ) |>
  dplyr::arrange(dplyr::desc(bonus_total))

readr::write_csv(result, "data/bonus_scores.csv")
message("Bonus scores written to data/bonus_scores.csv")
print(result)
