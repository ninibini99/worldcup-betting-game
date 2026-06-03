library(readr)
library(dplyr)
library(tidyr)
library(stringr)

load_fixtures <- function(path = "data/fixtures.csv") {
  readr::read_csv(path, show_col_types = FALSE)
}

load_results <- function(path = "data/results.csv") {
  readr::read_csv(path, show_col_types = FALSE)
}

load_predictions <- function(path = "data/predictions.csv") {
  readr::read_csv(path, show_col_types = FALSE)
}
