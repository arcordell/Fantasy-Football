library(CVXR)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(writexl)
library(readxl)

trade_data <- read.csv("C:/Users/alexc/Desktop/Fantasy Football/Trade Data/tradedata-11-28-2020.csv")
fantasypros <- read.csv("C:/Users/alexc/Desktop/Fantasy Football/Data/FantasyPros_2020_Dynasty_ALL_Rankings.csv")
player_values <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Player Values 2020-11-28.xlsx") %>%
  mutate(trade_date = as.Date(trade_date))

current_date <- max(player_values$trade_date)
current_values <- player_values %>%
  filter(trade_date == current_date) %>%
  select(player_name, player_value)

date_start <- as.Date("2020-09-01")
players_in_init_optimization <- 150
min_trades_for_val <- 5
average_player_value <- 20
export_data <- "Yes"

#https://www.rdocumentation.org/packages/glmc/versions/0.3-1/topics/glmc
#convex optimization https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.lsq_linear.html

remove_brackets <- function (x) {
  x <- gsub("[","",x, fixed = TRUE)
  x <- gsub("]","",x, fixed = TRUE)
  x <- gsub('\"', "", x, fixed = TRUE)
}

remove_suffixes <- function (x) {
  x <- gsub(" Fuller V", " Fuller", x)
  x <- gsub(" IV", "", x)
  x <- gsub(" III", "", x)
  x <- gsub(" II", "", x)
  x <- gsub(" Jr.", "", x)
  x <- gsub("\\.", "", x)
}

player_details <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Data/Player Details.xlsx") %>%
  filter(Position %in% c("QB", "RB", "WR", "TE")) %>%
  select(Player, Position) %>%
  mutate(Player = remove_suffixes(Player))

#Add fields to the data so that it is easier to use
trade_values_1 <- trade_data %>%
  filter(is_dynasty == "true") %>%
  rename(#num_teams = league_settings.num_teams,
  #       num_qbs = league_settings.num_qbs,
          ppr_type = ppr) %>%
  filter(num_qbs %in% c(1),
         num_teams %in% c(12),
         ppr_type %in% c(1)) %>%
  mutate(trade_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
         side1_no_brackets = remove_brackets(side1),
         side2_no_brackets = remove_brackets(side2),
         trade_id = row_number(),
         num_qbs = factor(num_qbs)) %>%
  arrange(trade_date) %>%
  separate(side1_no_brackets, c("side1_player1", "side1_player2", "side1_player3", "side1_player4"), sep = ",", remove = FALSE) %>%
  separate(side2_no_brackets, c("side2_player1", "side2_player2", "side2_player3", "side2_player4"), sep = ",", remove = FALSE) %>%
  #Filter to trades when there are 3 or fewer players on each side
  filter(is.na(side1_player4), is.na(side2_player4), #is.na(side1_player2) | is.na(side2_player2),
         side1 != '["]', side2 != '["]', str_length(side1) >= 4, str_length(side2) >= 4) %>%
  select(-c(side1_player4, side2_player4)) %>%
  filter(side1_player1 != side2_player1 | is.na(side1_player1) | is.na(side2_player1),
         side1_player1 != side2_player2 | is.na(side1_player1) | is.na(side2_player2),
         side1_player1 != side2_player3 | is.na(side1_player1) | is.na(side2_player3),
         side1_player2 != side2_player1 | is.na(side1_player2) | is.na(side2_player1),
         side1_player2 != side2_player2 | is.na(side1_player2) | is.na(side2_player2),
         side1_player2 != side2_player3 | is.na(side1_player2) | is.na(side2_player3),
         side1_player3 != side2_player1 | is.na(side1_player3) | is.na(side2_player1),
         side1_player3 != side2_player2 | is.na(side1_player3) | is.na(side2_player2),
         side1_player3 != side2_player3 | is.na(side1_player3) | is.na(side2_player3)) %>%
  select(-c(side1_player1:side1_player3, side2_player1:side2_player3), c(side1_player1:side1_player3, side2_player1:side2_player3)) %>%
  filter(trade_date >= date_start) %>%
  mutate(side1_player1 = remove_suffixes(side1_player1),
         side1_player2 = remove_suffixes(side1_player2),
         side1_player3 = remove_suffixes(side1_player3),
         side2_player1 = remove_suffixes(side2_player1),
         side2_player2 = remove_suffixes(side2_player2),
         side2_player3 = remove_suffixes(side2_player3)) %>%
  left_join(player_values, by = c("side1_player1" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values, by = c("side1_player2" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values, by = c("side1_player3" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values, by = c("side2_player1" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values, by = c("side2_player2" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values, by = c("side2_player3" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(is.na(side1_player1) | !is.na(s1_p1_value),
         is.na(side1_player2) | !is.na(s1_p2_value),
         is.na(side1_player3) | !is.na(s1_p3_value),
         is.na(side2_player1) | !is.na(s2_p1_value),
         is.na(side2_player2) | !is.na(s2_p2_value),
         is.na(side2_player3) | !is.na(s2_p3_value)) %>%
  left_join(current_values, by = c("side1_player1" = "player_name")) %>%
  rename(s1_p1_current_value = player_value) %>%
  left_join(current_values, by = c("side1_player2" = "player_name")) %>%
  rename(s1_p2_current_value = player_value) %>%
  left_join(current_values, by = c("side1_player3" = "player_name")) %>%
  rename(s1_p3_current_value = player_value) %>%
  left_join(current_values, by = c("side2_player1" = "player_name")) %>%
  rename(s2_p1_current_value = player_value) %>%
  left_join(current_values, by = c("side2_player2" = "player_name")) %>%
  rename(s2_p2_current_value = player_value) %>%
  left_join(current_values, by = c("side2_player3" = "player_name")) %>%
  rename(s2_p3_current_value = player_value) %>%
  filter(is.na(side1_player1) | !is.na(s1_p1_current_value),
         is.na(side1_player2) | !is.na(s1_p2_current_value),
         is.na(side1_player3) | !is.na(s1_p3_current_value),
         is.na(side2_player1) | !is.na(s2_p1_current_value),
         is.na(side2_player2) | !is.na(s2_p2_current_value),
         is.na(side2_player3) | !is.na(s2_p3_current_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_current_value), ~replace(., is.na(.), 0)) %>%
  mutate(s1_value = s1_p1_value + s1_p2_value + s1_p3_value,
         s2_value = s2_p1_value + s2_p2_value + s2_p3_value,
         s1_current_value = s1_p1_current_value + s1_p2_current_value + s1_p3_current_value,
         s2_current_value = s2_p1_current_value + s2_p2_current_value + s2_p3_current_value,
         value_difference = s2_value - s1_value,
         current_value_difference = s2_current_value - s1_current_value) %>%
  select(trade_date, side1_no_brackets, side2_no_brackets, value_difference, current_value_difference)

trade_values_2 <- trade_values_1 %>%
  mutate(side1 = side2_no_brackets,
         side2 = side1_no_brackets,
         value_difference = -value_difference,
         current_value_difference = -current_value_difference) %>%
  select(trade_date, side1, side2, value_difference, current_value_difference)

trade_values_1 <- trade_values_1 %>%
  rename(side1 = side1_no_brackets, side2 = side2_no_brackets)

trades_all <- bind_rows(trade_values_1, trade_values_2) %>%
  filter(value_difference < 0) %>%
  arrange(desc(current_value_difference))