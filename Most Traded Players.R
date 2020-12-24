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

trade_data <- read.csv("C:/Users/alexc/Desktop/Fantasy Football/Trade Data/tradedata-12-9-2020.csv")
fantasypros <- read.csv("C:/Users/alexc/Desktop/Fantasy Football/Data/FantasyPros_2020_Dynasty_ALL_Rankings.csv")
trade_values <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Player Values 2020-12-09.xlsx")

current_values <- trade_values %>%
  filter(trade_date == as.Date("2020-12-09"))

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
trade_data_2 <- trade_data %>%
  filter(is_dynasty == "true") %>%
  rename(#num_teams = league_settings.num_teams,
  #       num_qbs = league_settings.num_qbs,
          ppr_type = ppr) %>%
  filter(num_qbs %in% c(1, 2),
         num_teams %in% c(10, 12, 14, 16),
         ppr_type %in% c(0, 0.5, 1)) %>%
  mutate(trade_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
         side1_no_brackets = remove_brackets(side1),
         side2_no_brackets = remove_brackets(side2),
         trade_id = row_number(),
         num_qbs = factor(num_qbs)) %>%
  arrange(trade_date) %>%
  separate(side1_no_brackets, c("side1_player1", "side1_player2", "side1_player3", "side1_player4"), sep = ",", remove = FALSE) %>%
  separate(side2_no_brackets, c("side2_player1", "side2_player2", "side2_player3", "side2_player4"), sep = ",", remove = FALSE) %>%
  #Filter to trades when there are 3 or fewer players on each side
  filter(is.na(side1_player4), is.na(side2_player4), is.na(side1_player2) | is.na(side2_player2), side1 != '["]', side2 != '["]', str_length(side1) >= 4, str_length(side2) >= 4) %>%
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
         side2_player3 = remove_suffixes(side2_player3))

count_players <- function(player_var) {
  trade_table <- trade_data_2 %>%
  mutate(player_name = player_var) %>%
  group_by(player_name) %>%
  summarize(trade_count = n())
  return(trade_table)
}

s1_p1_count <- count_players(trade_data_2$side1_player1)
s1_p2_count <- count_players(trade_data_2$side1_player2)
s1_p3_count <- count_players(trade_data_2$side1_player3)
s2_p1_count <- count_players(trade_data_2$side2_player1)
s2_p2_count <- count_players(trade_data_2$side2_player2)
s2_p3_count <- count_players(trade_data_2$side2_player3)

trade_counts <- bind_rows(s1_p1_count, s1_p2_count, s1_p3_count, s2_p1_count, s2_p2_count, s2_p3_count)

trade_count_by_player <- trade_counts %>%
  group_by(player_name) %>%
  summarize(trade_count = sum(trade_count)) %>%
  arrange(desc(trade_count)) %>%
  filter(!grepl("Round", player_name), !is.na(player_name)) %>%
  left_join(player_details, by = c("player_name" = "Player")) %>%
  inner_join(current_values, by = "player_name") %>%
  select(-trade_date)

current_date <- as.Date(now())
write_xlsx(trade_count_by_player, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Most Traded Players ", current_date, ".xlsx", sep = ""))
