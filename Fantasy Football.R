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

date_start <- as.Date("2020-07-01")
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

#Organize the data by player
player_data <- trade_data_2 %>%
  pivot_longer(side1_player1:side2_player3, names_to = "player_type", values_to = "player_name") %>%
  filter(!is.na(player_name), str_length(player_name) >= 1) %>%
  mutate(value = case_when(grepl("side1", player_type) ~ 1, TRUE ~ -1)) %>%
  group_by(trade_id, trade_date, player_name) %>%
  summarize(value = sum(value))

#Find the number of times each player was traded
player_num_trades <- player_data %>%
  group_by(player_name) %>%
  summarize(num_trades = n()) %>%
  arrange(desc(num_trades))

#Add the number of times each player was traded to the data
player_data_2 <- player_data %>%
  left_join(player_num_trades, by = "player_name")

#Find how many times the player who was traded the least for each trade
trades_min_player <- player_data_2 %>%
  group_by(trade_id) %>%
  summarize(min_player = min(num_trades))

#Get the minimimum number of trades to include in the optimization
min_trades_for_init <- player_num_trades$num_trades[players_in_init_optimization]

#Filter to trades where every player was traded at least the minimum number of times
player_data_3 <- player_data_2 %>%
  left_join(trades_min_player, by = "trade_id") %>%
  filter(min_player >= min_trades_for_init)

#Find the number of times each player was traded
player_num_trades_2 <- player_data_3 %>%
  group_by(player_name) %>%
  summarize(num_trades = n())

#Create independent variables for each player
player_wide <- player_data_3 %>%
  pivot_wider(id_cols = c(trade_id, trade_date), names_from = player_name, values_from = value, values_fill = list(value = 0))

X <- player_wide %>%
  ungroup() %>%
  arrange(trade_id) %>%
  select(-c(trade_id:trade_date)) %>%
  as.matrix()

#Example: https://cran.r-project.org/web/packages/CVXR/vignettes/cvxr_intro.html
betaHat <- Variable(ncol(X))
Y <- rep(0, nrow(X))

#Find the player values
objective <- Minimize(sum((Y - X %*% betaHat)^2))
problem <- Problem(objective, constraints = list(betaHat >= 0, mean(betaHat) == average_player_value))
result <- solve(problem)
player_values <- data.frame(player_name = (names(player_wide[-c(1:2)])), value = round(result$getValue(betaHat), 1))

player_values <- player_values %>%
  mutate(player_name = as.character(player_name))

#Create a matrix with the player values in each trade
value_vector <- player_values$value
point_df <- as.data.frame(sweep(X, MARGIN=2, value_vector, "*"))

#Have every player as player 1 for every trade
all_player_1 <- trade_data_2[rep(seq_len(nrow(trade_data_2)), 6), ] %>%
  mutate(data_num =  ceiling(row_number()/nrow(trade_data_2))) %>%
  select(data_num, everything()) %>%
  mutate(s1_p1_player = case_when(data_num == 1 ~ side1_player1,
                                       data_num == 2 ~ side1_player2,
                                       data_num == 3 ~ side1_player3,
                                       data_num == 4 ~ side2_player1,
                                       data_num == 5 ~ side2_player2,
                                       data_num == 6 ~ side2_player3),
         s1_p2_player = case_when(data_num == 1 ~ side1_player2,
                                       data_num == 2 ~ side1_player3,
                                       data_num == 3 ~ side1_player1,
                                       data_num == 4 ~ side2_player2,
                                       data_num == 5 ~ side2_player3,
                                       data_num == 6 ~ side2_player1),
         s1_p3_player = case_when(data_num == 1 ~ side1_player3,
                                       data_num == 2 ~ side1_player1,
                                       data_num == 3 ~ side1_player2,
                                       data_num == 4 ~ side2_player3,
                                       data_num == 5 ~ side2_player1,
                                       data_num == 6 ~ side2_player2),
         s2_p1_player = case_when(data_num == 1 ~ side2_player1,
                                       data_num == 2 ~ side2_player2,
                                       data_num == 3 ~ side2_player3,
                                       data_num == 4 ~ side1_player1,
                                       data_num == 5 ~ side1_player2,
                                       data_num == 6 ~ side1_player3),
         s2_p2_player = case_when(data_num == 1 ~ side2_player2,
                                       data_num == 2 ~ side2_player3,
                                       data_num == 3 ~ side2_player1,
                                       data_num == 4 ~ side1_player2,
                                       data_num == 5 ~ side1_player3,
                                       data_num == 6 ~ side1_player1),
         s2_p3_player = case_when(data_num == 1 ~ side2_player3,
                                       data_num == 2 ~ side2_player1,
                                       data_num == 3 ~ side2_player2,
                                       data_num == 4 ~ side1_player3,
                                       data_num == 5 ~ side1_player1,
                                       data_num == 6 ~ side1_player2)) %>%
  filter(!is.na(s1_p1_player)) %>%
  left_join(player_values, by = c("s1_p1_player" = "player_name")) %>%
  rename(s1_p1_player_value = value) %>%
  left_join(player_values, by = c("s1_p2_player" = "player_name")) %>%
  rename(s1_p2_player_value = value) %>%
  left_join(player_values, by = c("s1_p3_player" = "player_name")) %>%
  rename(s1_p3_player_value = value) %>%
  left_join(player_values, by = c("s2_p1_player" = "player_name")) %>%
  rename(s2_p1_player_value = value) %>%
  left_join(player_values, by = c("s2_p2_player" = "player_name")) %>%
  rename(s2_p2_player_value = value) %>%
  left_join(player_values, by = c("s2_p3_player" = "player_name")) %>%
  rename(s2_p3_player_value = value) %>%
  mutate_at(vars(s1_p1_player_value:s2_p3_player_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = s2_p1_player_value + s2_p2_player_value + s2_p3_player_value - s1_p2_player_value - s1_p3_player_value,
         color_val = case_when(
           !is.na(s1_p2_player) ~ "red",
           !is.na(s2_p2_player) ~ "blue",
           TRUE ~ "gray"
         ),
         fill_val = case_when(
          !is.na(s1_p2_player) & !is.na(s1_p3_player) ~ "Traded with 2 other players",
          !is.na(s2_p2_player) & !is.na(s2_p3_player) ~ "Traded for 3 players",
          !is.na(s1_p2_player) | !is.na(s1_p3_player) ~ "Traded with 1 other player",
          !is.na(s2_p2_player) | !is.na(s2_p3_player) ~ "Traded for 2 players",
          TRUE ~ "Traded for 1 player"
         )) %>%
  left_join(player_details, by = c("s1_p1_player" = "Player")) %>%
  mutate(s1_p1_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s1_p1_player) ~ "Pick")) %>%
  select(-Position) %>%
  left_join(player_details, by = c("s1_p2_player" = "Player")) %>%
  mutate(s1_p2_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s1_p2_player) ~ "Pick")) %>%
  select(-Position) %>%
  left_join(player_details, by = c("s1_p3_player" = "Player")) %>%
  mutate(s1_p3_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s1_p3_player) ~ "Pick")) %>%
  select(-Position) %>%
  left_join(player_details, by = c("s2_p1_player" = "Player")) %>%
  mutate(s2_p1_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s2_p1_player) ~ "Pick")) %>%
  select(-Position) %>%
  left_join(player_details, by = c("s2_p2_player" = "Player")) %>%
  mutate(s2_p2_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s2_p2_player) ~ "Pick")) %>%
  select(-Position) %>%
  left_join(player_details, by = c("s2_p3_player" = "Player")) %>%
  mutate(s2_p3_position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", s2_p3_player) ~ "Pick")) %>%
  select(-Position) %>%
  filter(is.na(s1_p1_player) | !is.na(s1_p1_position),
         is.na(s1_p2_player) | !is.na(s1_p2_position),
         is.na(s1_p3_player) | !is.na(s1_p3_position),
         is.na(s2_p1_player) | !is.na(s2_p1_position),
         is.na(s2_p2_player) | !is.na(s2_p2_position),
         is.na(s2_p3_player) | !is.na(s2_p3_position)) %>%
  select(-c(data_num:league_settings),
         #-c(league_settings.keepers:league_settings._id),
         -c(side1_no_brackets:side2_no_brackets), -c(side1_player1:side2_player3)) %>%
  mutate(qb_trade_na = ifelse((s1_p1_position == "QB" | s1_p2_position == "QB" | s1_p3_position == "QB" |
           s2_p1_position == "QB" | s2_p2_position == "QB" | s2_p3_position == "QB"), "Yes", "No"),
         qb_trade = ifelse(is.na(qb_trade_na), "No", "Yes")) %>%
  filter(!(num_qbs == 2 & qb_trade == "Yes"))

time_constant <- 10

date_end <- max(all_player_1$trade_date)
date_list <- data.frame(date_val = seq(date_start, date_end, by="days"))

#Create the function to get the weighted average of the player values
find_player_values <- function(date_1, trade_df) {

players_by_date_1 <- trade_df %>%
  group_by(trade_date, player_name = s1_p1_player) %>%
  summarize(average_value = mean(implied_value), trade_count = n()) %>%
  mutate(days_since_trade = as.numeric(difftime(date_1, trade_date, units = c("days")))) %>%
  filter(days_since_trade >= 0)

trade_count <- players_by_date_1 %>%
  group_by(player_name) %>%
  summarize(player_trade_count = sum(trade_count)) %>%
  filter(player_trade_count >= min_trades_for_val)

players_by_date_2 <- players_by_date_1 %>%
  inner_join(trade_count, by = "player_name")

player_values <- players_by_date_2 %>%
  mutate(trade_weight = trade_count * exp(-days_since_trade/time_constant), weighted_value = average_value * trade_weight) %>%
  group_by(player_name) %>%
  summarize(player_value = sum(weighted_value) / sum(trade_weight))

date_vector <- rep(date_1, nrow(player_values))
player_values_1_date <- data.frame(trade_date = as.Date(date_vector), player_values)

if (nrow(player_values) == 1) {
  return(NULL)
} else {
return(player_values_1_date)
}
}

#Create the function to replace the player values before the player has been traded enough times
replace_mins <- function(player_values_input) {
player_min_dates <- player_values_input %>%
  group_by(player_name) %>%
  summarize(min_date = min(trade_date))

player_value_on_min_date <- player_values_input %>%
  left_join(player_min_dates, by = "player_name") %>%
  filter(trade_date == min_date) %>%
  rename(min_date_player_value = player_value) %>%
  select(-trade_date)

players_and_dates <- player_values_input %>%
  distinct(player_name) %>%
  merge(date_list, by = NULL) %>%
  rename(trade_date = date_val)

all_player_values <- players_and_dates %>%
  left_join(player_values_input, by = c("player_name", "trade_date")) %>%
  left_join(player_value_on_min_date, by = "player_name") %>%
  mutate(final_player_value = ifelse(trade_date < min_date, min_date_player_value, player_value)) %>%
  select(player_name, trade_date, final_player_value) %>%
  rename(player_value = final_player_value)

return(all_player_values)
}

#Model run 1: run with only trades where there was only one player on the left side
all_player_no_trade_up <- all_player_1 %>%
      filter(is.na(s1_p2_player) | !is.na(s1_p2_player_value),
             is.na(s1_p3_player) | !is.na(s1_p3_player_value),
             is.na(s2_p1_player) | !is.na(s2_p1_player_value),
             is.na(s2_p2_player) | !is.na(s2_p2_player_value),
             is.na(s2_p3_player) | !is.na(s2_p3_player_value)) %>%
  filter(is.na(s1_p2_player), is.na(s1_p3_player))

player_values_by_date_1 <- apply(date_list, 1, find_player_values, trade_df = all_player_no_trade_up)
player_values_df_1_no_min <- do.call(rbind.data.frame, player_values_by_date_1)
player_values_df_1 <- replace_mins(player_values_df_1_no_min)

#Model run 2: Run with all trades
all_player_2 <- all_player_1 %>%
  left_join(player_values_df_1, by = c("s1_p1_player" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values_df_1, by = c("s1_p2_player" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values_df_1, by = c("s1_p3_player" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values_df_1, by = c("s2_p1_player" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values_df_1, by = c("s2_p2_player" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values_df_1, by = c("s2_p3_player" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(!is.na(s1_p1_value)) %>%
  filter(is.na(s1_p2_player) | !is.na(s1_p2_value),
         is.na(s1_p3_player) | !is.na(s1_p3_value),
         is.na(s2_p1_player) | !is.na(s2_p1_value),
         is.na(s2_p2_player) | !is.na(s2_p2_value),
         is.na(s2_p3_player) | !is.na(s2_p3_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = s1_p1_value / (s1_p1_value + s1_p2_value + s1_p3_value) * (s2_p1_value + s2_p2_value + s2_p3_value))

player_values_by_date_2 <- apply(date_list, 1, find_player_values, trade_df = all_player_2)
player_values_df_2_no_min <- do.call(rbind.data.frame, player_values_by_date_2)
player_values_df_2 <- replace_mins(player_values_df_2_no_min)

#Run the model 10 more times to reach an equilibrium
for (i in 1:10) {

all_player_2 <- all_player_2 %>%
  select(-implied_value, -c(s1_p1_value:s2_p3_value)) %>%
  left_join(player_values_df_2, by = c("s1_p1_player" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("s1_p2_player" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("s1_p3_player" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p1_player" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p2_player" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p3_player" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(!is.na(s1_p1_value)) %>%
    filter(is.na(s1_p2_player) | !is.na(s1_p2_value),
           is.na(s1_p3_player) | !is.na(s1_p3_value),
           is.na(s2_p1_player) | !is.na(s2_p1_value),
           is.na(s2_p2_player) | !is.na(s2_p2_value),
           is.na(s2_p3_player) | !is.na(s2_p3_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = s1_p1_value / (s1_p1_value + s1_p2_value + s1_p3_value) * (s2_p1_value + s2_p2_value + s2_p3_value))

player_values_by_date_2 <- apply(date_list, 1, find_player_values, trade_df = all_player_2)
player_values_df_2_no_min <- do.call(rbind.data.frame, player_values_by_date_2)
player_values_df_2 <- replace_mins(player_values_df_2_no_min)
}

s1_players <- all_player_2 %>%
select(s1_p1_player:s1_p3_player) %>%
apply(1, function(x) toString(na.omit(x)))

s2_players <- all_player_2 %>%
select(s2_p1_player:s2_p3_player) %>%
apply(1, function(x) toString(na.omit(x)))

all_player_2 <- bind_cols(all_player_2, s1_players = s1_players, s2_players = s2_players)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

player_to_graph <- "Patrick Mahomes"

player_line <- player_values_df_2 %>%
  filter(player_name == player_to_graph)

last_player_value_tbl <- player_line %>%
  filter(trade_date == date_end)

last_player_value <- last_player_value_tbl$player_value

output_trades <- all_player_2 %>%
  mutate(fill_val = factor(fill_val, levels = c("Traded for 3 players", "Traded for 2 players", "Traded for 1 player",
                                                "Traded with 1 other player", "Traded with 2 other players")), player_value = implied_value) %>%
  select(player_name = s1_p1_player, trade_date, player_value, color_val, fill_val, s1_players, s2_players)  %>%
  filter(!(grepl("2019", player_name) | grepl("2020", player_name)))

output_values <- player_values_df_2 %>%
  filter(!(grepl("2019", player_name) | grepl("2020", player_name)))

date_end_minus_14 <- date_end - 14

end_values <- output_values %>%
  filter(trade_date == date_end)

end_minus_14_values <- output_values %>%
  filter(trade_date == date_end_minus_14) %>%
  rename(player_value_minus_14 = player_value) %>%
  select(-trade_date)

output_last_values <- end_values %>%
  left_join(end_minus_14_values, by = "player_name") %>%
  mutate(change_last_14 = player_value - player_value_minus_14) %>%
  select(-player_value_minus_14) %>%
  arrange(desc(player_value)) %>%
  mutate(ranking = 1:n())

one_player <- output_trades %>%
  filter(player_name == player_to_graph)

g <- ggplot(data = one_player, aes(x = trade_date, y = player_value)) +
  geom_point(aes(color = color_val, fill = fill_val), size = 4, shape = 21, alpha = 0.8) +
  geom_line(data = player_line, lwd = 2, alpha = 0.4) +
  theme_fivethirtyeight() + 
  scale_color_manual(values = c("blue" = "blue", "gray" = "darkgray", "red" = "red"), guide = FALSE) +
  scale_fill_manual(values = c("Traded for 3 players" = "darkblue", "Traded with 2 other players" = "red3", "Traded for 2 players" = "lightblue",
                               "Traded for 1 player" = "gray", "Traded with 1 other player" = "indianred1")) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 16),
        title = element_text(size = 26),
        axis.text = element_text(size = 16),
        axis.title = element_text(),
        axis.title.x = element_text(size = 24, vjust = -1),
        axis.title.y = element_text(size = 24, vjust = 2),
        panel.background = element_rect(fill = "white")) +
  labs(title = paste(player_to_graph, "'", ifelse(right(player_to_graph, 1) != "s","s",""), " value over time in the 2020 season", sep = ""),
       x = "Trade date",
       y = "Value") +
  geom_vline(xintercept = date_end, color = "black", linetype = 3, lwd = 1) +
  
  xlim(date_start, date_end + 3)

g_breaks_adj <- (ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]) / 28.7

g + annotate("label",x = date_end, y = last_player_value + 4 * g_breaks_adj,label = paste(sub(" 0", " ", format(date_end, "%B %d")), " Value", sep = ""),
           vjust = 1, size = 8, color = "black", fill = "white", fontface="bold", label.size = 0) +
  geom_label(data = last_player_value_tbl, aes(x = date_end, y = last_player_value + 2.5 * g_breaks_adj,label = paste(round(last_player_value, 1), sep = "")),
           vjust = 1, size = 8, color = "black", fill = "lightsteelblue1", label.padding = unit(0.4, "cm"))

final_player_values <- player_values_df_2 %>%
  filter(trade_date == date_end)

rm(X, objective, problem, result, player_values_by_date_1, player_values_by_date_2)

#Find the adjustments to apply to different league settings
rel_data <- all_player_2 %>%
  select(num_teams, num_qbs, ppr_type, s1_p1_value, s1_p1_position, implied_value) %>%
  rename(player_value = s1_p1_value, position = s1_p1_position) %>%
  mutate(trade_miss = implied_value / player_value - 1)

qb_model <- lm(trade_miss ~ num_teams + ppr_type, data = filter(rel_data, position == "QB"))
rb_model <- lm(trade_miss ~ num_teams + ppr_type, data = filter(rel_data, position == "RB"))
wr_model <- lm(trade_miss ~ num_teams + ppr_type, data = filter(rel_data, position == "WR"))
te_model <- lm(trade_miss ~ num_teams + ppr_type, data = filter(rel_data, position == "TE"))
pick_model <- lm(trade_miss ~ num_teams + ppr_type, data = filter(rel_data, position == "Pick"))

d_num_teams <- distinct(select(all_player_2, num_teams))
d_ppr_type <- distinct(select(all_player_2, ppr_type))

league_settings <- merge(d_num_teams, d_ppr_type)

all_settings <- data.frame(league_settings,
                                   QB_adj = 1 + predict(qb_model, league_settings),
                                   RB_adj = 1 + predict(rb_model, league_settings),
                                   WR_adj = 1 + predict(wr_model, league_settings),
                                   TE_adj = 1 + predict(te_model, league_settings),
                                   Pick_adj = 1 + predict(pick_model, league_settings))

most_common_settings <- all_settings %>%
  filter(num_teams == 12, ppr_type == 1)

all_settings_data <- all_settings %>%
  select(-c(1:2))

most_common_settings_data <- most_common_settings %>%
  select(-c(1:2))

normalized_adjustments <- data.frame(mapply('/', all_settings_data, most_common_settings_data))

settings_adjustments_wide <- bind_cols(league_settings, normalized_adjustments) %>%
  arrange(desc(QB_adj))

settings_adjustments <- settings_adjustments_wide %>%
  pivot_longer(QB_adj:Pick_adj, names_to = "position", values_to = "adjustment") %>%
  mutate(position = gsub("_adj", "", position))

adj_player_1 <- all_player_2 %>%
  left_join(settings_adjustments, by = c("s1_p1_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s1_p1_adj = adjustment) %>%
  left_join(settings_adjustments, by = c("s1_p2_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s1_p2_adj = adjustment) %>%
  left_join(settings_adjustments, by = c("s1_p3_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s1_p3_adj = adjustment) %>%
  left_join(settings_adjustments, by = c("s2_p1_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s2_p1_adj = adjustment) %>%
  left_join(settings_adjustments, by = c("s2_p2_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s2_p2_adj = adjustment) %>%
  left_join(settings_adjustments, by = c("s2_p3_position" = "position", "num_teams", "ppr_type")) %>%
  rename(s2_p3_adj = adjustment) %>%
  mutate(s1_p1_value_adj = s1_p1_value * s1_p1_adj,
         s1_p2_value_adj = s1_p2_value * s1_p2_adj,
         s1_p3_value_adj = s1_p3_value * s1_p3_adj,
         s2_p1_value_adj = s2_p1_value * s2_p1_adj,
         s2_p2_value_adj = s2_p2_value * s2_p2_adj,
         s2_p3_value_adj = s2_p3_value * s2_p3_adj) %>%
         mutate_at(vars(s1_p1_value_adj:s2_p3_value_adj), ~replace(., is.na(.), 0)) %>%
         mutate(implied_value = (s1_p1_value_adj / (s1_p1_value_adj + s1_p2_value_adj + s1_p3_value_adj) *
                          (s2_p1_value_adj + s2_p2_value_adj + s2_p3_value_adj)) / s1_p1_adj)


#Run the model on the adjusted trades
adj_player_values_by_date <- apply(date_list, 1, find_player_values, trade_df = adj_player_1)
adj_player_values_df_no_min <- do.call(rbind.data.frame, adj_player_values_by_date)
adj_player_values_df <- replace_mins(adj_player_values_df_no_min)

#Run the model 10 more times to reach an equilibrium
for (i in 1:10) {

adj_player_2 <- adj_player_1 %>%
  select(-implied_value, -c(s1_p1_value:s2_p3_value)) %>%
  left_join(player_values_df_2, by = c("s1_p1_player" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("s1_p2_player" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("s1_p3_player" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p1_player" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p2_player" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("s2_p3_player" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(!is.na(s1_p1_value)) %>%
    filter(is.na(s1_p2_player) | !is.na(s1_p2_value),
           is.na(s1_p3_player) | !is.na(s1_p3_value),
           is.na(s2_p1_player) | !is.na(s2_p1_value),
           is.na(s2_p2_player) | !is.na(s2_p2_value),
           is.na(s2_p3_player) | !is.na(s2_p3_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_value), ~replace(., is.na(.), 0)) %>%
   mutate(s1_p1_value_adj = s1_p1_value * s1_p1_adj,
         s1_p2_value_adj = s1_p2_value * s1_p2_adj,
         s1_p3_value_adj = s1_p3_value * s1_p3_adj,
         s2_p1_value_adj = s2_p1_value * s2_p1_adj,
         s2_p2_value_adj = s2_p2_value * s2_p2_adj,
         s2_p3_value_adj = s2_p3_value * s2_p3_adj) %>%
         mutate_at(vars(s1_p1_value_adj:s2_p3_value_adj), ~replace(., is.na(.), 0)) %>%
         mutate(implied_value = (s1_p1_value_adj / (s1_p1_value_adj + s1_p2_value_adj + s1_p3_value_adj) *
                          (s2_p1_value_adj + s2_p2_value_adj + s2_p3_value_adj)) / s1_p1_adj)

adj_player_values_by_date <- apply(date_list, 1, find_player_values, trade_df = adj_player_2)
adj_player_values_df_no_min <- do.call(rbind.data.frame, adj_player_values_by_date)
adj_player_values_df <- replace_mins(adj_player_values_df_no_min)

}

#Create the output for the adjusted data
player_to_graph <- "Dwayne Haskins"

player_line <- adj_player_values_df %>%
  filter(player_name == player_to_graph)

last_player_value_tbl <- player_line %>%
  filter(trade_date == date_end)

last_player_value <- last_player_value_tbl$player_value

output_values_1 <- adj_player_values_df %>%
  filter(!(grepl("2019", player_name) | grepl("2020", player_name)))

end_values_1 <- output_values_1 %>%
  filter(trade_date == date_end)

average_player <- mean(end_values_1$player_value)
player_adjustment <- average_player_value / average_player

output_trades <- adj_player_2 %>%
  mutate(fill_val = factor(fill_val, levels = c("Traded for 3 players", "Traded for 2 players", "Traded for 1 player",
                                                "Traded with 1 other player", "Traded with 2 other players")), player_value = implied_value * player_adjustment) %>%
  select(player_name = s1_p1_player, trade_date, player_value, color_val, fill_val, s1_players, s2_players)  %>%
  filter(!(grepl("2019", player_name) | grepl("2020", player_name)))

output_values <- adj_player_values_df %>%
  filter(!(grepl("2019", player_name) | grepl("2020", player_name))) %>%
  mutate(player_value = player_value * player_adjustment)

end_values <- output_values %>%
  filter(trade_date == date_end)

date_end_minus_14 <- date_end - 14

end_minus_14_values <- output_values %>%
  filter(trade_date == date_end_minus_14) %>%
  rename(player_value_minus_14 = player_value) %>%
  select(-trade_date)

output_last_values <- end_values %>%
  left_join(end_minus_14_values, by = "player_name") %>%
  mutate(change_last_14 = player_value - player_value_minus_14) %>%
  select(-player_value_minus_14) %>%
  arrange(desc(player_value)) %>%
  left_join(player_details, by = c(player_name = "Player")) %>%
  mutate(position = case_when(!is.na(Position) ~ Position,
                                 grepl("Round", player_name) ~ "Pick"),
  ranking = 1:n())

one_player <- output_trades %>%
  filter(player_name == player_to_graph)

g <- ggplot(data = one_player, aes(x = trade_date, y = player_value)) +
  geom_point(aes(color = color_val, fill = fill_val), size = 4, shape = 21, alpha = 0.8) +
  geom_line(data = player_line, lwd = 2, alpha = 0.4) +
  theme_fivethirtyeight() + 
  scale_color_manual(values = c("blue" = "blue", "gray" = "darkgray", "red" = "red"), guide = FALSE) +
  scale_fill_manual(values = c("Traded for 3 players" = "darkblue", "Traded with 2 other players" = "red3", "Traded for 2 players" = "lightblue",
                               "Traded for 1 player" = "gray", "Traded with 1 other player" = "indianred1")) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 16),
        title = element_text(size = 26),
        axis.text = element_text(size = 16),
        axis.title = element_text(),
        axis.title.x = element_text(size = 24, vjust = -1),
        axis.title.y = element_text(size = 24, vjust = 2),
        panel.background = element_rect(fill = "white")) +
  labs(title = paste(player_to_graph, "'", ifelse(right(player_to_graph, 1) != "s","s",""), " value over time in the 2020 season", sep = ""),
       x = "Trade date",
       y = "Value") +
  geom_vline(xintercept = date_end, color = "black", linetype = 3, lwd = 1) +
  
  xlim(date_start, date_end + 3)

g_breaks_adj <- (ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]) / 28.7

g + annotate("label",x = date_end, y = last_player_value + 4 * g_breaks_adj,label = paste(sub(" 0", " ", format(date_end, "%B %d")), " Value", sep = ""),
           vjust = 1, size = 8, color = "black", fill = "white", fontface="bold", label.size = 0) +
  geom_label(data = last_player_value_tbl, aes(x = date_end, y = last_player_value + 2.5 * g_breaks_adj,label = paste(round(last_player_value, 1), sep = "")),
           vjust = 1, size = 8, color = "black", fill = "lightsteelblue1", label.padding = unit(0.4, "cm"))

#Compare the rankings to FantasyPros
fantasypros_2 <- fantasypros %>%
  #mutate(player_name = remove_suffixes(str_sub((str_match(PLAYER.NAME, ".+?(?<=\\()")), end = -3))) %>%
  mutate(player_name = remove_suffixes(PLAYER.NAME)) %>%
  inner_join(output_last_values, by = "player_name") %>%
  select(player_name) %>%
  mutate(fp_ranking = 1:n())

rankings_combined <- output_last_values %>%
  left_join(fantasypros_2, by = "player_name") %>%
  mutate(trade_ranking = ranking)

values_by_rank <- rankings_combined %>%
  filter(!is.na(fp_ranking)) %>%
  mutate(ranking = 1:n()) %>%
  select(ranking, fp_value = player_value)

rankings_comparison <- rankings_combined %>%
  left_join(values_by_rank, by = c("fp_ranking" = "ranking")) %>%
  select(player_name, position, trade_value = player_value, change_last_14, fp_value, trade_ranking, fp_ranking) %>%
  mutate(value_score = ifelse(is.na(fp_value), 0, trade_value - fp_value),
         fp_tier = case_when(fp_value >= 40 ~ "Tier 1",
                             fp_value >= 30 ~ "Tier 2",
                             fp_value >= 20 ~ "Tier 3",
                             TRUE ~ "Tier 4")) %>%
  group_by(position) %>%
  mutate(position_rank = rank(fp_ranking))

buy_low_players <- rankings_comparison %>%
  arrange(value_score) %>%
  group_by(position, fp_tier) %>%
  slice(1:3) %>%
  filter(value_score <= 0) %>%
  mutate(player_rank = rank(value_score),
         player_type = "Buy Low Players")

sell_high_players <- rankings_comparison %>%
  arrange(desc(value_score)) %>%
  group_by(position, fp_tier) %>%
  slice(1:3) %>%
  filter(value_score > 0) %>%
  mutate(player_rank = rank(desc(value_score)),
         player_type = "Sell High Players")

buy_low_sell_high <- bind_rows(buy_low_players, sell_high_players)

ggplot(rankings_comparison, aes(x = fp_value, y = trade_value)) + geom_point() + geom_abline()

current_date <- as.Date(now())

if (export_data == "Yes") {
  write_xlsx(output_trades, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Trade Data ", current_date, ".xlsx", sep = ""))
  write_xlsx(output_values, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Player Values ", current_date, ".xlsx", sep = ""))
  write_xlsx(settings_adjustments, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Settings Adjustments ", current_date, ".xlsx", sep = ""))
  write_xlsx(rankings_comparison, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Rankings Comparison ", current_date, ".xlsx", sep = ""))
  write_xlsx(buy_low_sell_high, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Buy Low Sell High ", current_date, ".xlsx", sep = ""))
}

#Fix this next time!!!!
all_rankings <- merge(output_last_values, settings_adjustments_wide) %>%
  mutate(adjustment = case_when(position == "QB" ~ QB_adj,
                                position == "RB" ~ RB_adj,
                                position == "WR" ~ WR_adj,
                                position == "TE" ~ TE_adj,
                                position == "Pick" ~ Pick_adj),
         new_player_value = player_value * adjustment,
         new_change_last_14 = change_last_14 * adjustment)

all_rankings_all <- all_rankings %>%
  select(player_name, trade_date, position, num_teams:ppr_type, player_value = new_player_value, change_last_14 = new_change_last_14)
#Add ranks