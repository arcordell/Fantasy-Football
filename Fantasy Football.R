#library(xlsx)
library(CVXR)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(lubridate)

trade_data <- read.csv("C:/Users/alexc/Desktop/Fantasy Football/tradedata.csv")
#https://www.rdocumentation.org/packages/glmc/versions/0.3-1/topics/glmc
#convex optimization https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.lsq_linear.html

remove_brackets <- function (x) {
  x <- gsub("[","",x, fixed = TRUE)
  x <- gsub("]","",x, fixed = TRUE)
}

#Add fields to the data so that it is easier to use
trade_data_2 <- trade_data %>%
  mutate(trade_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
         side1_no_brackets = remove_brackets(side1),
         side2_no_brackets = remove_brackets(side2),
         trade_id = row_number()) %>%
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
         side1_player3 != side2_player3 | is.na(side1_player3) | is.na(side2_player3),
         grepl("Pick", side1_player1) == FALSE, grepl("Pick", side1_player2) == FALSE, grepl("Pick", side1_player3) == FALSE,
         grepl("Pick", side2_player1) == FALSE, grepl("Pick", side2_player2) == FALSE, grepl("Pick", side2_player3) == FALSE,
         grepl("Round", side1_player1) == FALSE, grepl("Round", side1_player2) == FALSE, grepl("Round", side1_player3) == FALSE,
         grepl("Round", side2_player1) == FALSE, grepl("Round", side2_player2) == FALSE, grepl("Round", side2_player3) == FALSE) %>%
  select(-c(side1_player1:side1_player3, side2_player1:side2_player3), c(side1_player1:side1_player3, side2_player1:side2_player3)) %>%
  filter(trade_date >= as.Date("2019-9-1"))

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
  summarize(num_trades = n())

#Add the number of times each player was traded to the data
player_data_2 <- player_data %>%
  left_join(player_num_trades, by = "player_name")

#Find how many times the player who was traded the least for each trade
trades_min_player <- player_data_2 %>%
  group_by(trade_id) %>%
  summarize(min_player = min(num_trades))

#Filter to trades where every player was traded at least 10 times
player_data_3 <- player_data_2 %>%
  left_join(trades_min_player, by = "trade_id") %>%
  filter(min_player >= 50)

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
problem <- Problem(objective, constraints = list(betaHat >= 0, mean(betaHat) == 20))
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
  mutate(new_side1_player1 = case_when(data_num == 1 ~ side1_player1,
                                       data_num == 2 ~ side1_player2,
                                       data_num == 3 ~ side1_player3,
                                       data_num == 4 ~ side2_player1,
                                       data_num == 5 ~ side2_player2,
                                       data_num == 6 ~ side2_player3),
         new_side1_player2 = case_when(data_num == 1 ~ side1_player2,
                                       data_num == 2 ~ side1_player3,
                                       data_num == 3 ~ side1_player1,
                                       data_num == 4 ~ side2_player2,
                                       data_num == 5 ~ side2_player3,
                                       data_num == 6 ~ side2_player1),
         new_side1_player3 = case_when(data_num == 1 ~ side1_player3,
                                       data_num == 2 ~ side1_player1,
                                       data_num == 3 ~ side1_player2,
                                       data_num == 4 ~ side2_player3,
                                       data_num == 5 ~ side2_player1,
                                       data_num == 6 ~ side2_player2),
         new_side2_player1 = case_when(data_num == 1 ~ side2_player1,
                                       data_num == 2 ~ side2_player2,
                                       data_num == 3 ~ side2_player3,
                                       data_num == 4 ~ side1_player1,
                                       data_num == 5 ~ side1_player2,
                                       data_num == 6 ~ side1_player3),
         new_side2_player2 = case_when(data_num == 1 ~ side2_player2,
                                       data_num == 2 ~ side2_player3,
                                       data_num == 3 ~ side2_player1,
                                       data_num == 4 ~ side1_player2,
                                       data_num == 5 ~ side1_player3,
                                       data_num == 6 ~ side1_player1),
         new_side2_player3 = case_when(data_num == 1 ~ side2_player3,
                                       data_num == 2 ~ side2_player1,
                                       data_num == 3 ~ side2_player2,
                                       data_num == 4 ~ side1_player3,
                                       data_num == 5 ~ side1_player1,
                                       data_num == 6 ~ side1_player2)) %>%
  filter(!is.na(new_side1_player1)) %>%
  left_join(player_values, by = c("new_side1_player1" = "player_name")) %>%
  rename(new_side1_player1_value = value) %>%
  left_join(player_values, by = c("new_side1_player2" = "player_name")) %>%
  rename(new_side1_player2_value = value) %>%
  left_join(player_values, by = c("new_side1_player3" = "player_name")) %>%
  rename(new_side1_player3_value = value) %>%
  left_join(player_values, by = c("new_side2_player1" = "player_name")) %>%
  rename(new_side2_player1_value = value) %>%
  left_join(player_values, by = c("new_side2_player2" = "player_name")) %>%
  rename(new_side2_player2_value = value) %>%
  left_join(player_values, by = c("new_side2_player3" = "player_name")) %>%
  rename(new_side2_player3_value = value) %>%
  filter(is.na(new_side1_player2) | !is.na(new_side1_player2_value),
         is.na(new_side1_player3) | !is.na(new_side1_player3_value),
         is.na(new_side2_player1) | !is.na(new_side2_player1_value),
         is.na(new_side2_player2) | !is.na(new_side2_player2_value),
         is.na(new_side2_player3) | !is.na(new_side2_player3_value)) %>%
  mutate_at(vars(new_side1_player1_value:new_side2_player3_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = new_side2_player1_value + new_side2_player2_value + new_side2_player3_value - new_side1_player2_value - new_side1_player3_value,
         color_val = case_when(
           !is.na(new_side1_player2) ~ "red",
           !is.na(new_side2_player2) ~ "blue",
           TRUE ~ "gray"
         ),
         fill_val = case_when(
           !is.na(new_side1_player3) ~ "Traded with 2 other players for 1 player",
           !is.na(new_side2_player3) ~ "Traded for 3 players",
           !is.na(new_side1_player2) ~ "Traded with 1 other player for 1 player",
           !is.na(new_side2_player2) ~ "Traded for 2 players",
           TRUE ~ "Traded for 1 player"
         ))

time_constant <- 10

date_start <- as.Date("2019-09-01")
date_end <- as.Date("2019-11-07")
date_list <- data.frame(date_val = seq(date_start, date_end, by="days"))

#Create the function to get the weighted average of the player values
find_player_values <- function(date_1, trade_df) {

players_by_date_1 <- trade_df %>%
  group_by(trade_date, player_name = new_side1_player1) %>%
  summarize(average_value = mean(implied_value), trade_count = n()) %>%
  mutate(days_since_trade = as.numeric(difftime(date_1, trade_date, units = c("days")))) %>%
  filter(days_since_trade >= 0)

trade_count <- players_by_date_1 %>%
  group_by(player_name) %>%
  summarize(player_trade_count = sum(trade_count)) %>%
  filter(player_trade_count >= 10)

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
  filter(is.na(new_side1_player2))

player_values_by_date_1 <- apply(date_list, 1, find_player_values, trade_df = all_player_no_trade_up)
player_values_df_1_no_min <- do.call(rbind.data.frame, player_values_by_date_1)
player_values_df_1 <- replace_mins(player_values_df_1_no_min)

#Model run 2: Run with all trades
all_player_2 <- all_player_1 %>%
  left_join(player_values_df_1, by = c("new_side1_player1" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values_df_1, by = c("new_side1_player2" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values_df_1, by = c("new_side1_player3" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values_df_1, by = c("new_side2_player1" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values_df_1, by = c("new_side2_player2" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values_df_1, by = c("new_side2_player3" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(!is.na(s1_p1_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = s1_p1_value / (s1_p1_value + s1_p2_value + s1_p3_value) * (s2_p1_value + s2_p2_value + s2_p3_value))

player_values_by_date_2 <- apply(date_list, 1, find_player_values, trade_df = all_player_2)
player_values_df_2_no_min <- do.call(rbind.data.frame, player_values_by_date_2)
player_values_df_2 <- replace_mins(player_values_df_2_no_min)

#Run the model 10 more times to reach an equilibrium
for (i in 1:10) {

all_player_2 <- all_player_2 %>%
  select(-implied_value, -c(s1_p1_value:s2_p3_value)) %>%
  left_join(player_values_df_2, by = c("new_side1_player1" = "player_name", "trade_date")) %>%
  rename(s1_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("new_side1_player2" = "player_name", "trade_date")) %>%
  rename(s1_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("new_side1_player3" = "player_name", "trade_date")) %>%
  rename(s1_p3_value = player_value) %>%
  left_join(player_values_df_2, by = c("new_side2_player1" = "player_name", "trade_date")) %>%
  rename(s2_p1_value = player_value) %>%
  left_join(player_values_df_2, by = c("new_side2_player2" = "player_name", "trade_date")) %>%
  rename(s2_p2_value = player_value) %>%
  left_join(player_values_df_2, by = c("new_side2_player3" = "player_name", "trade_date")) %>%
  rename(s2_p3_value = player_value) %>%
  filter(!is.na(s1_p1_value)) %>%
  mutate_at(vars(s1_p1_value:s2_p3_value), ~replace(., is.na(.), 0)) %>%
  mutate(implied_value = s1_p1_value / (s1_p1_value + s1_p2_value + s1_p3_value) * (s2_p1_value + s2_p2_value + s2_p3_value))

player_values_by_date_2 <- apply(date_list, 1, find_player_values, trade_df = all_player_2)
player_values_df_2_no_min <- do.call(rbind.data.frame, player_values_by_date_2)
player_values_df_2 <- replace_mins(player_values_df_2_no_min)
}

#Add vertical bar with current value
#Fit graph to larger window

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

player_to_graph <- "Stefon Diggs"

player_line <- player_values_df_2 %>%
  filter(player_name == player_to_graph)

last_player_value_tbl <- player_line %>%
  filter(trade_date == date_end)

last_player_value <- last_player_value_tbl$player_value

one_player <- all_player_2 %>%
  filter(new_side1_player1 == player_to_graph) %>%
  mutate(fill_val = factor(fill_val, levels = c("Traded for 3 players", "Traded for 2 players", "Traded for 1 player",
                                                "Traded with 1 other player for 1 player", "Traded with 2 other players for 1 player")), player_value = implied_value) %>%
  select(player_name = new_side1_player1, trade_date, player_value, color_val, fill_val)

g <- ggplot(data = one_player, aes(x = trade_date, y = player_value)) +
  geom_point(aes(color = color_val, fill = fill_val), size = 4, shape = 21, alpha = 0.8) +
  geom_line(data = player_line, lwd = 2, alpha = 0.4) +
  theme_fivethirtyeight() + 
  scale_color_manual(values = c("blue" = "blue", "gray" = "darkgray", "red" = "red"), guide = FALSE) +
  scale_fill_manual(values = c("Traded for 3 players" = "darkblue", "Traded with 2 other players for 1 player" = "red3", "Traded for 2 players" = "lightblue",
                               "Traded for 1 player" = "gray", "Traded with 1 other player for 1 player" = "indianred1")) +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        legend.text = element_text(size = 16),
        title = element_text(size = 26),
        axis.text = element_text(size = 16),
        axis.title = element_text(),
        axis.title.x = element_text(size = 24, vjust = -1),
        axis.title.y = element_text(size = 24, vjust = 2),
        panel.background = element_rect(fill = "white")) +
  labs(title = paste(player_to_graph, "'", ifelse(right(player_to_graph, 1) != "s","s",""), " value over time in the 2019 season", sep = ""),
       x = "Trade date",
       y = "Value") +
  geom_vline(xintercept = date_end, color = "black", linetype = 3, lwd = 1) +
  
  xlim(date_start, date_end + 3)

g_breaks_adj <- (ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]) / 28.7

g + annotate("label",x = date_end, y = last_player_value + 4 * g_breaks_adj,label = paste(sub(" 0", " ", format(date_end, "%B %d")), " Value", sep = ""),
           vjust = 1, size = 8, color = "black", fill = "white", fontface="bold", label.size = 0) +
  geom_label(data = last_player_value_tbl, aes(x = date_end, y = last_player_value + 2.5 * g_breaks_adj,label = paste(round(last_player_value, 1), sep = "")),
           vjust = 1, size = 8, color = "black", fill = "lightsteelblue1", label.padding = unit(0.4, "cm"))

#Add trade id's as labels, for the back-end
#Josh could use the trade id's to bring up the trade below the graph, if a circle is hovered over
#Average 2 for 1 markup
#Send Josh the raw data

current_date <- as.Date(now())
write.xlsx(one_player, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Output ", current_date, ".xlsx", sep = ""), sheetName = "Trade Data", append = TRUE)
write.xlsx(player_line, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Output ", current_date, ".xlsx", sep = ""), sheetName = "Value Data", append = TRUE)
write.xlsx(last_player_value_tbl, paste("C:/Users/alexc/Desktop/Fantasy Football/Output Data/Output ", current_date, ".xlsx", sep = ""), sheetName = "Last Value", append = TRUE)