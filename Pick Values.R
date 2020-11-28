library(MASS)
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(forecast)

picks_to_plot <- 60

trade_data_2019 <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Output Data/2019 Draft Pick Trade Data.xlsx")

trade_grouped_2019 <- trade_data_2019 %>%
  filter(grepl("Pick", player_name)) %>%
  group_by(player_name) %>%
  summarize(player_value = mean(player_value)) %>%
  mutate(pick_round = as.numeric(substr(player_name, 12, 12)),
         pick_number_in_round = as.numeric(substr(player_name, 19, nchar(player_name))),
         pick_number = (pick_round - 1) * 12 + pick_number_in_round) %>%
  arrange(pick_number) %>%
  filter(pick_number <= picks_to_plot) %>%
  select(pick_number, player_value)
  
trade_data_2020 <- read_excel("C:/Users/alexc/Desktop/Fantasy Football/Output Data/2020 Draft Pick Trade Data.xlsx")

trade_grouped_2020 <- trade_data_2020 %>%
  filter(grepl("Pick", player_name)) %>%
  group_by(player_name) %>%
  summarize(player_value = mean(player_value)) %>%
  mutate(pick_round = as.numeric(substr(player_name, 12, 12)),
         pick_number_in_round = as.numeric(substr(player_name, 19, nchar(player_name))),
         pick_number = (pick_round - 1) * 12 + pick_number_in_round) %>%
  arrange(pick_number) %>%
  filter(pick_number <= picks_to_plot) %>%
  select(pick_number, player_value)

trades_combined <- bind_rows(trade_grouped_2019, trade_grouped_2020) %>%
  group_by(pick_number) %>%
  summarize(player_value = mean(player_value))

x <- trades_combined$pick_number
y <- trades_combined$player_value

glm_model <- glm(y ~ x, family = Gamma())

predictions <- data.frame(x = c(1:picks_to_plot))

glm_predictions <- predict(glm_model, newdata = predictions, type = "response")

all_picks <- data.frame(pick_number = c(1:picks_to_plot))

trades_with_pred <- bind_cols(all_picks, glm_pred = glm_predictions) %>%
  left_join(trades_combined, by = "pick_number") %>%
  mutate(pick_round = ceiling(pick_number / 12),
         pick_in_round = pick_number - ((pick_round - 1) * 12))

ggplot(data = trades_with_pred, aes(x = pick_number, y = player_value)) + geom_point() +
  geom_line(aes(y = glm_pred), col = "blue")

write_xlsx(trades_with_pred, "C:/Users/alexc/Desktop/Fantasy Football/Output Data/Draft Pick Values.xlsx")

# run the box-cox transformation
bc <- boxcox(y ~ x)

(best_lambda <- bc$x[which.max(bc$y)])
best_lambda <- BoxCox.lambda(y)
bc_model <- lm(((y^best_lambda - 1) / best_lambda) ~ x)
bc_pred <- predict(bc_model, newdata = predictions, type = "response")
bc_pred2 <- InvBoxCox(bc_pred, best_lambda)
